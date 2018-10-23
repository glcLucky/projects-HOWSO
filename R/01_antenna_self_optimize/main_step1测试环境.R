################
##查找到状态####
################
library(dplyr)
library(RPostgreSQL) 
library(sqldf)
library(SoDA)
library(data.table)
library(RUnit)
library(DEoptim)
library(parallel)
rm(list = ls())

#############################
setwd("E:\\projects\\R\\01_antenna_self_optimize")  ## 本地
# setwd("/etl/algorithm/workspace")  ## 福建工作路径
# data_dir<-"results_ECI"
# file_OTT<-"data/20171207_fuzhou"# 暂时从数据库读取OTT数据

source("helpers/data_process_db/evolution_step2.R",encoding = "UTF-8")#数据库、OTT计算、关联小区计算(联调小区计算)
# , encoding = "UTF-8")
#包含selected_cell和OTT计算
source("helpers/data_process_db/near_cell.R",encoding = "UTF-8") #计算邻区ci
source("helpers/optimise_helpers/initialization.R"); initialization() # for encoding issues #识别中文系统
source("helpers/optimise_helpers/telecom_func.R", encoding = "UTF-8") #通信里面的概念
source("helpers/optimise_helpers/evolution_func.R", encoding = "UTF-8") #评估概念 cost
rm(initialization, reinitialization)
#
 

####################
##连接数据库########
####################
drv = dbDriver("PostgreSQL") 
## 本地数据库
pgdb = dbConnect(drv, user="feeder_dev", password="feeder_dev",dbname ="feeder" ,port="5432" , host="192.168.101.66")

## 福建数据库
# pgdb = dbConnect(drv, user="deeplan", password="deeplan",dbname ="deeplan" ,port="5432" , host="10.48.147.66")
#解决数据中文乱码的问题
postgresqlpqExec(pgdb, "SET client_encoding = 'utf-8'")
#遍历数据库中数据表格
# dbListTables(pgdb)   

########################
#####读取数据库中的表格#
########################
# 读取数据库中的表
project<-dbReadTable(pgdb, "project")   # there are 3 rows
#查找相关工程编号下面的ECI
project_details<-dbReadTable(pgdb, "project_details")
#工参表,用readTable比getquery 慢0.04秒
# begin.time<-Sys.time()
sm_cl<-dbReadTable(pgdb,"sm_cl_location")

# sm_cl_ch<-na.omit(sm_cl,cols=c("horizontal_beam_feat","vertical_beam_feat"))

# end.time<-Sys.time()
# print(end.time-begin.time)

# 读取前10行OTT数据作为样例
plan_OTT<-dbGetQuery(pgdb,"SELECT * FROM plan_ott_data limit 10")

######################################
#############读取空值数据设置#########
######################################
## NA strings in all inputed data frames
na_strings = c("NA", "", "NULL","\\N")


## 插入事件的表，后面更新值，插入都数据
event_1 = data.frame(eId = c(NaN),
                     eType = c(NaN),
                     eName = c(NaN),
                     sTime = c(NaN),
                     eTime = c(NaN),
                     detail = c(NaN),
                     user =c(NaN))
event_1$eType = 'feederProjectOptimize'
event_1$eName = '天馈工程优化状态跟踪'
event_1$user = '算法'
#由于可能有多个工程要起，所以OTT读取放在后面

##############################################
#########对状态为1的数据进行计算##############
##############################################
#目标：输出弱覆盖重叠覆盖等信息，改状态1为2，并更改时间

projectid<-dbGetQuery(pgdb,"select next_run_project('1') as project_id");
print(paste0("=========================================This projectid is ",projectid$project_id))
if(!is.null(projectid$project_id) && !is.na(projectid$project_id)){
  file.create(paste0("./active_project/",projectid$project_id,"_",Sys.getpid(),".txt"))
#查找程序中状态为1的工程#并查找到目标网元
tab_status1<-dbGetQuery(pgdb,paste0("SELECT project.project_code,project_details.ci,status, longitude,latitude
                        FROM project, project_details ,sm_cl_location
                        WHERE project.project_code=project_details.project_code 
                        AND project_details.ci=sm_cl_location.ci
                        AND project.status='1'
                        AND project_details.cell_type='1' and project_id=",projectid$project_id,
                        " order by project.project_id"))


#('TK-20171227-0008','TK-20171227-0007','TK-20171227-0006')")

# tab_status1<-dbGetQuery(pgdb,"SELECT project.project_code,project_details.ci,status, longitude,latitude
#                         FROM project, project_details ,sm_cl_location
#                         WHERE project.project_code=project_details.project_code
#                         AND project_details.ci=sm_cl_location.ci
#                         AND project.status='1'
#                         AND project_details.cell_type='1'
#                         AND project.project_code in ('TK-20171227-0009')")
project_code<-unique(tab_status1$project_code)

# if(any(is.na(project_code))) {
#   stop("project code cannot be NA")
# }
## index = 要跑的工单里的工程编号的索引，K在这里index中跑
 
if(!is.null(project_code)){
  for(k in 1:length(project_code)){
    #
    event_1$sTime = Sys.time()
    t1 = Sys.time() 
    #print(t1)
    # 查找到相关的工程编号,并找出问题小区#页面显示是目标网元
    p_cell<-tab_status1[which(tab_status1$project_code %in% project_code[k]),]
    #查找到问题小区的OTT数据
    time_begin<-gsub("-", "", '2018-06-01')#30天前的日期
    time_end<-gsub("-", "", '2018-06-30')#今天的日期
    #读取30天内数据
    data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(p_cell$ci, collapse ="','"),"')"," AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'"))
    #SELECT * FROM feeder.plan_ott_data WHERE s_ci in ('460-00-131297-65')
    #筛选出目标网元的OTT
    #查找程序中状态为1的工程
    # data_OTT<-data_OTT[which(s_ci %in% p_cell$ci)]
    names_OTT_ch<-c("ott_sample_cnt","ott_poor_cover_cnt","ott_cover_ratio","ott_overlap_cnt","ott_overlap_ratio")
    #增加OTT的覆盖情况
    OTT_calculate(data_OTT,names_OTT_ch,cell_type=1)
    
    ## 添加event表中，添加事件内容
    event_1$eId = project_code[k]
    event_1$eTime = Sys.time()
    event_1$detail = paste0('开始优化：问题小区个数（',length(unique(p_cell$ci)),'），问题小区30天采样点个数（',nrow(data_OTT),'），问题小区覆盖情况计算已完成')

    dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
                 row.names=FALSE,add.id=TRUE,overwrite=FALSE)
    
    #更改状态，添加指标关联完成时间
       # dbExecute(pgdb,paste0("UPDATE project SET status='2', relation_end_time = ","'",Sys.Date(),"'","  WHERE project_code=","'",project_code[k],"'"))
    dbExecute(pgdb,paste0("UPDATE project SET status='3', relation_end_time = ","'",Sys.Date(),"'","  WHERE project_code=","'",project_code[k],"'"))   
    print(paste0(k," step1 fini"))
    print((Sys.time()-t1))
  }  
}else{
  print(paste0("project_code is null"))
}


##############################################################
##########对状态为3的工程进行计算#############################
##############################################################
#计算出联调小区(cell_type=2)和关联小区(cell_type=3)，改状态3为4，并更改时间

#查找程序中状态为3的工程
tab_status2<-dbGetQuery(pgdb,paste0("SELECT project.project_code,project_details.ci,status, longitude,latitude ,abs(mod(project.project_id,2))
                        FROM project, project_details ,sm_cl_location
                        WHERE project.project_code=project_details.project_code 
                        AND project_details.ci=sm_cl_location.ci
                        AND project.status='3'
                        AND project_details.cell_type='1' AND project_id=",projectid$project_id,
                        " order by project.project_id"))
project_code2<-unique(tab_status2$project_code)
# abs(mod(hashtext(project.project_code),2))

#tab_status2_ch<-dbGetQuery(pgdb,"SELECT project.project_id,project.project_code,project_details.ci,status, longitude,latitude ,abs(mod(project.project_id,2))
#                        FROM project, project_details ,sm_cl_location
#                        WHERE project.project_code=project_details.project_code 
#                        AND project_details.ci=sm_cl_location.ci
#                        AND project.status='3'
#                        AND project_details.cell_type='1'
#                        order by project.project_id")
#project_id<-unique(tab_status2_ch$project_id[which(tab_status2_ch$abs == 0)])

print(paste0("project_code: ",project_code2[1]))
print(paste0("project_id: ",project_id[1]))
####################
###插入联调小区#####
####################
#length(project_code2)

if((!(is.null(project_code2)||is.na(project_code2)))& (length(project_code2) >0)){
  for(k in 1:1){
    event_1$sTime = Sys.time()
    t1 = Sys.time()
    # 查找到相关的工程编号
    p_cell<-tab_status2[which(tab_status2$project_code %in% project_code2[k]),]
    print(project_code2[k])
    #查询问题小区
    cell_2<-dbGetQuery(pgdb,paste0("SELECT * FROM project_details WHERE cell_type='1' AND project_code = ","'",project_code2[k],"'" ))
    #查询联调小区
    cell_2_ch<-dbGetQuery(pgdb,paste0("SELECT * FROM project_details WHERE cell_type='2' AND project_code = ","'",project_code2[k],"'" ))
    
    # cell_2_ch<-dbGetQuery(pgdb,paste0("SELECT * FROM project_details WHERE cell_type='2' AND project_code = ","'","TK-20171229-0011","'" ))
    
    time_begin<-gsub("-", "", "2018-06-01")#30天数据
    time_end<-gsub("-", "", "2018-06-30")
    
    data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(cell_2$ci, collapse ="','"),"')"," AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'"))
    
    # 判断
    print("==============guess===========")
   # print(data_OTT);
    if(nrow(data_OTT) <= 100){   ## 需要给定一个阀值，如果OTT数据小于多少认为不会有方案，向魏文俊确认一下,自己定义了500
    print("==============guess===========")
      cell_2$cell_type<-"2"
      #除去第一行id，插入其他数据
      dbWriteTable(pgdb, name = "project_details", value = cell_2[,-1],append = TRUE,
                   row.names=FALSE,add.id=TRUE,overwrite=FALSE)
      cell_2$cell_type<-"3"
      #除去第一行id，插入其他数据
      dbWriteTable(pgdb, name = "project_details", value = cell_2[,-1],append = TRUE,
                   row.names=FALSE,add.id=TRUE,overwrite=FALSE)
      #
      result_fl<-matrix(0, nrow = length(cell_2$ci), ncol = 3, byrow = TRUE)
      result_fl<-as.data.frame(result_fl)
      colnames(result_fl)<-c("ci","azimuth_calc_range","down_tilt_calc_range")
      result_fl$ci<-cell_2$ci
      
      for(l in 1:nrow(result_fl)){
        dbExecute(pgdb,paste0("UPDATE project_details SET azimuth_calc_range = ", result_fl$azimuth_calc_range[l],", down_tilt_calc_range = ",
                              result_fl$down_tilt_calc_range[l],", optimize_num = 1",", reason = 'too little OTT data' ",
                              " WHERE project_code=","'",project_code2[k],"'"," AND ci=","'",result_fl$ci[l],"'"," AND cell_type = '2'"))
        # 此处增加一个字段，输出原因，  
	  }
       dbExecute(pgdb,paste0("UPDATE project SET status='6', optimization_time = ","'",Sys.Date(),"'"," WHERE project_code=","'",project_code2[k],"'"))
       ## 该处增加一个optimze_duration为当前时间减去-
       
       ## 添加事件内容
       event_1$eId = project_code2[k]
       event_1$eTime = Sys.time()
       event_1$detail = paste0('优化结束：问题小区30天采样点数小于100，无法输出方案（OTT采样点太少）')
       dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
                    row.names=FALSE,add.id=TRUE,overwrite=FALSE)
       
	}else{
    if(((nrow(cell_2_ch)==0)&(nrow(cell_2)>0))){
      # cell_2$cell_type<-"2"
      n_cell<-nrow(cell_2)
      #加入的重叠覆盖/弱覆盖的相关排序等
      # a<-c("460-00-223924-145","460-00-252354-134","460-00-252354-135","460-00-223924-146")
      #计算联调小区
      sm_cl_filter<-cell_info_filter(max(data_OTT$longitude), min(data_OTT$longitude), max(data_OTT$latitude), min(data_OTT$latitude), sm_cl, 1.5)

      a<-liantiao_cell(data_OTT,cell_2$ci,sm_cl_filter)
      a2<-a[which(!a %in% cell_2$ci)]
      
      l_a2<-length(a2)
      # 判断新添加的ci个数
      
      ## 插入联调小区计算的event，插入event表中
      event_1$eId = project_code2[k]
      event_1$eTime = Sys.time()
      event_1$detail = paste0('计算联调小区：联调小区个数（',length(a),"）")
      dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
                   row.names=FALSE,add.id=TRUE,overwrite=FALSE)
      if(l_a2==0){
        cell_2$cell_type<-"2"
      }else{
        #加上不是问题小区的ci
        cell_2[(n_cell+1):(n_cell+l_a2),]<-NA
        cell_2$project_code<-cell_2$project_code[1]
        cell_2$ci[(n_cell+1):(n_cell+l_a2)]<-a2
        cell_2$cell_type<-"2"
        cell_2[(n_cell+1):(n_cell+l_a2),c("chose_azimuth")] <-sm_cl[which(sm_cl$ci %in% (cell_2$ci[(n_cell+1):(n_cell+l_a2)])),c("azimuth")]
        cell_2[, c("chose_azimuth")] = sapply(cell_2[, c("chose_azimuth")], as.integer)
        }
      #project_details<-dbReadTable(pgdb, "project_details")
      # cell_2$details_id<-(max(project_details$details_id)+1):(max(project_details$details_id)+length(cell_2$ci))
      #dbWriteTable(pgdb, name = "project_details", value = cell_2,append = TRUE,row.names=FALSE,overwrite=FALSE)

      #除去第一行id，插入其他数据
       dbWriteTable(pgdb, name = "project_details", value = cell_2[,-1],append = TRUE,
                   row.names=FALSE,add.id=TRUE,overwrite=FALSE)
      
    }else if(nrow(cell_2)==0){
      print("cell_2 is null")
    }else{
      print("cell_2_ch is not null")
      #依据ci唯一性保存
      cell_2<-cell_2_ch[!duplicated(cell_2_ch[,c('ci')]),]
      
    }
    
    
    #新建数据newtable为
	  event_1$sTime = Sys.time()
    newtable<-NULL
    newtable<-sm_cl_filter[,c("ci","longitude","latitude")]
    #计算评估小区
    select_cell<-selected_cell2(p_cell,newtable)
    
    # ###############################################
    # #再读一遍，后期肯定会影响速度
    # project_details<-dbReadTable(pgdb, "project_details")
    # ##############################
    # #id 到底怎么写还是不写？
    # select_cell$details_id<-(max(project_details$details_id)+1:length(select_cell$ci))
    # select_cell$details_id<-NA
    #写入数据库，去除行名称，带上列名称并覆盖导入
    #首先先把状态改到6，在寻求改数据库状态
    # dbExecute(pgdb,paste0("UPDATE project SET status='6' WHERE project_code=","'",project_code2[k],"'"))
    
    
    if(!is.null(select_cell)){
      #project_details<-dbReadTable(pgdb, "project_details")
      # select_cell$details_id<-(max(project_details$details_id)+1):(max(project_details$details_id)+length(select_cell$ci))
      #dbWriteTable(pgdb, name = "project_details", value = select_cell,append = TRUE,row.names=FALSE,overwrite=FALSE)
      dbWriteTable(pgdb, name = "project_details", value = select_cell[,-1],append = TRUE,
                   row.names=FALSE,add.id=TRUE)
      # dbRemoveTable(pgdb, "project_details2")
      #输出调整方位角和下倾角
      # calculate_angle2(select_cell,cell_2)
      source("helpers/data_process_db/optimise_step2.R",encoding = "UTF-8")
      # 不用改时间
      # dbExecute(pgdb,paste0("UPDATE project SET status='4' WHERE project_code=","'",project_code2[k],"'"))
      print((Sys.time()-t1))
      dbExecute(pgdb,paste0("UPDATE project SET status='6', optimization_time = ","'",Sys.Date(),"'"," WHERE project_code=","'",project_code2[k],"'"))
      ## add the optimize_duration in here
    }else{
      print("select_cell calculate is null")
      #出错把状态改回到3
     # dbExecute(pgdb,paste0("UPDATE project SET status='3' WHERE project_code=","'",project_code2[k],"'"))
      print((Sys.time()-t1))
    }
  }
    
    
    
   
  }
}else{
  print("project_code2 is null")
}

}
 

# end.time<-Sys.time()
# print(end.time-begin.time)
# #完成项目关联部分，关闭连接
# # Close PostgreSQL connection 
dbDisconnect(pgdb)  
###################################################
