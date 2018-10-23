# calculate_angle2<-function(select_cell,cell_2){
  all_select_cell<-unique(select_cell$ci)#选出方案中所有小区
  #查找到问题小区的OTT数据
  
  time_begin<- "20180601" #"gsub("-", "", (Sys.Date()-60))#30天前的日期
  time_end<- "20180630"#gsub("-", "", (Sys.Date()-30))#今天的日期
  
  if(length(all_select_cell)<=300){
    data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ",
                                     "('", paste(all_select_cell, collapse ="','"),"')",
                                     " AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'"))
  }else{
    data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ",
                                     "('", paste(all_select_cell[1:300], collapse ="','"),"')",
                                     " AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'",
                                     ' UNION ALL',
                                     " SELECT * FROM plan_ott_data WHERE s_ci in ",
                                     "('", paste(all_select_cell[301:length(all_select_cell)], collapse ="','"),"')",
                                     " AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'" ))
  }
 
  
  ## 插入计算评估小区event
  event_1$eId = project_code2[k]
  event_1$eTime = Sys.time()
  event_1$detail = paste0('计算评估小区：评估小区个数（',length(all_select_cell),'）,评估小区30天采样点个数（',nrow(data_OTT),"）")
  dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
               row.names=FALSE,add.id=TRUE,overwrite=FALSE)
  
  #########################
  #先设置更新工参表为工参表
  ##########################
  sm_cl_u <- sm_cl_filter
 
  #######################################
  #########给OTT数据关联邻区ECI##########
  #######################################
  
  event_1$sTime = Sys.time()
  #小区配置表、工参所需数据
  # col_cell_use<-c( "ci","cell_name","pci","earfcn","latitude", "longitude")
  col_cell_use<-c( "ci","cell_name","pci","hannel_number","latitude", "longitude")
  col_cell_ch<-c("ECI_cell","cellname_cell","PCI_cell","EARFCN_cell","lat_cell","lon_cell")
  
  
  #邻区
  cell_near_num<-1
  data_OTT_u<-data_OTT
  if((sum(!is.na(data_OTT$n1_earfcn))>0)&(sum(!is.na(data_OTT$n1_ci))==0)){
    data_OTT_u<-ECI_cell_near(cellinfo=sm_cl_u,dataset=data_OTT,col_cell_use,col_cell_ch)    
  }else if(sum(!is.na(data_OTT$n1_earfcn))==0){
          data_OTT_u<-NULL
          print("n1_earfcn is NULL")
  }else{
      print("n1_ci is not vide")
  }
  
  ## 添加邻区CI计算event
  event_1$eId = project_code2[k]
  event_1$eTime = Sys.time()
  event_1$detail = "关联邻区CI：关联成功"
  dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
               row.names=FALSE,add.id=TRUE,overwrite=FALSE)
  
  
  ################################################
  #######运行Alexis代码，计算出最终调整方案#######
  ################################################
  #要四个数据：
  #1、cellinfo勘站后数据 sm_cl_u /
  #2、selected_cell
  #3、prolem_cell
  #4、data_OTT  data_OTT_up
  
  #cellinfo<-sm_cl_u
  #data_OTT<-data_ott_up
  #联调小区
  problem_cell<-unique(cell_2$ci[which(!is.na(cell_2$ci))])
  
  #评估小区
  selected_cell<-unique(select_cell$ci[which(!is.na(select_cell$ci))])
  
  
  #角度范围 down_tilt_range //azimuth_range
  #########################
  #首先判断已经勘站完成
  #########################
  ##首先赋值

  
  #names(cellinfo<-names(cellinfo)
  #Alexis 代码
  if((nrow(data_OTT_u)!=0)&(length(selected_cell)!=0)&(!is.null(problem_cell))&(length(problem_cell)<=length(unique(data_OTT_u$s_ci)))){  ## 此处更改为小于等于也可以跑结果
    #直接带ALexis代码
    source("helpers/optimise_main/1version.R", encoding = "UTF-8")
  }else{
    event_1$sTime = Sys.time()
    result_fl<-matrix(0, nrow = length(problem_cell), ncol = 3, byrow = TRUE)
    result_fl<-as.data.frame(result_fl)
    colnames(result_fl)<-c("ci","azimuth_calc_range","down_tilt_calc_range")
    result_fl$ci<-problem_cell
    print("There are warnings with data")
    for(l in 1:nrow(result_fl)){
      dbExecute(pgdb,paste0("UPDATE project_details SET azimuth_calc_range = ", result_fl$azimuth_calc_range[l],
                            ", down_tilt_calc_range = ",result_fl$down_tilt_calc_range[l],
                            ", optimize_num = 1",", reason = 'data of some ci are missing in OTT' ",
                            " WHERE project_code=","'",project_code2[k],"'"," AND ci=","'",result_fl$ci[l],"'"," AND cell_type = '2'"))
    }
    event_1$eId = project_code2[k]
    event_1$eTime = Sys.time()
    event_1$detail = '优化结束：未进入DE算法，无方案输出（联调小区个数大于评估小区OTT的CI个数）'
    event_1$user = '算法'
    dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
                 row.names=FALSE,add.id=TRUE,overwrite=FALSE)
  }
  event_1$sTime = Sys.time()
  # result_fl$ci[l]
  ################################
  #####计算相应的OTT数据##########
  ################################  
  #添加前7天的OTT评估
  ## 现在都改为30天的数据评估
  time_begin_30<- "20180524" #"gsub("-", "", (Sys.Date()-60))#30天前的日期
  time_end<- "20180623"#gsub("-", "", (Sys.Date()-30))#今天的日期
  
  # time_begin_30 <- gsub("-", "", (Sys.Date()-30))
  # time_end7<-gsub("-", "", (Sys.Date() -23))
  all_eci =  dbGetQuery(pgdb,paste0("SELECT ci from project_details WHERE project_code =","'",project_code2[k],"'"))
  
  #读取7天数据
  # idex_time<-which(data_OTT_u$rpt_time>time_begin7 & data_OTT_u$rpt_time<time_end7)
  # data_OTT_7<-data_OTT[idex_time,]
  
  if(length(unique(all_eci$ci)) <= 300){
    data_OTT_30 <- dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(unique(all_eci$ci), collapse ="','"),"')",
                                          " AND rpt_time > ","'",time_begin_30,"'"," AND rpt_time < ","'",time_end,"'"))
  }else{
    data_OTT_30 <- dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(unique(all_eci$ci)[1:300], collapse ="','"),"')",
                                          " AND rpt_time > ","'",time_begin_30,"'"," AND rpt_time < ","'",time_end,"'",
                                          ' UNION ALL',
                                          " SELECT * FROM plan_ott_data WHERE s_ci in ",
                                          "('", paste(unique(all_eci$ci)[301:length(unique(all_eci))], collapse ="','"),"')",
                                          " AND rpt_time > ","'",time_begin_30,"'"," AND rpt_time < ","'",time_end,"'" ))
  }
  # data_OTT_30 <- dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(unique(all_eci$ci), collapse ="','"),"')",
  #                                       " AND rpt_time > ","'",time_begin_30,"'"," AND rpt_time < ","'",time_end,"'"))
  # 
  # 由于7天OTT数据可能为空，先做一个判断
  if(nrow(data_OTT_30)==0){
    print("data_OTT_30 is vide")
  }else{
    # names_OTT_ch<-c("调整前OTT采样点总数","调整前OTT弱采样点","调整前OTT覆盖率","调整前OTT满足条件的邻区数大等于3的样本点","调整前OTT重叠覆盖度")
    names_OTT<-c("before_ott_sample_cnt","before_ott_poor_cover_cnt","before_ott_cover_ratio","before_ott_overlap_cnt","before_ott_overlap_ratio")
    
    ######################
    ###写入前30天OTT评估####
    ######################
    project_code_ch<-project_code2[k]
    OTT_calculate_new(data_OTT_30,names_OTT,cell_type=2,project_code_ch)
    OTT_calculate_new(data_OTT_30,names_OTT,cell_type=1,project_code_ch)
    OTT_calculate_new(data_OTT_30,names_OTT,cell_type=3,project_code_ch)
    OTT_calculate_new(data_OTT_30,names_OTT,cell_type=4,project_code_ch)
    event_1$eId = project_code2[k]
    event_1$eTime = Sys.time()
    event_1$detail = '计算优化前OTT覆盖情况'
    event_1$user = '算法'
    dbWriteTable(pgdb, name = "event", value = event_1, append = TRUE,
                 row.names=FALSE,add.id=TRUE,overwrite=FALSE)
  }
 
  
# }
