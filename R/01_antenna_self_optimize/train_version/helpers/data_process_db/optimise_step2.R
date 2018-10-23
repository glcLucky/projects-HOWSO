# calculate_angle2<-function(select_cell,cell_2){
  all_select_cell<-unique(select_cell$ci) #选出方案中所有小区
  #查找到问题小区的OTT数据
  
  time_begin<-gsub("-", "", (Sys.Date()-30))#30天数据
  time_end<-gsub("-", "", (Sys.Date()))
  
  data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(all_select_cell, collapse ="','"),"')"," AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'"))
  
  
  #########################
  #先设置更新工参表为工参表
  ##########################
  sm_cl_u<-sm_cl
 
  #######################################
  #########给OTT数据关联邻区ECI##########
  #######################################
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
  if((nrow(data_OTT_u)!=0)&(length(selected_cell)!=0)&(!is.null(problem_cell))&(length(problem_cell)<length(unique(data_OTT_u$s_ci)))){
    #直接带ALexis代码
    source("helpers/optimise_main/1version.R", encoding = "UTF-8")
  }else{
    result_fl<-matrix(0, nrow = length(problem_cell), ncol = 3, byrow = TRUE)
    result_fl<-as.data.frame(result_fl)
    colnames(result_fl)<-c("ci","azimuth_calc_range","down_tilt_calc_range")
    result_fl$ci<-problem_cell
    print("There are warnings with data")
  }
  
  
  
  
  for(l in 1:nrow(result_fl)){
    dbExecute(pgdb,paste0("UPDATE project_details SET azimuth_calc_range = ", result_fl$azimuth_calc_range[l],", down_tilt_calc_range = ",result_fl$down_tilt_calc_range[l],", optimize_num = 1"," WHERE project_code=","'",project_code2[k],"'"," AND ci=","'",result_fl$ci[l],"'"," AND cell_type = '2'"))
  }
  # result_fl$ci[l]
  ################################
  #####计算相应的OTT数据##########
  ################################  
  #添加前7天的OTT评估
  time_begin7<-gsub("-", "", (Sys.Date()-7))
  # time_end7<-gsub("-", "", (Sys.Date()))
  
  #读取7天数据
  idex_time<-which(data_OTT_u$rpt_time>time_begin7 & data_OTT_u$rpt_time<time_end)
  data_OTT_7<-data_OTT[idex_time,]
  # 由于7天OTT数据可能为空，先做一个判断
  if(nrow(data_OTT_7)==0){
    print("data_OTT_7 is vide")
  }else{
    # names_OTT_ch<-c("调整前OTT采样点总数","调整前OTT弱采样点","调整前OTT覆盖率","调整前OTT满足条件的邻区数大等于3的样本点","调整前OTT重叠覆盖度")
    names_OTT<-c("before_ott_sample_cnt","before_ott_poor_cover_cnt","before_ott_cover_ratio","before_ott_overlap_cnt","before_ott_overlap_ratio")
    
    ######################
    ###写入前七天OTT评估####
    ######################
    project_code_ch<-project_code2[k]
    OTT_calculate_new(data_OTT_7,names_OTT,cell_type=2,project_code_ch)
    
  }
 
  
# }
