####################
####################
####################
# 计算delta，判断同频点并且rsrp主小区比邻小区小于-6db的小区
delta_frequency_func_ch= function(df_updated, nb_neighbor_eci) {
  ## Which rows have RSRP_i - RSRP_0 > -6, for each neighbor cell i?
  df_rsrp = df_updated[, paste0("n", 1:nb_neighbor_eci,"_rsrp"), with = FALSE]
  df_rsrp0 = df_updated[, paste0("s","_rsrp"), with = FALSE][[1]]
  df_rsrp_diff_bool = df_rsrp - df_rsrp0 > -6
  
  ## Which rows have carrier_i == carrier_0, for each neighbor cell i?
  df_carrier0 = df_updated[, paste0("s","_earfc"), with = FALSE][[1]]
  df_carrier = df_updated[, paste0("n", 1:nb_neighbor_eci,"_earfcn"), with = FALSE]
  df_carrier_same = df_carrier == df_carrier0
  
  ## Which rows have those 2 together
  matrix_index_equal = df_rsrp_diff_bool * df_carrier_same
  
  ## Take the sum, i.e. the number of times there is 
  # RSRP_i - RSRP_0 > -6 and carrier_i == carrier_0
  # for i from 1 to nb_neighbor_eci
  delta = rowSums(matrix_index_equal, na.rm = TRUE)
  return(delta)
}


##################
##OTT 覆盖率计算##
##################
OTT_calculate<-function(data_OTT,names_OTT,cell_type=1){
  if(nrow(data_OTT)>0){
    #邻区数目
    nb_neighbor_eci<-9
    #计算ORR的delta_overlap
    data_OTT<-data.table(data_OTT)
    data_OTT[["delta_overlap"]] = delta_frequency_func_ch(data_OTT, nb_neighbor_eci)
    df_summarize = data_OTT %>%
      group_by(s_ci) %>%
      #采样点数
      summarize(ott_sample_cnt = n(),
                # 以下为全局评估函数：弱覆盖小区数+重叠覆盖小区数+各小区弱覆盖率+各小区重叠覆盖率       #弱覆盖采样点数       
                ott_poor_cover_cnt = sum(s_rsrp< (-110),na.rm=TRUE),
                #OTT 覆盖率
                ott_cover_ratio=100-round((sum(s_rsrp< (-110),na.rm=TRUE)/ott_sample_cnt)*100,2),
                #邻区>=3的采样点数
                ott_overlap_cnt=sum((s_rsrp >= -110) & (delta_overlap >= 3),na.rm=TRUE),
                #邻区>=3的重叠覆盖采样点
                ott_overlap_ratio=round((ott_overlap_cnt/ott_sample_cnt)*100,2))
    # return(df_summarize)
    for(i in 1:length(df_summarize$s_ci)){
      dbExecute(pgdb,paste0("UPDATE project_details SET ",names_OTT[1],"=" ,df_summarize$ott_sample_cnt[i],", ",names_OTT[2],"=",df_summarize$ott_poor_cover_cnt[i], ", ",names_OTT[3],"=",df_summarize$ott_cover_ratio[i],", ",names_OTT[4],"=",df_summarize$ott_overlap_cnt[i],", ",names_OTT[5],"=",df_summarize$ott_overlap_ratio[i],"  WHERE project_code=","'",project_code[k],"'", " ANd ci = ","'",df_summarize$s_ci[i],"'"," ANd cell_type = ","'",cell_type,"'"))
      print(i)
    }
  }
}

#######################
#计算selected_cell####
#######################

selected_cell2<-function(p_cell,newtable){
  for(i in 1:nrow(p_cell)){
    p_cell_lon = rep(p_cell$longitude[i], nrow(newtable))
    p_cell_lat = rep(p_cell$latitude[i], nrow(newtable))
    newtable[[paste0("dist", i)]] =
      GetDistance(p_cell_lon, p_cell_lat,
              newtable$longitude, newtable$latitude)
  }
  
  ##  计算距离小值
  # newtable$dist_use<-apply(newtable[,c(paste0("dist",1:dim(p_cell)[1]))],
  #                          1,min,na.rm=TRUE)
  if(nrow(p_cell)==1){
    newtable$dist_use<-newtable$dist1
  }else{
    newtable$dist_use<-apply(newtable[,c(paste0("dist",1:nrow(p_cell)))],
                             1,min)
  }
  
  new3<-newtable[,c('ci','dist_use')]
  # 
  # new3<-newtable %>% group_by(ci)%>%
  #   # filter(!is.na(dist1)) %>%
  #   select(c(names(newtable),'dist_use'))
  
  # idex_ch<-which((!new3$ci %in% p_cell$ci)&(new3$dist_use<=550))
  idex_ch<-which(new3$dist_use<=550)# 先设置550米
  new3<-new3[idex_ch,]
  if(nrow(new3)>0){
    new3$project_code<-project_code2[k]
    # names_use<-names(new3)[which(names(new3) %in% names(project_details))]
    new4<-new3[,c("project_code","ci")]
    new4$cell_type<-"2"
    new4$cell_type[which(new3$ci %in% p_cell$ci)]<-"1"
    #计算 selected_cell
    # new42<-new4[which(new4$cell_type %in% "2"),]
    
    ##############################
    ##计算关联小区#####
    ##############################
    
    select_cell<-NULL
    select_cell<-matrix(NA, nrow = length(new4$ci), ncol = length(names(project_details)), byrow = TRUE)
    select_cell<-as.data.frame(select_cell)
    colnames(select_cell)<-names(project_details)
    select_cell[,"project_code"]<-project_code2[k]
    select_cell$ci<-new4$ci
    select_cell<-select_cell[,c(names(project_details))]
    #selected_cell type
    select_cell$cell_type<-"3"
  }else{
    select_cell<-NULL
  }
  
  return(select_cell)
}

########################
##updatechange angle#####
#########################

update_angle<-function(up_cellinfo){
 #对空值进行设置
  up_cellinfo[which(is.na(up_cellinfo$azimuth_range) | up_cellinfo$azimuth_range %in% "-"),"azimuth_range"] = "-60~60"
  up_cellinfo[which(is.na(up_cellinfo$down_tilt_range) | up_cellinfo$down_tilt_range %in% "-"),"down_tilt_range"] = "-12~12"
  #解出所有数据
  up_cellinfo$azimuth_min<-as.numeric(sapply(strsplit(up_cellinfo$azimuth_range, "\\~"), "[[", 1)) #值
  up_cellinfo$azimuth_max<-as.numeric(sapply(strsplit(up_cellinfo$azimuth_range, "\\~"), "[[", 2))
  up_cellinfo$down_tilt_min<-as.numeric(sapply(strsplit(up_cellinfo$down_tilt_range, "\\~"), "[[", 1))
  up_cellinfo$down_tilt_max<-as.numeric(sapply(strsplit(up_cellinfo$down_tilt_range, "\\~"), "[[", 2))
  #
  up_cellinfo[which(up_cellinfo$azimuth_min < -60),"azimuth_min"] = -60
  up_cellinfo[which(up_cellinfo$azimuth_max  > 60),"azimuth_max"] = 60
  # up_cellinfo$azimuth_range_new = paste(up_cellinfo$azimuth_min,up_cellinfo$azimuth_max,sep="~")
  up_cellinfo[which(up_cellinfo$down_tilt_min < -12),"down_tilt_min"] = -12
  up_cellinfo[which(up_cellinfo$down_tilt_max  > 12),"down_tilt_max"] = 12
  # up_cellinfo$down_tilt_range_new = paste(up_cellinfo$down_tilt_min,up_cellinfo$down_tilt_max,sep="~")
  # 增加道路覆盖条件
  idex_road_cover<-which(up_cellinfo$is_road_cover %in% "1")
  up_cellinfo[idex_road_cover,c("azimuth_min","azimuth_max","down_tilt_min","down_tilt_max")] = 0 
  up_cellinfo[which(up_cellinfo$is_road_cover %in% '1'),c("azimuth_min","azimuth_max","down_tilt_min","down_tilt_max")] = 0
  return(up_cellinfo)
}


##########################################
########update results 调站方案###########
#########################################
# results_angle<-function(wminmax,est.ras_ch,problem_cell){
#   
#   
# }

#############################################
OTT_calculate_new<-function(data_OTT,names_OTT,cell_type,project_code_ch){
  if(nrow(data_OTT)>0){
    #邻区数目
    nb_neighbor_eci<-9
    #计算ORR的delta_overlap
    data_OTT<-data.table(data_OTT)
    data_OTT[["delta_overlap"]] = delta_frequency_func_ch(data_OTT, nb_neighbor_eci)
    df_summarize = data_OTT %>%
      group_by(s_ci) %>%
      #采样点数
      summarize(ott_sample_cnt = n(),
                # 以下为全局评估函数：弱覆盖小区数+重叠覆盖小区数+各小区弱覆盖率+各小区重叠覆盖率       #弱覆盖采样点数       
                ott_poor_cover_cnt = sum(s_rsrp< (-110),na.rm=TRUE),
                #OTT 覆盖率
                ott_cover_ratio=100-round((sum(s_rsrp< (-110),na.rm=TRUE)/ott_sample_cnt)*100,2),
                #邻区>=3的采样点数
                ott_overlap_cnt=sum((s_rsrp >= -110) & (delta_overlap >= 3),na.rm=TRUE),
                #邻区>=3的重叠覆盖采样点
                ott_overlap_ratio=round((ott_overlap_cnt/ott_sample_cnt)*100,2))
    # return(df_summarize)
    for(i in 1:length(df_summarize$s_ci)){
      dbExecute(pgdb,paste0("UPDATE project_details SET ",names_OTT[1],"=" ,df_summarize$ott_sample_cnt[i],
                            ", ",names_OTT[2],"=",df_summarize$ott_poor_cover_cnt[i], ", ",names_OTT[3],"=",
                            df_summarize$ott_cover_ratio[i],", ",names_OTT[4],"=",df_summarize$ott_overlap_cnt[i],
                            ", ",names_OTT[5],"=",df_summarize$ott_overlap_ratio[i],"  WHERE project_code=","'",project_code_ch,"'", " ANd ci = ","'",df_summarize$s_ci[i],"'"," ANd cell_type = ","'",cell_type,"'"))
      print(i)
    }
  }
}

##################################
#######计算联调小区程序###########
##################################
liantiao_cell<-function(data_OTT,problem_cell,sm_cl){
  #OTT加邻区ci
  #查找到问题小区的OTT数据
  
  # time_begin<-gsub("-", "", (Sys.Date()-30))#30天数据
  # time_end<-gsub("-", "", (Sys.Date()))
  # 
  # data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(problem_cell, collapse ="','"),"')"," AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'"))
  
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
  
  ##################################
  
  # problem_cell<-c("460-00-223924-145","460-00-252354-134","460-00-252354-135")
  # 同频高强度
  data_OTT_ch<-data_OTT_u
  # 判断同频
  data_OTT_ch[,paste0("same_EARFCN_",1:9)]<-(data_OTT_ch[,c(paste0("n",1:9,"_earfcn"))]==data_OTT_ch$s_earfc)
  #强邻区rsrp信号统计
  data_OTT_ch[,paste0("overlap_",1:9)]<-(data_OTT_ch[,c(paste0("n",1:9,"_rsrp"))]-data_OTT_ch$s_rsrp>-6)
  #合并两个字段,把为空情况设为FALSE
  data_OTT_ch[,paste0("index_",1:9)]<-(data_OTT_ch[,paste0("overlap_",1:9)] & data_OTT_ch[,paste0("same_EARFCN_",1:9)] & (!is.na(data_OTT_ch[,paste0("overlap_",1:9)])) & (!is.na(data_OTT_ch[,paste0("same_EARFCN_",1:9)])))
  
  #统计待加入小区排序
  a<-NULL;b<-NULL
  for(i in 1:9){
    a<-data_OTT_ch[data_OTT_ch[,paste0("index_",i)],c("s_ci",paste0("n",i,"_ci"))]
    names(a)<-c("s_ci","n1_9_ci")
    b<-rbind(b,a)
  }
  
  # begin.time<-Sys.time()
  data_ch<-b %>% group_by(s_ci,n1_9_ci) %>%
    summarize(ci_nb = n()) 
  
  data_ch2<-data_ch %>% group_by(s_ci) %>%
    slice(which.max(ci_nb)) %>%
    select(s_ci,n1_9_ci,ci_nb)
  # end.time<-Sys.time()
  # print(end.time-begin.time)
  
  ## 最频繁邻区的经纬度
  data_u2<-data_ch2 %>% 
    inner_join(sm_cl,by = c("n1_9_ci" = "ci")) %>%       
    select(names(data_ch2),"longitude","latitude")
  # lon1<-sm_cl[which(sm_cl$ci %in% data_ch2$s_ci),"longitude"]
  # lat1<-sm_cl[which(sm_cl$ci %in% data_ch2$s_ci),"latitude"]
  
  ## 主小区的经纬度
  data_u21<-data_ch2 %>% 
          inner_join(sm_cl,by = c("s_ci" = "ci")) %>%       
          select(names(data_ch2),"longitude","latitude")
  
  ## 两者匹配在一起
  data_u3 = data_u2 %>% inner_join(data_u21,by = c('s_ci'='s_ci'))
  names(data_u3) = c('s_ci','n1_9_ci_1','ci_nb_1','lon_n19ci','lat_n19ci','n1_9_ci_2','ci_nb_2','lon_sci','lat_sci')
  
  if(length(data_u3$lon_sci)!=0 && length(data_u3$lat_sci)!=0 ){
    data_u3$dist<-mapply(GetDistance,data_u3$lon_sci,data_u3$lat_sci,data_u3$lon_n19ci, data_u3$lat_n19ci)
  }else{
    data_u3<-NULL
  }
  data_u3<-data_u3[which(data_u3$dist<550),]
  
  # 判断为空的情况
  if(is.null(data_ch2)){
    problem_cell<-problem_cell
  } else{
    problem_cell<-c(problem_cell,data_u3$n1_9_ci_1)
  } 
  problem_cell<-unique(problem_cell)
  
  print("problem_cell sauf enodeb_id is...")
  print(problem_cell)
  
  # 选取同enode基站的小区  并且同频的小区
  idex<-which(sm_cl$ci %in% problem_cell)
  chois_Enode_fcn_id<-sm_cl[idex,c("enodeb_id","hannel_number","cover_type")]
  chois_Enode_fcn_id$enode_fcn = paste(chois_Enode_fcn_id$enodeb_id,chois_Enode_fcn_id$hannel_number,sep = '|')
  sm_cl$enode_fcn <- paste(sm_cl$enodeb_id,sm_cl$hannel_number,sep = '|')
  
  idex_enode_id<-which(sm_cl$enode_fcn %in% unlist(unique(chois_Enode_fcn_id$enode_fcn)))
  out_ci_ch<-sm_cl[idex_enode_id,c("ci","longitude","latitude","hannel_number","cover_type")]
  
  #添加距离判断
  for(i in 1:length(problem_cell)){
    lon1_ch<-rep(sm_cl[which(sm_cl$ci %in% problem_cell[i]),"longitude"], nrow(out_ci_ch))
    lat1_ch<-rep(sm_cl[which(sm_cl$ci %in% problem_cell[i]),"latitude"], nrow(out_ci_ch))
    out_ci_ch[[paste0("dist", i)]] =
      GetDistance(lon1_ch, lat1_ch,
              out_ci_ch$longitude, out_ci_ch$latitude)
  }
  if(length(problem_cell)==1){
    out_ci_ch$dist_use<-out_ci_ch$dist1
  }else{
    out_ci_ch$dist_use<-apply(out_ci_ch[,c(paste0("dist",1:length(problem_cell)))],
                              1,min)
  }
  
  # print(out_ci_ch)
  idex_ci_ch<-which(out_ci_ch$dist_use<=50 & out_ci_ch$cover_type == '室外')# 先设置550米
  # print(idex_ci_ch)
  out_ci<-unique(out_ci_ch[idex_ci_ch,'ci'])
  idex_n_in<-which(out_ci_ch$dist_use > 50)
  idex_indoor <- which(out_ci_ch$cover_type == '室内')
  # print(idex_n_in)
  print("liantiao cell is...")#输出结果
  print(out_ci)
  print("liantiao cell out of 50m ci is: ")
  print(out_ci_ch$ci[idex_n_in])
  print("liantiao cell is not outdoor is: ")
  print(out_ci_ch$ci[idex_indoor])
  
  # end.time<-Sys.time()
  # print(end.time-begin.time)
  #完成项目关联部分，关闭连接
  return(unlist(out_ci))
}


#############################################
OTT_calculate_new<-function(data_OTT,names_OTT,cell_type,project_code_ch){
  if(nrow(data_OTT)>0){
    #邻区数目
    nb_neighbor_eci<-9
    #计算ORR的delta_overlap
    data_OTT<-data.table(data_OTT)
    data_OTT[["delta_overlap"]] = delta_frequency_func_ch(data_OTT, nb_neighbor_eci)
    df_summarize = data_OTT %>%
      group_by(s_ci) %>%
      #采样点数
      summarize(ott_sample_cnt = n(),
                # 以下为全局评估函数：弱覆盖小区数+重叠覆盖小区数+各小区弱覆盖率+各小区重叠覆盖率       #弱覆盖采样点数       
                ott_poor_cover_cnt = sum(s_rsrp< (-110),na.rm=TRUE),
                #OTT 覆盖率
                ott_cover_ratio=100-round((sum(s_rsrp< (-110),na.rm=TRUE)/ott_sample_cnt)*100,2),
                #邻区>=3的采样点数
                ott_overlap_cnt=sum((s_rsrp >= -110) & (delta_overlap >= 3),na.rm=TRUE),
                #邻区>=3的重叠覆盖采样点
                ott_overlap_ratio=round((ott_overlap_cnt/ott_sample_cnt)*100,2))
    # return(df_summarize)
    for(i in 1:length(df_summarize$s_ci)){
      dbExecute(pgdb,paste0("UPDATE project_details SET ",names_OTT[1],"=" ,df_summarize$ott_sample_cnt[i],", ",names_OTT[2],"=",df_summarize$ott_poor_cover_cnt[i], ", ",names_OTT[3],"=",df_summarize$ott_cover_ratio[i],", ",names_OTT[4],"=",df_summarize$ott_overlap_cnt[i],", ",names_OTT[5],"=",df_summarize$ott_overlap_ratio[i],"  WHERE project_code=","'",project_code_ch,"'", " ANd ci = ","'",df_summarize$s_ci[i],"'"," ANd cell_type = ","'",cell_type,"'"))
      print(i)
    }
  }
}
#################################
selected_cell_new<-function(p_cell,newtable,project_code_ch){
  for(i in 1:nrow(p_cell)){
    p_cell_lon = rep(p_cell$longitude[i], nrow(newtable))
    p_cell_lat = rep(p_cell$latitude[i], nrow(newtable))
    newtable[[paste0("dist", i)]] =
      GetDistance(p_cell_lon, p_cell_lat,
              newtable$longitude, newtable$latitude)
  }
  
  ##  计算距离小值
  # newtable$dist_use<-apply(newtable[,c(paste0("dist",1:dim(p_cell)[1]))],
  #                          1,min,na.rm=TRUE)
  if(nrow(p_cell)==1){
    newtable$dist_use<-newtable$dist1
  }else{
    newtable$dist_use<-apply(newtable[,c(paste0("dist",1:nrow(p_cell)))],
                             1,min)
  }
  
  new3<-newtable[,c('ci','dist_use')]
  # 
  # new3<-newtable %>% group_by(ci)%>%
  #   # filter(!is.na(dist1)) %>%
  #   select(c(names(newtable),'dist_use'))
  
  # idex_ch<-which((!new3$ci %in% p_cell$ci)&(new3$dist_use<=550))
  idex_ch<-which(new3$dist_use<=550)# 先设置550米
  new3<-new3[idex_ch,]
  if(nrow(new3)>0){
    new3$project_code<-project_code_ch
    # names_use<-names(new3)[which(names(new3) %in% names(project_details))]
    new4<-new3[,c("project_code","ci")]
    new4$cell_type<-"2"
    new4$cell_type[which(new3$ci %in% p_cell$ci)]<-"1"
    #计算 selected_cell
    # new42<-new4[which(new4$cell_type %in% "2"),]
    
    ##############################
    ##计算关联小区#####
    ##############################
    
    select_cell<-NULL
    select_cell<-matrix(NA, nrow = length(new4$ci), ncol = length(names(project_details)), byrow = TRUE)
    select_cell<-as.data.frame(select_cell)
    colnames(select_cell)<-names(project_details)
    select_cell[,"project_code"]<-project_code_ch
    select_cell$ci<-new4$ci
    select_cell<-select_cell[,c(names(project_details))]
    #selected_cell type
    select_cell$cell_type<-"3"
  }else{
    select_cell<-NULL
  }
  
  return(select_cell)
}

##################################
#######过滤###########
##################################
cell_info_filter <- function(lon_max, lon_min, lat_max, lat_min, cell_info, dist){
  # 选取工参表中小区经纬度在给定经纬度dist范围之内的小区 以提高匹配速度
  # lon: 给定经度 lat: 给定维度 cell_info: 工参表 dist: 给定距离
  
  dist_lat = (1/111.194) * dist # 维度扩充距离
  dist_lon = (1/111.194) * cos((lat_max + lat_min) / 2 ) * dist
  
  lat_up = lat_max + dist_lat
  lat_dw = lat_min - dist_lat
  lon_up = lon_max + dist_lon
  lon_dw = lon_min - dist_lon
  sm_cl_u <- cell_info[(cell_info$longitude >= lon_dw) & (cell_info$longitude <= lon_up) & (cell_info$latitude >= lat_dw) & (cell_info$latitude <= lat_up),]
  return(sm_cl_u)
  
}
