#######################
#计算距离
rad<-function(d){
  return(d*pi / 180.0)
}

GetDistance<-function(lon1,lat1,lon2,lat2){
  # 根据两个地方的经纬度计算两地的距离
  EARTH_RADIUS = 6378.137 #地球半径
  radLat1 = rad(lat1);
  radLat2 = rad(lat2);
  a = radLat1 - radLat2;
  b = rad(lon1) - rad(lon2);

  s = 2 * asin(sqrt((sin(a/2)^2) +cos(radLat1)*cos(radLat2)*(sin(b/2)^2)));
  s = s * EARTH_RADIUS;
  s=s*1000
  # s = (s * 10000) / 10000;
  return(s);
}
# library(geosphere)
# distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
# distm(c(119.7018,31.17453),c(119.6867,31.22556), fun = distHaversine)

##########################
ECI_cell_ch<-function(cellinfo,dataset,col_cell_use,col_cell_ch,cell_near_num){
  # cellinfo：公参数据 dataset：ott数据 col_cell_use ： 工参数据使用的列名  col_cell_ch: 标准列名
  #指纹库所需列名
  cell_near<-c(paste0("n",cell_near_num,"_earfcn"),paste0("n",cell_near_num,"_pci"))
  col_data_set<-c("s_ci","longitude","latitude","s_earfc","s_pci",cell_near)
  col_data_ch<-c("ECI","lon","lat","EARFCN","PCI","cell.1st.EARFCN","cell.1st.PCI")
  
  
  ###############################
  ################数据处理#######
  ###############################

  #确定所需列
  cellinfo_use<-cellinfo %>% select(col_cell_use)
  data_set_use<-dataset %>% select(col_data_set)
  
  #跟换路测列名
  colnames(data_set_use)<-col_data_ch
  
  #跟换工参列名
  colnames(cellinfo_use)<-col_cell_ch
  
  nms <- c("lon_cell", "lat_cell") 
  #需要添加一条变更数据格式的条件#从data.table 变换成data.frame
  cellinfo_use<-data.frame(cellinfo_use)
  cellinfo_use[,nms]<- lapply(cellinfo_use[,nms], as.double) 
  
  nms2 <- c("PCI_cell", "EARFCN_cell")
  cellinfo_use[,nms2]<- lapply(cellinfo_use[,nms2], as.integer)
  
  data_set_use<-as.data.frame(data_set_use)
  nms3 <- c("cell.1st.PCI", "cell.1st.EARFCN") 
  data_set_use[,nms3]<- lapply(data_set_use[,nms3], as.integer)
  data_set_use<-as.data.table(data_set_use)
  #可以在这里增加相关输出
  ###############################
  ############输出关联邻区ECI####
  ###############################
  #添加id以标出相关路测数据
  data_set_use$id<-1:dim(data_set_use)[1]
  
  # start.time <- Sys.time()
  # merge 工参数据和ott数据 并计算用户设备和小区的距离
  newdata<-data_set_use %>% 
    inner_join(cellinfo_use,by = c("cell.1st.PCI" = "PCI_cell", "cell.1st.EARFCN" = "EARFCN_cell")) %>%        select(names(data_set_use),"ECI_cell","lon_cell","lat_cell") %>% 
    mutate(dist=abs(lon-lon_cell)+abs(lat-lat_cell)) 
  # newdata$dist<-mapply(GetDistance,newdata$lon,newdata$lat,newdata$lon_cell,newdata$lat_cell)
  newdata2<-newdata %>% group_by(id) %>% slice(which.min(dist)) # 按id分组选择每组最小dist所对应的eci
  # newdata3<-newdata %>% group_by(id) %>% arrange(dist) %>% slice(1)
  # newdata3$dist2<-mapply(GetDistance,newdata3$lon,newdata3$lat,newdata3$lon_cell,newdata3$lat_cell)
  newdata2$dist2<-mapply(GetDistance,newdata2$lon,newdata2$lat,newdata2$lon_cell,newdata2$lat_cell)     
  list(ECI_cell=newdata2$ECI_cell,id=newdata2$id) 
}
#####################
## 建表
##
ECI_cell_near<-function(cellinfo,dataset,col_cell_use,col_cell_ch){
  # cellinfo：公参数据 dataset：OTT数据 
  #路测数据
  ## tester cellinfo for this data
  # dataset <- fread(file_test,header=T,encoding = "UTF-8")#类似于路测数据
  dataset2<-dataset
  # exp1<-ECI_cell_ch(file_cell,file_test,col_cell_use,col_cell_ch,cell_near_num=1)
  # dataset2[exp$id,"邻区1的ECI"]<-exp$ECI_cell
  for(i in 1:9){
    exp<-ECI_cell_ch(cellinfo,dataset,col_cell_use,col_cell_ch,cell_near_num=i)
    dataset2[exp$id,paste("n",as.character(i),"_ci", sep = "")]<-exp$ECI_cell
    rm(exp)  
  }
  return(dataset2)
}

##############################
# 第一邻小区发生错误的地方 
# newdata2[which(newdata2$dist2>8000),c('ECI','lon','lat','lon_cell','lat_cell','ECI_cell','cell.1st.EARFCN','cell.1st.PCI')]
# cellinfo_use[which(cellinfo_use$PCI_cell==272 &cellinfo_use$EARFCN_cell==38544),]->a
#a[which.min(abs(a$lon_cell-119.3078)+abs(a$lat_cell-26.03692)),]

  