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
library(readxl)

rm(list = ls())

#############################
setwd("E:\\projects\\R\\01_antenna_self_optimize")  ## 本地
#setwd("E:/sky/20171221_allcode")
# setwd("/etl/algorithm/workspace")
# data_dir<-"results_ECI"
# file_OTT<-"data/20171207_fuzhou"# 暂时从数据库读取OTT数据

source("train_version/helpers/data_process_db/evolution_step2.R")#数据库、OTT计算、关联小区计算(联调小区计算)
# , encoding = "UTF-8")
#包含selected_cell和OTT计算
source("train_version/helpers/data_process_db/near_cell.R",encoding = "UTF-8") #计算邻区ci
source("train_version/helpers/optimise_helpers/initialization.R"); initialization() # for encoding issues #识别中文系统
source("train_version/helpers/optimise_helpers/telecom_func.R", encoding = "UTF-8") #通信里面的概念
source("train_version/helpers/optimise_helpers/evolution_func.R", encoding = "UTF-8") #评估概念 cost
rm(initialization, reinitialization)
#
#读取OTT数据
# data_OTT_f <- fread("train_version/data/plan_ott_data/quanzhou_DT_OTT.csv",encoding = 'UTF-8') ##新给的路测数据
# data_OTT_f <- fread("train_version/data/plan_ott_data/plan_ott_data.csv",encoding = 'UTF-8',select=c(paste0("V",c(1:45))))
data_OTT_f <- fread("train_version/data/plan_ott_data/data_OTT_f_subset.csv",encoding = 'UTF-8',select=c(paste0("V",c(1:45))))
names(data_OTT_f)<-c("rpt_time", "s_ci","longitude","latitude","s_rsrp","s_enb_id","s_sector","s_earfc","s_pci","n1_rsrp","n2_rsrp","n3_rsrp","n4_rsrp","n5_rsrp","n6_rsrp","n7_rsrp","n8_rsrp","n9_rsrp","n1_earfcn","n2_earfcn","n3_earfcn","n4_earfcn","n5_earfcn","n6_earfcn","n7_earfcn","n8_earfcn","n9_earfcn","n1_pci","n2_pci","n3_pci","n4_pci","n5_pci","n6_pci","n7_pci","n8_pci","n9_pci", paste0("n",1:9,"_ci"))
# 读取工参数据
sm_cl<-fread("train_version/data/sm_cl_location/sm_cl_location.csv",encoding='UTF-8')
column_names <-c("id","ci","cell_name","site_name","enodeb_id","area","grid","local_cell_id","cell_coveage" ,"mstxpwr","reference_signal", "port_signal_ratio","hannel_number", "cover_type","overlay_scene","vender", "longitude", "latitude","working_band","pitching_angle","azimuth","groud_height","horizontal_beam_feat" ,"vertical_beam_feat", "antenna_model" , "m_down_tilt","down_tilt","antenna_type","antenna_gain","e_down_tilt" ,"street","city","county","pci","temp") 
names(sm_cl) <- column_names



###########################################################################################################
####################################确定问题小区###########################################################  
new_table<- read_excel("train_version/data/table/test.xlsx")
p_cell<-NULL
p_cell$ci<-new_table$CI
p_cell<-as.data.frame(p_cell)
p_cell$ci<-as.character(p_cell$ci)
p_cell<-p_cell %>% inner_join(sm_cl[,c("ci","longitude","latitude")],by = c("ci" = "ci"))

idex<-which(data_OTT_f$s_ci %in% p_cell$ci)
data_OTT<-data_OTT_f[idex,]
data_OTT<-as.data.frame(data_OTT)
###########################################################################################################



###########################################################################################################
####################################确定联调小区###########################################################  
sm_cl<-as.data.frame(sm_cl)
a<-liantiao_cell(data_OTT,p_cell$ci,sm_cl) # 包含问题小区和两种联调小区
problem_cell<-a
###########################################################################################################



###########################################################################################################
####################################确定评估小区###########################################################  
# 将距离问题小区550米的小区都纳入到评估小区范围中
# 计算工参表中所有小区到问题小区的距离 然后选择距离小于550米的小区作为评估集
#新建数据newtable为
newtable<-NULL
newtable<-sm_cl[,c("ci","longitude","latitude")]

# 计算工参表中所有小区到问题小区距离
for(i in 1:nrow(p_cell)){
  p_cell_lon = rep(p_cell$longitude[i], nrow(newtable))
  p_cell_lat = rep(p_cell$latitude[i], nrow(newtable))
  newtable[[paste0("dist", i)]] =
    GetDistance(p_cell_lon, p_cell_lat,
            newtable$longitude, newtable$latitude)
}

# 选择距离最小值作为每个小区到问题小区的使用距离
if(nrow(p_cell)==1){
  newtable$dist_use<-newtable$dist1
}else{
  newtable$dist_use<-apply(newtable[,c(paste0("dist",1:nrow(p_cell)))],
                           1,min)
}

# 选择距离小于550米的小区
new3<-newtable[,c('ci','dist_use')]
idex_ch<-which(new3$dist_use<=550)# 先设置550米
new3<-new3[idex_ch,]
selected_cell<-new3$ci
############################################################################################################

# 选择ott数据中主小区在selected_cell(评估小区)中数据
idex_s<-which(data_OTT_f$s_ci %in% selected_cell)
data_OTT<-data_OTT_f[idex_s,]
data_OTT<-as.data.frame(data_OTT)



###########################################################################################################
#################################给OTT数据关联邻区ECI######################################################  
# 建立工参表副本 以备更新
sm_cl_u<-sm_cl

# 小区配置表、工参所需数据
col_cell_use<-c( "ci","cell_name","pci","hannel_number","latitude", "longitude") # hannel_number即enodeb_id
col_cell_ch<-c("ECI_cell","cellname_cell","PCI_cell","EARFCN_cell","lat_cell","lon_cell") # 标准列名

#为每个邻区匹配ECI
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


###########################################################################################################
#############################运行Alexis代码，计算出最终调整方案############################################
#要四个数据：
#1、cellinfo勘站后数据 sm_cl_u /
#2、selected_cell 评估小区
#3、prolem_cell 问题小区+联调小区
#4、data_OTT  data_OTT_up 

#cellinfo<-sm_cl_u
#data_OTT<-data_ott_up
#联调小区
# problem_cell<-unique(cell_2$ci[which(!is.na(cell_2$ci))])
# 
# #评估小区
# selected_cell<-unique(select_cell$ci[which(!is.na(select_cell$ci))])

# f_idx<-which(problem_cell %in% selected_cell)
# length(f_idx)-length(problem_cell)>0

#角度范围 down_tilt_range //azimuth_range
#########################
#首先判断已经勘站完成
#########################
##首先赋值


#names(cellinfo<-names(cellinfo)
#Alexis 代码
source("train_version/helpers/optimise_main/1version_v2.R", encoding = "UTF-8")

fwrite( result_fl, "test.csv", row.names = FALSE,col.names = TRUE, sep = ",")


