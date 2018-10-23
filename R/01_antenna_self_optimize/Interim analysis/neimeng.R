library(readxl)
library(dplyr)
library(RPostgreSQL) 
library(sqldf)
library(data.table)
################
##���ҵ�״̬####
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
setwd("E:\\projects\\R\\01_antenna_self_optimize")  ## ����
#setwd("E:/sky/20171221_allcode")
# setwd("/etl/algorithm/workspace")
# data_dir<-"results_ECI"
# file_OTT<-"data/20171207_fuzhou"# ��ʱ�����ݿ��ȡOTT����

source("train_version/helpers/data_process_db/evolution_step2.R")#���ݿ⡢OTT���㡢����С������(����С������)
# , encoding = "UTF-8")
#����selected_cell��OTT����
source("train_version/helpers/data_process_db/near_cell.R",encoding = "UTF-8") #��������ci
source("train_version/helpers/optimise_helpers/initialization.R"); initialization() # for encoding issues #ʶ������ϵͳ
source("train_version/helpers/optimise_helpers/telecom_func.R", encoding = "UTF-8") #ͨ������ĸ���
source("train_version/helpers/optimise_helpers/evolution_func.R", encoding = "UTF-8") #�������� cost
rm(initialization, reinitialization)

eci_lst<- read_excel("E:\\data\\01_antenna_self_optimize\\neimeng.xlsx")[, 'ECI']
eci_lst <- as.vector(unlist(eci_lst[1]))
####################
##�������ݿ�########
####################
drv = dbDriver("PostgreSQL") 
## �������ݿ�
pgdb = dbConnect(drv, user="feeder_dev", password="feeder_dev",dbname ="feeder_wuxi" ,port="5432" , host="192.168.101.66")

## �������ݿ�
# pgdb = dbConnect(drv, user="deeplan", password="deeplan",dbname ="deeplan" ,port="5432" , host="10.48.147.66")
#��������������������
postgresqlpqExec(pgdb, "SET client_encoding = 'utf-8'")
#�������ݿ������ݱ���
# dbListTables(pgdb)   

########################
#####��ȡ���ݿ��еı���#
########################
# ��ȡ���ݿ��еĹ��α�
# sm_cl <- dbReadTable(pgdb,"sm_cl_location_neimenggu")

sm_cl <- read_excel("E:\\data\\01_antenna_self_optimize\\sm_cl_neimenggu.xlsx")
sm_cl <- sm_cl[, c('С����', 'ECI', 'Longitude', 'Latitude', 'EARFCN', 'PCI', 'eNodeBID', 'Azimuth', '���߹Ҹ�', '��е����', '�������', 'ˮƽ��������', '��ֱ��������', '��������', 'working_band')]
names(sm_cl) <- c("cell_name", 'ci', 'longitude', 'latitude', 'hannel_number', 'pci', 'enodeb_id', 'azimuth', 'groud_height', 'm_down_tilt', 'down_tilt', 'horizontal_beam_feat', 'vertical_beam_feat', 'antenna_gain', 'working_band')
sm_cl = as.data.frame(sm_cl)
sm_cl$ci <- sapply(sm_cl$ci, as.character)

# ��ȡ����С���б��е�����С�� ����p_cell
p_cell = c()
for (i in 1:length(eci_lst)) {
  print(eci_lst[i])
  test = dbGetQuery(pgdb, paste0("SELECT s_ci, longitude, latitude, s_rsrp FROM test_neiment_mdt WHERE s_ci = '", eci_lst[i], "'"))
  test <- subset(test, ! is.na(test$s_rsrp))
  test['is_weak'] = 0
  test[which(test$s_rsrp < -110), 'is_weak'] <- 1
  if (mean(test$is_weak) > 0.1) p_cell <- append(p_cell, eci_lst[i])
}


p_cell_all <- c(31947416, 80591628, 80534040, 80534041, 80348685, 80455958, 80595212, 31817379)

p_cell_all<-as.data.frame(p_cell_all)
names(p_cell_all) <- 'ci'
p_cell_all$ci<-as.character(p_cell_all$ci)
p_cell_all<-p_cell_all %>% inner_join(sm_cl[,c("ci","longitude","latitude")],by = c("ci" = "ci"))
p_cell_all <- p_cell_all[order(p_cell_all$longitude), ]
plot(p_cell_all$longitude, p_cell_all$latitude)
p_cell_1 <- c('80595212')
p_cell_2 <- c('80455958', '80348685', '31947416')
p_cell_3 <- c('80534040', '80534041', '80591628', '31817379')


p_cell<-p_cell_all[p_cell_all$ci %in% p_cell_2, ]
# ��ȡ����С����mdt����
df_mdt <- dbGetQuery(pgdb, paste0("SELECT * FROM test_neiment_mdt WHERE s_ci in ('", paste(p_cell$ci, collapse ="','"), "')"))
data_OTT <- df_mdt
problem_cell <- p_cell$ci

liantiao_cell<-function(data_OTT,problem_cell,sm_cl){
  #OTT������ci
  #���ҵ�����С����OTT����
  
  # time_begin<-gsub("-", "", (Sys.Date()-30))#30������
  # time_end<-gsub("-", "", (Sys.Date()))
  # 
  # data_OTT<-dbGetQuery(pgdb,paste0("SELECT * FROM plan_ott_data WHERE s_ci in ","('", paste(problem_cell, collapse ="','"),"')"," AND rpt_time > ","'",time_begin,"'"," AND rpt_time < ","'",time_end,"'"))
  
  sm_cl_u<-sm_cl
  
  #######################################
  #########��OTT���ݹ�������ECI##########
  #######################################
  #С�����ñ���������������
  # col_cell_use<-c( "ci","cell_name","pci","earfcn","latitude", "longitude")
  col_cell_use<-c( "ci","cell_name","pci","hannel_number","latitude", "longitude")
  col_cell_ch<-c("ECI_cell","cellname_cell","PCI_cell","EARFCN_cell","lat_cell","lon_cell")
  
  
  #����
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
  # ͬƵ��ǿ��
  data_OTT_ch<-data_OTT_u
  # �ж�ͬƵ
  data_OTT_ch[,paste0("same_EARFCN_",1:9)]<-(data_OTT_ch[,c(paste0("n",1:9,"_earfcn"))]==data_OTT_ch$s_earfc)
  #ǿ����rsrp�ź�ͳ��
  data_OTT_ch[,paste0("overlap_",1:9)]<-(data_OTT_ch[,c(paste0("n",1:9,"_rsrp"))]-data_OTT_ch$s_rsrp>-6)
  #�ϲ������ֶ�,��Ϊ�������ΪFALSE
  data_OTT_ch[,paste0("index_",1:9)]<-(data_OTT_ch[,paste0("overlap_",1:9)] & data_OTT_ch[,paste0("same_EARFCN_",1:9)] & (!is.na(data_OTT_ch[,paste0("overlap_",1:9)])) & (!is.na(data_OTT_ch[,paste0("same_EARFCN_",1:9)])))
  
  #ͳ�ƴ�����С������
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
  
  ## ��Ƶ�������ľ�γ��
  data_u2<-data_ch2 %>% 
    inner_join(sm_cl,by = c("n1_9_ci" = "ci")) %>%       
    select(names(data_ch2),"longitude","latitude")
  # lon1<-sm_cl[which(sm_cl$ci %in% data_ch2$s_ci),"longitude"]
  # lat1<-sm_cl[which(sm_cl$ci %in% data_ch2$s_ci),"latitude"]
  ## ��С���ľ�γ��
  data_u21<-data_ch2 %>% 
    inner_join(sm_cl,by = c("s_ci" = "ci")) %>%       
    select(names(data_ch2),"longitude","latitude")
  
  ## ����ƥ����һ��
  data_u3 = data_u2 %>% inner_join(data_u21,by = c('s_ci'='s_ci'))
  names(data_u3) = c('s_ci','n1_9_ci_1','ci_nb_1','lon_n19ci','lat_n19ci','n1_9_ci_2','ci_nb_2','lon_sci','lat_sci')
  
  if(length(data_u3$lon_sci)!=0 && length(data_u3$lat_sci)!=0 ){
    data_u3$dist<-mapply(GetDistance,data_u3$lon_sci,data_u3$lat_sci,data_u3$lon_n19ci, data_u3$lat_n19ci)
  }else{
    data_u3<-NULL
  }
  data_u3<-data_u3[which(data_u3$dist<550),]
  
  # �ж�Ϊ�յ����
  if(is.null(data_ch2)){
    problem_cell<-problem_cell
  } else{
    problem_cell<-c(problem_cell,data_u3$n1_9_ci_1)
  } 
  problem_cell<-unique(problem_cell)
  
  print("problem_cell sauf enodeb_id is...")
  print(problem_cell)
  
  # ѡȡͬenode��վ��С��  ����ͬƵ��С��
  idex<-which(sm_cl$ci %in% problem_cell)
  chois_Enode_fcn_id<-sm_cl[idex,c("enodeb_id","hannel_number")]
  chois_Enode_fcn_id$enode_fcn = paste(chois_Enode_fcn_id$enodeb_id,chois_Enode_fcn_id$hannel_number,sep = '|')
  sm_cl$enode_fcn <- paste(sm_cl$enodeb_id,sm_cl$hannel_number,sep = '|')
  idex_enode_id<-which(sm_cl$enode_fcn %in% unlist(unique(chois_Enode_fcn_id$enode_fcn)))
  out_ci_ch<-sm_cl[idex_enode_id,c("ci","longitude","latitude","hannel_number")]
  #���Ӿ����ж�
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
  idex_ci_ch<-which(out_ci_ch$dist_use<=50)# ������550��
  # print(idex_ci_ch)
  out_ci<-unique(out_ci_ch[idex_ci_ch,'ci'])
  idex_n_in<-which(out_ci_ch$dist_use > 50)
  # print(idex_n_in)
  print("liantiao cell is...")#������
  print(out_ci)
  # end.time<-Sys.time()
  # print(end.time-begin.time)
  #�����Ŀ�������֣��ر�����
  return(unlist(out_ci))
}

a<-liantiao_cell(df_mdt, p_cell$ci, sm_cl)
problem_cell <- a


###########################################################################################################
####################################ȷ������С��###########################################################  
# ����������С��550�׵�С�������뵽����С����Χ��
# ���㹤�α�������С��������С���ľ��� Ȼ��ѡ�����С��550�׵�С����Ϊ������
#�½�����newtableΪ
newtable<-NULL
newtable<-sm_cl[,c("ci","longitude","latitude")]

# ���㹤�α�������С��������С������
for(i in 1:nrow(p_cell)){
  p_cell_lon = rep(p_cell$longitude[i], nrow(newtable))
  p_cell_lat = rep(p_cell$latitude[i], nrow(newtable))
  newtable[[paste0("dist", i)]] =
    GetDistance(p_cell_lon, p_cell_lat,
                newtable$longitude, newtable$latitude)
}

# ѡ�������Сֵ��Ϊÿ��С��������С����ʹ�þ���
if(nrow(p_cell)==1){
  newtable$dist_use<-newtable$dist1
}else{
  newtable$dist_use<-apply(newtable[,c(paste0("dist",1:nrow(p_cell)))],
                           1,min)
}

# ѡ�����С��550�׵�С��
new3<-newtable[,c('ci','dist_use')]
idex_ch<-which(new3$dist_use<=50)# ������550��
new3<-new3[idex_ch,]
selected_cell<-new3$ci
# selected_cell <- c("31947415", "31947416", "31947417", "80348683", "80348684", "80348685", "80455957", "80455958", "80455959", "80481055", "80481056")
############################################################################################################

# ѡ��ott��������С����selected_cell(����С��)������
df_mdt_selected <- dbGetQuery(pgdb, paste0("SELECT * FROM test_neiment_mdt WHERE s_ci in ('", paste(selected_cell, collapse ="','"), "')"))
data_OTT<-as.data.frame(df_mdt_selected)


###########################################################################################################
#################################��OTT���ݹ�������ECI######################################################  
# �������α����� �Ա�����
sm_cl_u<-sm_cl

# С�����ñ���������������
col_cell_use<-c( "ci","cell_name","pci","hannel_number","latitude", "longitude") # hannel_number��enodeb_id
col_cell_ch<-c("ECI_cell","cellname_cell","PCI_cell","EARFCN_cell","lat_cell","lon_cell") # ��׼����

#Ϊÿ������ƥ��ECI
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

fwrite( data_OTT_u, "data_OTT_u.csv", row.names = FALSE,col.names = TRUE, sep = ",")

###########################################################################################################
#############################����Alexis���룬��������յ�������############################################
#Ҫ�ĸ����ݣ�
#1��cellinfo��վ������ sm_cl_u /
#2��selected_cell ����С��
#3��prolem_cell ����С��+����С��
#4��data_OTT  data_OTT_up 

#cellinfo<-sm_cl_u
#data_OTT<-data_ott_up
#����С��
# problem_cell<-unique(cell_2$ci[which(!is.na(cell_2$ci))])
# 
# #����С��
# selected_cell<-unique(select_cell$ci[which(!is.na(select_cell$ci))])

# f_idx<-which(problem_cell %in% selected_cell)
# length(f_idx)-length(problem_cell)>0

#�Ƕȷ�Χ down_tilt_range //azimuth_range
#########################
#�����ж��Ѿ���վ���
#########################
##���ȸ�ֵ


#names(cellinfo<-names(cellinfo)
#Alexis ����
sm_cl_u<-data.table(sm_cl)
fwrite( sm_cl_u, "sm_cl_u.csv", row.names = FALSE,col.names = TRUE, sep = ",")
data_OTT_u <- fread("data_OTT_u.csv")
sm_cl_u <- fread("sm_cl_u.csv")
source("E:/projects/R/01_antenna_self_optimize/train_version/helpers/optimise_main/1version_v2_1.R", encoding = "UTF-8")
data_OTT_u <- data_OTT_u[sample(1:nrow(data_OTT_u), 500000), ]
fwrite( result_fl, "test2.csv", row.names = FALSE,col.names = TRUE, sep = ",")