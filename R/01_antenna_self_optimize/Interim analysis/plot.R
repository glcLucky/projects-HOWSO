library(readxl)
library(dplyr)
library(RPostgreSQL) 
library(sqldf)
library(data.table)
library(RUnit)
library(DEoptim)
library(parallel)
library(ggplot2)
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
p_cell_pct = c()
for (i in 1:length(eci_lst)) {
  print(eci_lst[i])
  test = dbGetQuery(pgdb, paste0("SELECT s_ci, longitude, latitude, s_rsrp FROM test_neiment_mdt WHERE s_ci = '", eci_lst[i], "'"))
  test <- subset(test, ! is.na(test$s_rsrp))
  test['is_weak'] = 0
  test[which(test$s_rsrp < -110), 'is_weak'] <- 1
  w_pct <- mean(test$is_weak)
  if (w_pct > 0.1) {
    p_cell <- append(p_cell, eci_lst[i])
    p_cell_pct <-append(p_cell_pct, w_pct)
}}

p_cell <- c(31947416, 80591628, 80534040, 80534041, 80348685, 80455958, 80595212, 31817379)
p_cell_pct <- c(0.1625497, 0.4720000, 0.1161017, 0.2269939, 0.1434880, 0.7108317, 0.1933198, 0.1182864)
p_cell_all<-as.data.frame(p_cell)
names(p_cell_all) <- 'ci'
p_cell_all$ci<-as.character(p_cell_all$ci)
p_cell_all<-p_cell_all %>% inner_join(sm_cl[,c("ci","longitude","latitude")],by = c("ci" = "ci"))
p_cell_all <- p_cell_all[order(p_cell_all$longitude), ]

i <- 6
for (i in 1:length(p_cell)){
  print(i)
  ci = p_cell[i]
  ci_pct = p_cell_pct[i]
  df_test <- dbGetQuery(pgdb, paste0("SELECT * FROM test_neiment_mdt WHERE s_ci = '", ci, "'"))
  df_test <- df_test[(df_test$longitude > 85) & (df_test$latitude > 20), ]
  df_test <- df_test[(df_test$longitude < 130) & (df_test$latitude < 55), ]
  df_test['is_weak'] = 0
  df_test[which(df_test$s_rsrp < -110), 'is_weak'] <- 1
  df_test$is_weak <- factor(df_test$is_weak)
  
  x_dw <- min(min(df_test$longitude), p_cell_all[i, 'longitude']) - 0.0001
  x_up <- max(max(df_test$longitude), p_cell_all[i, 'longitude']) + 0.0001
  y_dw <- min(min(df_test$latitude), p_cell_all[i, 'latitude']) - 0.0001
  y_up <- max(max(df_test$latitude), p_cell_all[i, 'latitude']) + 0.0001
  # png(dir, width = 20, height = 80)
  plot <- ggplot(data = df_test, aes(x=longitude, y=latitude, shape = is_weak, color = is_weak, alpha=0.6)) +
    geom_point(size=2) + labs(title=paste("WEAK COVERAGE CELL: ", ci, "��������: ", ci_pct), x='LONGITUDE', y='LATITUDE') +
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", label = "�����", x = p_cell_all[i, 'longitude'], y = p_cell_all[i, 'latitude'], size = 3, colour = "red")+
    xlim(x_dw, x_up)+
    ylim(y_dw, y_up)
  ggsave(paste0(i, '_', ci, '.png'), plot) 
}



geom_text(data = p_cell, aes(x = longitude, y = latitude, label = "�����"))
x_dw <- min(min(df_test$longitude), p_cell_all$longitude) - 0.0001
x_up <- max(max(df_test$longitude), p_cell_all$longitude) + 0.0001
y_dw <- min(min(df_test$latitude), p_cell_all$latitude) - 0.0001
y_up <- max(max(df_test$latitude), p_cell_all$latitude) + 0.0001
xlim(x_dw, x_up)
ylim(y_dw, y_up)
ggplot(data = df_test, aes(x=longitude, y=latitude, shape = is_weak, color = is_weak) +geom_point(size=2) +labs(title="WEAK COVERAGE CELLS", x='LONGITUDE', y='LATITUDE') +theme(plot.title = element_text(hjust = 0.5)))


       + xlim(x_dw, x_up) + ylim(y_dw, y_up) + geom_text(data = p_cell, aes(x = longitude, y = latitude, label = "�����"))) 
  text(p_cell$longitude, p_cell$latitude, labels = "�����") 
)plot(df_test$longitude, df_test$latitude)
min(df_test$longitude)

df_test <- df_test[(df_test$longitude > 10) & (df_test$latitude > 10), ]