##################################################
# Antenna Self-Optimizing via Evolutionary Model #
# Howso 2017                                     #
##################################################
# library(data.table)
# library(dplyr)
# library(SoDA) # for geoDist function in preprocessing
# library(RUnit)
# library(DEoptim)
# library(parallel)
# rm(list = ls())
# # setwd("E:/Alexis/Projects/天馈/20171121_code")
# setwd("E:/sky/20171225_tiankui")
# source("helpers/initialization.R"); initialization() # for encoding issues
# rm(initialization, reinitialization)
# source("helpers/telecom_func.R", encoding = "UTF-8")
# source("helpers/evolution_func.R", encoding = "UTF-8")


################################################################################################
################################# A - Data preprocessing #######################################


##############
# Parameters #
##############

# Directories

# data_dir = "data/20171207_fuzhou/"
# cellinfo_file = "tdl_cm_cell_20171025update.csv" # general info of cells
# problem_cell_file = "problem_cell_Kanzhan.csv" # cells' angle to be adjusted
# mr_zwk_file = "gushanOTT.csv" # contain time series collected with MR data
# selected_cell_file = "selected_cell.csv" # restrict MR data to those cells

##
# General parameters
##

# Number of neighbor ECI available in mr_zwk_file

nb_neighbor_eci = 9 # earfcn不全为空的邻区个数 这个数字先设置为所有邻区 然后根据后面函数计算出来

col_x = paste0("n", 1:nb_neighbor_eci,"_earfcn")

# 计算earfcn不全为空的邻区个数
update_nb_neighbor_eci = function(df, col_x) {
  df<-data.frame(df)
  nb_non_na_each_col_x = apply(df[,c(col_x)], 2, function(x){sum(!is.na(x))})
  nb_neighbor_eci = max(which(nb_non_na_each_col_x > 0)) # 
  return(nb_neighbor_eci)
}

nb_neighbor_eci <- update_nb_neighbor_eci(data_OTT_u, col_x)

# NA strings in all inputed data frames
na_strings = c("NA", "", "NULL")



######################################################################################
###########################Names of columns for mr_zwk_file###########################
# Feature names
mr_eci_feat = "s_ci"
mr_eci_feat_neighbors = paste0("n", 1:nb_neighbor_eci, "_ci")
mr_longitude_feat = "longitude"
mr_latitude_feat = "latitude"
mr_rsrp_feat = "s_rsrp"
mr_rsrp_feat_neighbors = paste0("n", 1:nb_neighbor_eci, "_rsrp")
mr_carrier_number_feat = "s_earfc"
nb_neighbor_eci = 3 # 强制设为3  因为数据量太大
mr_carrier_number_neighbor_feat = paste0("n", 1:nb_neighbor_eci, "_earfcn")

# Feature names together
mr_feat = c(mr_eci_feat, 
            mr_eci_feat_neighbors,
            mr_longitude_feat, 
            mr_latitude_feat,
            mr_carrier_number_feat,
            mr_carrier_number_neighbor_feat,
            mr_rsrp_feat,
            mr_rsrp_feat_neighbors)

# Numeric features
mr_numeric_feats = c(mr_longitude_feat, mr_latitude_feat,
                     mr_rsrp_feat,
                     mr_rsrp_feat_neighbors,
                     mr_carrier_number_neighbor_feat)

#
# Names of columns for cellinfo_file
# cellinfo_eci_feat = "CI"
# cellinfo_longitude_feat = "经度"
# cellinfo_latitude_feat = "纬度"
# cellinfo_antenna_height_feat = "天线挂高" # useful to define vertical angle
# cellinfo_frequency_band_feat = "频段指示" # useful for Enodeb_id_fre only
# cellinfo_antenna_gain_feat = "天线增益" # useful for func_gain function
# cellinfo_azimuth_feat = "方位角" # useful for delta azimuth
# cellinfo_down_dip_angle_feat = "下倾角" # useful for delta tilt
# cellinfo_horizontal_beam_feat = "水平波束宽度" # useful for func_gain function
# cellinfo_vertical_beam_feat = "垂直波束宽度" # useful for func_gain function
# cellinfo_enode_feat = "Enodeb_id" # useful for Enodeb_id_fre and helu only
# cellinfo_mechanical_dip_angle_feat = "机械下倾角" 
# # Features which are not used: "日期", "小区名称", "参考信号功率", "电下倾角"
###########################
cellinfo_eci_feat = "ci"
cellinfo_longitude_feat = "longitude"
cellinfo_latitude_feat = "latitude"
cellinfo_antenna_height_feat = "groud_height" # useful to define vertical angle
cellinfo_frequency_band_feat = "working_band" # useful for Enodeb_id_fre only
cellinfo_antenna_gain_feat = "antenna_gain" # useful for func_gain function
cellinfo_azimuth_feat = "azimuth" # useful for delta azimuth
cellinfo_down_dip_angle_feat = "down_tilt" # useful for delta tilt
cellinfo_horizontal_beam_feat = "horizontal_beam_feat" # useful for func_gain function
cellinfo_vertical_beam_feat = "vertical_beam_feat" # useful for func_gain function
cellinfo_enode_feat = "enodeb_id" # useful for Enodeb_id_fre and helu only
cellinfo_mechanical_dip_angle_feat = "m_down_tilt" 

# Feature names together
cellinfo_feat = c(cellinfo_eci_feat,
                  cellinfo_antenna_height_feat, 
                  cellinfo_longitude_feat,
                  cellinfo_latitude_feat,
                  cellinfo_frequency_band_feat, 
                  cellinfo_antenna_gain_feat, 
                  cellinfo_azimuth_feat,
                  cellinfo_down_dip_angle_feat, 
                  cellinfo_horizontal_beam_feat, 
                  cellinfo_vertical_beam_feat, 
                  cellinfo_enode_feat,
                  cellinfo_mechanical_dip_angle_feat)

# Numeric features
not_num = which(!cellinfo_feat %in% c(cellinfo_eci_feat, cellinfo_enode_feat))
cellinfo_numeric_feats = cellinfo_feat[not_num]
rm(not_num)

# Names of columns for problem_cell_file
problem_cell_eci_feat = "ECI"
# Names of columns for selected_cell_file
selected_eci_feat = "CI"
######################################################################################



######################################################################################
#####################################缺失值处理#######################################
# Change missing values in cellinfo
# Missing values are those equal to 0 or NA in cellinfo data frame
# Default value for "方位角" == cellinfo_azimuth_feat
# cellinfo[cellinfo_azimuth_feat]
def_azimuth_0 = 0

# Default value for horizontal_beam == azimuth_3db == "水平波束宽度"
# common values are {65, 80}
def_horizontal_beam_0 = 10 # set 10 for 0 

# Default value for vertical_beam == tilt_3db == "垂直波束宽度"
# common values are {5,9,  10, 60, 65}
def_vertical_beam_0 = 5 # set 5 for 0 

# Default value for antenna_gain == gain_antenna == "天线增益"
# common values are {4, 10, 12, 14, 15, 15.8, 16, 16.5, 17}
def_gain_antenna_0 = 5 # set 5 for 0

# Change missing values caused by missing neighbor ECIs 处理那些因为缺失eci信息而缺失的值
#
## In MR_zhiwenku data frame, many neighbors cells are not known and
# are NA. When merging with cell info, this induce NA values for quantities
# necessary to compute the gain (namely 水平波束宽度, 垂直波束宽度, 
# 天线增益, 方位角 and 下倾角).

# Those NA are replaced with default values
def_horizontal_beam = 60 # 60 for NA after merging
def_vertical_beam = 10 # 10 for NA after merging
def_gain_antenna = 5 # 5 for NA after merging
def_delta_azimuth = 130 # angle in ]0,360[
def_delta_tilt = 30 # between 0 and 80, usually smaller than 20
#########################################################################################



#########################################################################################
####################### 1. Loading MR dataframe (指纹库信息) ############################

# Fingerprint library is the core data frame.
# For each (ECI, time), various number of users are fingerprinted.
# For each user we get its position and different measure of quality
# of its received signal. This allows us to evaluate quality of service.
#
# "测量时间" --> time with hour granularity
# "小区ECI" --> ECI
# "经度" --> longitude (fingerprint of user's position)
# "纬度" --> latitude (fingerprint of user's position)
# "基站站号" --> base station number
# "服务小区的载波号" --> carrier number of service cell
# For i from 1 to 9:
# "邻区i的参考信号接收功率" --> receiving power of ith adjacent area
# "邻区i的载波号" --> carrier number of ith adjacent area
# "邻区i的ECI" --> ECI of adjacent region i

## Loading data
#  data_OTT_u= fread(file.path(data_dir, mr_zwk_file), header=TRUE
# # , select = mr_feat
# ,na.strings = na_strings,colClasses=list(character=42:45))
data_OTT_u<-data.table(data_OTT_u) # zwk 指纹库
mr_zwk<-data_OTT_u[,c(mr_feat),with=FALSE]
mr_zwk = as.data.frame(mr_zwk)


## Converting to numeric
# From https://stackoverflow.com/questions/37060791
mr_zwk[, mr_numeric_feats] = sapply(mr_zwk[, mr_numeric_feats], as.numeric)
rm(mr_feat, mr_numeric_feats)


## Deduce available cells in database 获取本次分析所有可用的小区eci 包括主小区和邻区
available_cells = sort(unique(mr_zwk[[mr_eci_feat]]))

for(i in 1:length(mr_eci_feat_neighbors)) {
  to_add = unique(mr_zwk[[mr_eci_feat_neighbors[i]]])
  available_cells = c(available_cells, to_add)
}
rm(to_add, i)
available_cells = unique(sort(available_cells))
print(paste0("There are ", length(available_cells), " cells available for analysis."))
#########################################################################################



#########################################################################################
######### 2. Loading cellinfo, containing one row information for each ECI ##############
# cellinfo has 212344 rows and 12 selected columns
# There are 212344 unique ECI in the dataframe, 
# so data frame contains information for each ECI.
#
# "CI" --> ECI
# "天线挂高" --> antenna height
# "纬度" --> latitude
# "经度" --> longitude
# "频段指示" --> frequency band indication
# "天线增益" --> antenna gain
# "方位角" --> azimuth
# "机械下倾角" --> mechanical dip angle
# "下倾角" --> down dip angle
# "水平波束宽度" --> horizontal beam width
# "垂直波束宽度" --> vertical beam width
# "Enodeb_id" --> another id (less unique elements compared with CI)

## Load cellinfo
# sm_cl = fread(file.path(data_dir, cellinfo_file),
#                  header=TRUE,
#                  # select = cellinfo_feat,
#                  na.strings = na_strings)
sm_cl_u<-data.table(sm_cl_u)
cellinfo<-sm_cl_u[,c(cellinfo_feat),with=FALSE]

cellinfo = as.data.frame(cellinfo)
cellinfo[, cellinfo_numeric_feats] = sapply(cellinfo[, cellinfo_numeric_feats], as.numeric)
rm( cellinfo_feat, cellinfo_numeric_feats, na_strings)

## Select only cells for which MR data is available, or for selected cells
# 如果selected_cell不为空则选取这些小区的工参信息 否则就选取上述获得的可行小区
idx_select<-as.list(selected_cell)
if(length(idx_select)==0){
 dx_select = which(cellinfo[[cellinfo_eci_feat]] %in% available_cells)
}else{
 idx_select = which(cellinfo[[cellinfo_eci_feat]] %in% idx_select)
}

cellinfo = cellinfo[idx_select,]
rm(idx_select, available_cells, selected_eci_feat)

## Adding helu column
# 合路天线约束, 此连接语句可放在函数外面
cellinfo[["helu"]] = paste(cellinfo[[cellinfo_enode_feat]],
                           cellinfo[[cellinfo_antenna_height_feat]],
                           cellinfo[[cellinfo_down_dip_angle_feat]],
                           cellinfo[[cellinfo_azimuth_feat]], 
                           sep = "|")

## Adding enodb_id_fre column
# 共天线约束, 此连接语句可放在函数外面
cellinfo[["Enodeb_id_fre"]] = paste(cellinfo[[cellinfo_enode_feat]],
                                    cellinfo[[cellinfo_frequency_band_feat]],
                                    sep = "|")

## Replacing NA by default values
# for azimuth (替换小区异常信息):
cellinfo[[cellinfo_azimuth_feat]] = replace_0_and_na(cellinfo[[cellinfo_azimuth_feat]],
                                                     def_azimuth_0)

# for horizontal_beam == azimuth_3db == "水平波束宽度"
cellinfo[[cellinfo_horizontal_beam_feat]] = replace_0_and_na(cellinfo[[cellinfo_horizontal_beam_feat]],
                                                             def_horizontal_beam_0)

# for vertical_beam == tilt_3db == "垂直波束宽度"
cellinfo[[cellinfo_vertical_beam_feat]] = replace_0_and_na(cellinfo[[cellinfo_vertical_beam_feat]],
                                                           def_vertical_beam_0)

# for antenna_gain == gain_antenna == "天线增益"
cellinfo[[cellinfo_antenna_gain_feat]] = replace_0_and_na(cellinfo[[cellinfo_antenna_gain_feat]],
                                                          def_gain_antenna_0)

rm(def_azimuth_0, def_horizontal_beam_0, 
   def_vertical_beam_0, def_gain_antenna_0)

## Remove unused columns
cellinfo[cellinfo_enode_feat] = NULL
cellinfo[cellinfo_frequency_band_feat] = NULL
rm(cellinfo_enode_feat, cellinfo_frequency_band_feat)




######################################################################################################
############## 3. Loading problem cells, containing one row information for each ECI #################
# problem_cell = fread(file.path(data_dir, problem_cell_file), header=TRUE)
# names(problem_cell)[2]<-"ECI"
# problem_cell = as.data.frame(problem_cell)
# problem_cell = problem_cell[[problem_cell_eci_feat]]
# problem_cell = problem_cell[["ECI"]]

## Restrict MR data frame to problematic ECIs 
# Restrict to rows containing at least one problematic cell for main or neighbor ECI. 
# 约束MR数据的每一行至少包含一个联调小区(主区+邻区)
all_eci_feat = c(mr_eci_feat, mr_eci_feat_neighbors)
idx_select = sapply(all_eci_feat, 
                    function(feat){which(mr_zwk[[feat]] %in% problem_cell)})

# which(mr_zwk[['n3_ci']] %in% problem_cell) 选出n3_ci在问题小区列表中的行索引
idx_select = unique(unlist(idx_select)) # or with do.call
mr_zwk = mr_zwk[idx_select,]
rownames(mr_zwk) = NULL # 重编列序号
rm(problem_cell_eci_feat, all_eci_feat, idx_select)
# 4163 elements and 32 columns
#########################################################################################################



#########################################################################################################
############################## 4. Renaming columns of the data frames ###################################
## Renaming for cell_info
names(cellinfo)[which(names(cellinfo) == cellinfo_eci_feat)] = "ECI"
names(cellinfo)[which(names(cellinfo) == cellinfo_longitude_feat)] = "经度"
names(cellinfo)[which(names(cellinfo) == cellinfo_latitude_feat)] = "纬度"
rm(cellinfo_eci_feat, cellinfo_longitude_feat, cellinfo_latitude_feat)

names(cellinfo)[which(names(cellinfo) == cellinfo_antenna_gain_feat)] = "天线增益"
names(cellinfo)[which(names(cellinfo) == cellinfo_azimuth_feat)] = "方位角"
names(cellinfo)[which(names(cellinfo) == cellinfo_down_dip_angle_feat)] = "下倾角"
names(cellinfo)[which(names(cellinfo) == cellinfo_horizontal_beam_feat)] = "水平波束宽度"
names(cellinfo)[which(names(cellinfo) == cellinfo_mechanical_dip_angle_feat)] = "机械下倾角"
names(cellinfo)[which(names(cellinfo) == cellinfo_vertical_beam_feat)] = "垂直波束宽度"
names(cellinfo)[which(names(cellinfo) == cellinfo_antenna_height_feat)] = "天线挂高"
rm(cellinfo_antenna_gain_feat, cellinfo_azimuth_feat, 
   cellinfo_down_dip_angle_feat, cellinfo_horizontal_beam_feat,
   cellinfo_mechanical_dip_angle_feat, cellinfo_vertical_beam_feat,
   cellinfo_antenna_height_feat)

## Renaming for mr_zwk
names(mr_zwk)[which(names(mr_zwk) == mr_eci_feat)] = "ECI0"
names(mr_zwk)[which(names(mr_zwk) == mr_rsrp_feat)] = "RSRP0"
names(mr_zwk)[which(names(mr_zwk) == mr_carrier_number_feat)] = "carrier_nb0"
rm(mr_eci_feat, mr_rsrp_feat, mr_carrier_number_feat)

for(i in 1:nb_neighbor_eci) {
  names(mr_zwk)[which(names(mr_zwk) == mr_eci_feat_neighbors[i])] = paste0("ECI", i)
  names(mr_zwk)[which(names(mr_zwk) == mr_rsrp_feat_neighbors[i])] = paste0("RSRP", i)
  names(mr_zwk)[which(names(mr_zwk) == mr_carrier_number_neighbor_feat[i])] = paste0("carrier_nb", i)
}
rm(i, mr_eci_feat_neighbors, mr_rsrp_feat_neighbors, mr_carrier_number_neighbor_feat)

# Rename longitude and latitude to prevent feature conflict after merging
names(mr_zwk)[which(names(mr_zwk) == mr_longitude_feat)] = "经度_user"
names(mr_zwk)[which(names(mr_zwk) == mr_latitude_feat)] = "纬度_user"
rm(mr_longitude_feat, mr_latitude_feat)
##########################################################################################################



##########################################################################################################
#################################### 5. Merging MR with cellinfo to form df ##############################
df = mr_zwk

## Take all cellinfo, excepting special_feat columns
special_feat = c("helu", "Enodeb_id_fre", "机械下倾角")
cellinfo_base = cellinfo[,which(!names(cellinfo) %in% special_feat)]
rm(special_feat)

## Adding columns in df, according to cellinfo information
for(i in 0:nb_neighbor_eci) {
  # Rename this cell info 
  cellinfo_i = cellinfo_base
  names(cellinfo_i) = paste0(names(cellinfo_base), i)
  df = merge(df, cellinfo_i, by=paste0("ECI", i), all.x=TRUE)
}

## Sorting columns of df
# names(df)
df = df[c(paste0("ECI", 0:nb_neighbor_eci),
          paste0("RSRP", 0:nb_neighbor_eci),
          "经度_user",
          paste0("经度", 0:nb_neighbor_eci),
          "纬度_user",
          paste0("纬度", 0:nb_neighbor_eci),
          paste0("carrier_nb", 0:nb_neighbor_eci),
          paste0("天线挂高", 0:nb_neighbor_eci),
          paste0("天线增益", 0:nb_neighbor_eci),
          paste0("方位角", 0:nb_neighbor_eci),
          paste0("下倾角", 0:nb_neighbor_eci),
          paste0("水平波束宽度", 0:nb_neighbor_eci),
          paste0("垂直波束宽度", 0:nb_neighbor_eci)
)]

rm(mr_zwk, cellinfo_base, cellinfo_i, i)
#############################################################################################################



#############################################################################################################
##################################### 6. Adding telecom variables in df #####################################
## 计算水平角 垂直角 方位角 下倾角 天线增益
latitude_base = df[["纬度_user"]]
longitude_base = df[["经度_user"]]
for(i in c(0:nb_neighbor_eci)) {
  ### 无邻区,有邻区但邻区 不在调优范围,室内ott经纬度与小区经纬度相同
  longitude_current = df[[paste0("经度", i)]]
  latitude_current = df[[paste0("纬度", i)]]
  
  ### 初始化
  df[[paste0("horizontal_angle", i)]] = NA
  df[[paste0("delta_azimuth", i)]] = NA
  df[[paste0("vertical_angle", i)]] = NA
  df[[paste0("delta_tilt", i)]] = NA
  df[[paste0("dis_m_u", i)]] = NA
  
  ### Select elements for which angle can be defined,
  # i.e. where longitude_current != NA
  #            latitude_base != latitude_current 
  #            longitude_base != longitude_current
  idx = which(!is.na(longitude_current) & (latitude_base != latitude_current))
  
  ### Horizontal angle 计算水平角 用于计算方位角
  # Horizontal 包括 %二象限+360 第三四象限及Y轴负半轴+180
  df[[paste0("horizontal_angle", i)]][idx] = 
    horizontal_angle_func(longitude_base[idx], 
                          longitude_current[idx], 
                          latitude_base[idx], 
                          latitude_current[idx])
  
  # dis_m_u is only used to deduce vertical_angle dis_m_U是临时变量 仅仅用来计算垂直角
  # geoDist is a SoDA function
  # dis_m_u is the geodetic distance in meters between corresponding points
  df[[paste0("dis_m_u", i)]][idx] = 
    GetDistance(latitude_current[idx],
            longitude_current[idx],
            latitude_base[idx],
            longitude_base[idx])
  
  ### Vertical angle 计算垂直角 用于计算下倾角
  df[[paste0("vertical_angle", i)]][idx] = 
    vertical_angle_func(df[[paste0("天线挂高",i)]][idx], 
                        df[[paste0("dis_m_u",i)]][idx])
  
  ### Delta azimuth 计算方位角
  #df$方位角索引cell info中小区azimuth
  # 计算夹角，向下取整，由于方向图基本对称，可以使用对称的方式计算增益所以使用绝对值
  df[[paste0("delta_azimuth", i)]][idx] = 
    floor_diff(df[[paste0("方位角", i)]][idx], 
               df[[paste0("horizontal_angle", i)]][idx])
  df[[paste0("delta_azimuth", i)]] = replace_na(df[[paste0("delta_azimuth", i)]],
                                                by = def_delta_azimuth)
  
  ### Delta tilt 计算下倾角
  df[[paste0("delta_tilt", i)]][idx] = 
    floor_diff(df[[paste0("下倾角", i)]][idx],
               df[[paste0("vertical_angle",i)]][idx])
  df[[paste0("delta_tilt", i)]] = replace_na(df[[paste0("delta_tilt", i)]],
                                             by = def_delta_tilt)
  
  ### Replace NA caused by merging with missing ECI (not the same with 0 values)
  df[[paste0("水平波束宽度", i)]] = replace_na(df[[paste0("水平波束宽度", i)]],
                                         by = def_horizontal_beam)
  df[[paste0("垂直波束宽度", i)]] = replace_na(df[[paste0("垂直波束宽度", i)]],
                                         by = def_vertical_beam)
  df[[paste0("天线增益", i)]] = replace_na(df[[paste0("天线增益", i)]],
                                       by = def_gain_antenna)
  
  ### Gain 天线增益
  df[[paste0("gain", i)]] =  
    funcgain(df[[paste0("delta_azimuth", i)]],
             df[[paste0("delta_tilt", i)]], 
             df[[paste0("水平波束宽度", i)]], # contain NA
             df[[paste0("垂直波束宽度", i)]], # contain NA
             df[[paste0("天线增益", i)]]) # contain NA
}
rm(i, idx, longitude_base, latitude_base, longitude_current, latitude_current)
rm(def_horizontal_beam, def_vertical_beam, def_gain_antenna)



#############################################################################################
####################### 7. 方位角采样点覆盖范围限定在主波瓣 [-90,90] 范围内 #################
#####################################################
t_azimuth = which(abs(df[["delta_azimuth0"]]) <= 90)
df = df[t_azimuth,]
rm(t_azimuth)



#############################################################################################
################# 8. Keep only useful features for df and cellinfo #########################
## Keep data frames:
# - cellinfo
# - df
## Reduce cellinfo to useful features
cellinfo = cellinfo[c("ECI",
                      "下倾角",
                      "机械下倾角",
                      "方位角",
                      "Enodeb_id_fre",
                      "helu")]

## Reduce df to useful features
df = df[c(paste0("ECI", 0:nb_neighbor_eci),
          paste0("horizontal_angle", 0:nb_neighbor_eci),
          paste0("vertical_angle", 0:nb_neighbor_eci),
          paste0("RSRP", 0:nb_neighbor_eci),
          paste0("gain", 0:nb_neighbor_eci),
          paste0("carrier_nb", 0:nb_neighbor_eci),
          paste0("水平波束宽度", 0:nb_neighbor_eci),
          paste0("垂直波束宽度", 0:nb_neighbor_eci),
          paste0("天线增益", 0:nb_neighbor_eci))]
df = as.data.table(df) # data.table is more efficient

################################
################################
## B - Evolutionary algorithm ##
################################
################################

##############
# Parameters #
##############

## Number of particles 
# 50 initially; NA for #parameters*10
#NP = 120 群体中个体的总数
 NP = 2*length(problem_cell)*10
## Number of iterations
# 60 initially
itermax = 60

## Strategy
# 2 for default strategy; 1 can also be tested
strategy = 2

## Parallel
# 1 for non parallel computations; nb_clusters usually between 1 and 7.
nb_clusters = 1

## Seed
seed = 1234
?DEoptim
###################
# 1. Tuning range #
###################
#problem_cell = problem_cell[1:5] # only for testing purpose!
# Define range of adjustment for horizontal and vertical angles.
wminmax = wminmax_func(cellinfo, problem_cell)
# print(wminmax)

#########################
# 2. Control parameters #
#########################
## More documentation with command ?DEoptim.control
# We have a cost function f() and we want to decrease it.
if(nb_clusters == 1) {
  ##
  # Non parallel version
  ##
  control_param = DEoptim.control(
    VTR = -Inf, # value to be reached
    strategy = strategy, # which strategy for optimization, see below
    bs = FALSE,
    ## Population size NP: NA corresponds to default value, i.e.
    # population size set to 10 times length of param vector
    NP = NP,
    itermax = itermax, # number of iterations
    CR = 0.6, # crossover probability from interval [0,1]
    F = 0.8, # differential weighting factor from interval [0,2]
    trace = TRUE, # printing progress of evolution?
    ## storepopfrom:
    ## if we want to store each population, set storepopfrom to 1
    ## if we do not want to store, set storepopfrom to itermax + 1
    storepopfrom = 1 # itermax + 1
  )
} else {
  ##
  # Parallel version
  ##
  cl = makeCluster(nb_clusters)
  control_param = DEoptim.control(
    VTR = -Inf, # value to be reached
    strategy = strategy, # which strategy for optimization, see below
    bs = FALSE,
    ## Population size NP: NA corresponds to default value, i.e.
    # population size set to 10 times length of param vector
    NP = NP,
    itermax = itermax, # number of iterations
    CR = 0.6, # crossover probability from interval [0,1]
    F = 0.8, # differential weighting factor from interval [0,2]
    trace = TRUE, # printing progress of evolution?
    ## storepopfrom:
    ## if we want to store each population, set storepopfrom to 1
    ## if we do not want to store, set storepopfrom to itermax + 1
    storepopfrom = 1, # itermax + 1
    parallelType = 1,
    cluster = cl,
    packages=c("data.table", "dplyr"),
    parVar=c("cellinfo",
             "df",
             'cost0',
             'update_cellinfo',
             "delta_frequency_func",
             "problem_cell",
             "update_df",
             "nb_neighbor_eci",
             "floor_diff",
             "funcgain",
             "replace_na",
             "%>%",
             "group_by",
             "summarize",
             "data.table",
             "def_delta_azimuth",
             "def_delta_tilt",
             "cost_from_df_updated",
             "cost0_new",
             "update_cellinfo_new",
             "check_infinite_cost_new",
             "rounding_param"
    )
  )
}

################
# 3. Evolution #
################
set.seed(seed)
# Rprof("log.txt")
start.time = Sys.time()
print(start.time)
est.ras <- DEoptim(cost_new2, # function to optimize 第一个参数是待优化参数 根据下面的lower和upper确定 这里即所有问题小区的两个角度
                   lower=wminmax$wmin, # lower bound for each parameter
                   upper=wminmax$wmax, # higher bound for each parameter
                   control = control_param)

end.time = Sys.time()
print(end.time)
time.taken = end.time - start.time
print(time.taken)
# Rprof(NULL)
# summaryRprof("log.txt")$by.total[1:8,]

#################
#################
## C - Results ##
#################
#################
print(cost_new2(rep(0, nrow(wminmax))))
print(est.ras$optim$bestmem)
# ## Best member = best parameters x = (x1, x2) found:
# cat(est.ras$optim$bestmem)
if(cost_new2(rep(0, nrow(wminmax)))>cost_new2(rounding_param(est.ras$optim$bestmem))){
  # result_fl<-matrix(0, nrow = length(problem_cell), ncol = 3, byrow = TRUE)
  result_fl<-matrix(0, nrow = floor(length(est.ras$optim$bestmem)/2), ncol = 3, byrow = TRUE)
  result_fl<-as.data.frame(result_fl)
  colnames(result_fl)<-c("ci","azimuth_calc_range","down_tilt_calc_range")
  # result_fl$ci<-problem_cell
  result_fl$ci<-wminmax$eci[1:floor(length(est.ras$optim$bestmem)/2)]
  #写入数据
  result_fl$azimuth_calc_range<-rounding_param(est.ras$optim$bestmem)[1:floor(length(est.ras$optim$bestmem)/2)]
  result_fl$down_tilt_calc_range<- rounding_param(est.ras$optim$bestmem)[(floor(length(est.ras$optim$bestmem)/2)+1):length(est.ras$optim$bestmem)]
 }else{
   # result_orig<-cbind(est.ras$optim$bestmem[1:floor(length(est.ras$optim$bestmem)/2)],est.ras$optim$bestmem[(floor(length(est.ras$optim$bestmem)/2)+1):length(est.ras$optim$bestmem)]) 
   result_fl<-matrix(0, nrow = length(problem_cell), ncol = 3, byrow = TRUE)
   result_fl<-as.data.frame(result_fl)
   colnames(result_fl)<-c("ci","azimuth_calc_range","down_tilt_calc_range")
   result_fl$ci<-problem_cell
  
  

 }
