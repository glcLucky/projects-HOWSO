##
# Give the range of each parameter
##
# Output is a data frame with 4 columns:
# - eci,
# - type (horizontal or vertical parameter),
# - wmin,
# - wmax
wminmax_func = function(cellinfo, problem_cell) {
  ##############
  # Preprocess #
  ##############
  ## Get problem cells which are really in cell info
  index_problem = which(cellinfo[["ECI"]] %in% problem_cell)
  # Reorder problem_cell if necessary
  problem_cell = cellinfo[["ECI"]][index_problem]
  len = length(problem_cell)
  
  ####################
  # Horizontal angle #
  ####################
  # min
  wmin_horizontal = rep(-60, len)
  # max
  wmax_horizontal = rep(60, len)  
  
  ##################
  # Vertical angle #
  ##################
  ## Init range of vertical angle that will be completed
  # min
  wmin_vertical = rep(NA, len)
  # max
  wmax_vertical = rep(NA, len)
  
  ##
  # Get 下倾角 and 机械下倾角 for problematic cells
  ##
  down_dip_problem = cellinfo[["下倾角"]][index_problem]
  tilt_problem = cellinfo[["机械下倾角"]][index_problem]
  
  ##
  # Define limit between small down dip and large down dip
  ##
  down_dip_limit = 12
  
  ##
  # Large down dip fill
  ##
  idx_large = down_dip_problem > down_dip_limit
  tilt1_values = tilt_problem[idx_large]
  wmin_vertical[idx_large] = max(-tilt1_values, -5)
  wmax_vertical[idx_large] = 0
  
  ##
  # Small down dip fill
  ##
  tilt2_values = tilt_problem[!idx_large]
  wmin_vertical[!idx_large] = pmax(-tilt2_values, -5)
  wmax_vertical[!idx_large] = down_dip_limit - tilt2_values
  
  ############################################
  # Output data frame of range of parameters #
  ############################################
  out = data.frame(eci = c(problem_cell, problem_cell),
                   type = c(rep("horizontal", len), rep("vertical", len)),
                   wmin = c(wmin_horizontal, wmin_vertical),
                   wmax = c(wmax_horizontal, wmax_vertical))
  
  return(out)
}

##
# Global function to compute cost
##
# If m is the number of problem cells, the number of parameters is 2*m
# param := (delta_horizontal_1, ... delta_horizontal_m; delta_vertical_1, ... delta_vertical_m)
# No change corresponds to param = c(0, 0, 0, ..., 0)

# cost = function(param) {
#   cost0(param,
#            problem_cell,
#            cellinfo,
#            df,
#            nb_neighbor_eci,
#            def_delta_azimuth,
#            def_delta_tilt)
# }

rounding_param = function(param) {
  ##
  # Rounding parameters as needed
  ##
  # 1- Horizontal angle is rounded to be in: 
  # -60, -55, -50, ..., -10, -5, 0, 5, ...
  #
  # 2- Vertical angle is rounded to be in:
  # -6, -5, -4, -3, -2, 0, 2, 3, 4, 5, 6
  #
  #param = rep(0, nrow(wminmax))
  #param = rnorm(nrow(wminmax), 10, 10)
  idx_horizontal = 1:(length(param)/2)
  idx_vertical = (1+length(param)/2):length(param)
  param[idx_horizontal] = round(2*(param[idx_horizontal]), -1)/2
  param[idx_vertical] = round(param[idx_vertical])
  idx_toreinit = which(param[idx_vertical] %in% c(-1, 1))
  if(length(idx_toreinit) > 0) {
    param[idx_vertical][idx_toreinit] = 0
  }
  return(param)
}

cost_new = function(param) {
  param = rounding_param(param)
  ## problem_cell更改为全部小区，param也更改为全部的param。
  param = same_prama(idx_for_DE,problem_cell_all,param,df_idx)
  cost0_new(param,
            problem_cell_all,
            cellinfo,
            df,
            nb_neighbor_eci,
            def_delta_azimuth,
            def_delta_tilt)
}

# cost_new2 = function(param) {
#   param = rounding_param(param)
#   cost0(param,
#             problem_cell,
#             cellinfo,
#             df,
#             nb_neighbor_eci,
#             def_delta_azimuth,
#             def_delta_tilt)
# }
# ##
# # Cost function
# ##
# cost0 = function(param,
#                  problem_cell,
#                  cellinfo,
#                  df,
#                  nb_neighbor_eci,
#                  def_delta_azimuth,
#                  def_delta_tilt) {
#   ##########
#   # Part I #
#   ##########
#   # 小区信息表 cellinfo
#   # 天线的方位角偏移量设计
#   # 天线的下倾角偏移量设计
#   # 加入1~9各邻区方位角/下倾角的调整值的信息表df1, 使用后其为内部变量会自动注销
#   cellinfo = update_cellinfo(cellinfo, 
#                              param,
#                              problem_cell)
#   ## Keep 3 columns in cellinfo
#   cellinfo_base = cellinfo[c("ECI", "azimuth.adjustment", "tilt.adjustment")]
#   
#   ###########
#   # Part II #
#   ###########
#   # 85% of time cost
#   df_updated = update_df(df, 
#                          cellinfo_base, 
#                          nb_neighbor_eci,
#                          def_delta_azimuth,
#                          def_delta_tilt)
#   
#   ############
#   # Part III #
#   ############
#   df_updated[["delta_overlap"]] = delta_frequency_func(df_updated, nb_neighbor_eci)
#   
#   ###########
#   # Part IV #
#   ###########
#   ## Compute the cost as the sum of cost caused by weak cover and overlap
#   # for both problem cell and other cells.
#   z = cost_from_df_updated(df_updated, problem_cell)
#   return(z)
# }

cost0_new = function(param,
                     problem_cell_all,
                     cellinfo,
                     df,
                     nb_neighbor_eci,
                     def_delta_azimuth,
                     def_delta_tilt) {
  ##########
  # Part I #
  ##########
  # 小区信息表 cellinfo
  # 天线的方位角偏移量设计
  # 天线的下倾角偏移量设计
  # 加入1~9各邻区方位角/下倾角的调整值的信息表df1, 使用后其为内部变量会自动注销
  cellinfo = update_cellinfo_new(cellinfo, 
                                 param,
                                 problem_cell_all)
  # is_infinite = check_infinite_cost_new(cellinfo, param, problem_cell)
  # if(is_infinite) {
  #   return(+Inf)
  # }
  
  ## Keep 3 columns in cellinfo
  cellinfo_base = cellinfo[c("ECI", "azimuth.adjustment", "tilt.adjustment")]
  
  
    
  ###########
  # Part II #
  ###########
  # 85% of time cost
  df_updated = update_df(df, 
                         cellinfo_base, 
                         nb_neighbor_eci,
                         def_delta_azimuth,
                         def_delta_tilt)
  
  ############
  # Part III #
  ############
  df_updated[["delta_overlap"]] = delta_frequency_func(df_updated, nb_neighbor_eci)
  
  ###########
  # Part IV #
  ###########
  ## Compute the cost as the sum of cost caused by weak cover and overlap
  # for both problem cell and other cells.
  z = cost_from_df_updated(df_updated, problem_cell_all)
  return(z)
}

#############################
# Part I : Update cellinfo 2#
#############################
update_cellinfo_new = function(cellinfo,
                               param,
                               problem_cell_all) {
  ##
  # myvector is the change of angle for all selected cells
  ##
  # param is the change of angle for all problematic cells
  # myvector is 0 for selected_cell \ problem_cells
  #          is param for problem_cells
  size = nrow(cellinfo)
  myvector = rep(0, 2*size)
  index_problem = which(cellinfo[["ECI"]] %in% problem_cell_all)
  myvector[c(index_problem, index_problem + size)] = param
  
  ##
  # Updating cellinfo by adding azimuth.adjustment and tilt.adjustment
  ##
  # azimuth.adjustment is the new value for horizontal angle,
  # tilt.adjustment is the new value for vertical angle
  cellinfo[["azimuth.adjustment"]] = floor(cellinfo[["方位角"]] + myvector[1:size]) %% 360
  cellinfo[["tilt.adjustment"]] = floor(cellinfo[["下倾角"]] + myvector[(size+1):(2*size)])
  
  return(cellinfo)
}

################################
# Part I : Check infinite cost #
################################
# Function is sloppy, but it does not cost much computation time
# check_infinite_cost_new = function(cellinfo,
#                            param,
#                            problem_cell) {
#   infinite = FALSE
#   
#   ##
#   # Change azimuth.adjustment and tilt.adjustment based on Enodeb_id_fre and helu
#   ##
#   # Enode:
#   # 同天线的小区方位角调整后不能改变小区分区( 频段指示.param   Enodeb_id )
#   # 加入约束
#   # 若不满足则方位角不调动/保持不变
#   # Helu:
#   # 合路情况-- 若 天线挂高 Enodeb_id 下倾角 方位角 一致的小区
#   # 则其方位角/下倾角同时一样调整   用其最大值
#   # Grid helu (此连接语句可放在函数外面)
#   
#   ##
#   # Cryptic code. Raise a "!!!" print if it changes anything.
#   ##
#   grid_id_fre = unique(cellinfo[["Enodeb_id_fre"]])
#   for(k in 1:length(grid_id_fre)) {
#     ## Cells with this enodeb_id_fre
#     idx = which(cellinfo[["Enodeb_id_fre"]] == grid_id_fre[k])
#     len = length(idx)
#     
#     # sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE) > 0
#     # ==>
#     # sum(cellinfo[["ECI"]][idx] %in% problem_cell, na.rm = TRUE) > 0
#     # nb_problem_eci = sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE)
#     nb_problem_eci = sum(cellinfo[["ECI"]][idx] %in% problem_cell, na.rm = TRUE)
#     if(len > 1 && nb_problem_eci > 0) {
#       for(i in 1:(len-1)) {
#         cellinfo_azimuth_adj_i = cellinfo[["azimuth.adjustment"]][idx[i]]
#         cellinfo_azimuth_adj_after = cellinfo[["azimuth.adjustment"]][idx[(i+1):len]]
#         abs_diff_adj_i_and_after = abs(cellinfo_azimuth_adj_i - cellinfo_azimuth_adj_after)
#         ##
#         # Count how many outside [50, 360-50]
#         ##
#         #          50      360-50
#         # 0--------]//////////[--------360
#         # print(cellinfo[["azimuth.adjustment"]][idx])
#         if(sum(is.na(cellinfo[["azimuth.adjustment"]][idx]) >= 1)) {
#           print(paste0("Please remove those problem cells:"))
#           print(cellinfo[["ECI"]][idx])
#           stop("There is at least one cell with unknown original azimuth!")
#         }
#         
#         nb_out = sum(abs_diff_adj_i_and_after < 50 | abs_diff_adj_i_and_after > 360 - 50)
# 
#         if(nb_out > 1) {
#           print("Enodebfre problem")
#           print(cellinfo[["Enodeb_id_fre"]][idx])
#           print(cellinfo[["azimuth.adjustment"]][idx])
#           print(cellinfo[["ECI"]][idx])
#           print(nb_out)
#           infinite = TRUE
#         }
#       }
#     }
#   }
#   
#   ##
#   # Cryptic code 2. Raise a "!!!" print if it changes anything.
#   ##
#   # grid_helu = unique(cellinfo[["helu"]])
#   # if(length(grid_helu) < length(cellinfo[["helu"]])) {
#   #   for(k in 1:length(grid_helu)) {
#   #     idx = which(cellinfo[["helu"]] == grid_helu[k])
#   #     len = length(idx)
#   #     #方位角 下倾角一起同角度调整
#   #     
#   #     # sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE) > 0
#   #     # ==>
#   #     # sum(cellinfo[["ECI"]][idx] %in% problem_cell, na.rm = TRUE) > 0
#   #     # nb_problem_eci = sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE)
#   #     nb_problem_eci = sum(cellinfo[["ECI"]][idx] %in% problem_cell, na.rm = TRUE)
#   #     if(len > 1 && nb_problem_eci > 0) {
#   #       #print(cellinfo[["ECI"]][idx[1]])
#   #       #print(paste(k, "Helu problem in cellinfo"))
#   #       #infinite = TRUE
#   #     }
#   #   }
#   # }
#   return(infinite)
# }

# ############################
# # Part I : Update cellinfo #
# ############################
# # Function is sloppy, but it does not cost much computation time
# update_cellinfo = function(cellinfo,
#                            param,
#                            problem_cell,
#                            output_param = FALSE) {
#   ##
#   # myvector is the change of angle for all selected cells
#   ##
#   # param is the change of angle for all problematic cells
#   # myvector is 0 for selected_cell \ problem_cells
#   #          is param for problem_cells
#   size = nrow(cellinfo)
#   myvector = rep(0, 2*size)
#   index_problem = which(cellinfo[["ECI"]] %in% problem_cell)
#   myvector[c(index_problem, index_problem + size)] = param
# 
#   ##
#   # Updating cellinfo by adding azimuth.adjustment and tilt.adjustment
#   ##
#   # azimuth.adjustment is the new value for horizontal angle,
#   # tilt.adjustment is the new value for vertical angle
#   cellinfo[["azimuth.adjustment"]] = floor(cellinfo[["方位角"]] + myvector[1:size]) %% 360
#   cellinfo[["tilt.adjustment"]] = floor(cellinfo[["下倾角"]] + myvector[(size+1):(2*size)])
# 
#   ##
#   # Change azimuth.adjustment and tilt.adjustment based on Enodeb_id_fre and helu
#   ##
#   # Enode:
#   # 同天线的小区方位角调整后不能改变小区分区( 频段指示.param   Enodeb_id )
#   # 加入约束
#   # 若不满足则方位角不调动/保持不变
#   # Helu:
#   # 合路情况-- 若 天线挂高 Enodeb_id 下倾角 方位角 一致的小区
#   # 则其方位角/下倾角同时一样调整   用其最大值
#   # Grid helu (此连接语句可放在函数外面)
# 
#   ##
#   # Cryptic code. Raise a "!!!" print if it changes anything.
#   ##
#   grid_id_fre = unique(cellinfo[["Enodeb_id_fre"]])
#   for(k in 1:length(grid_id_fre)) {
#     ## Cells with this enodeb_id_fre
#     idx = which(cellinfo[["Enodeb_id_fre"]] == grid_id_fre[k])
#     len = length(idx)
# 
#     # sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE) > 0
#     # ==>
#     # sum(cellinfo[["ECI"]][idx] %in% problem_cell, na.rm = TRUE) > 0
#     nb_problem_eci = sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE)
#     if(len > 1 && nb_problem_eci > 0) {
#       for(i in 1:(len-1)) {
#         cellinfo_azimuth_adj_i = cellinfo[["azimuth.adjustment"]][idx[i]]
#         cellinfo_azimuth_adj_after = cellinfo[["azimuth.adjustment"]][idx[(i+1):len]]
#         abs_diff_adj_i_and_after = abs(cellinfo_azimuth_adj_i - cellinfo_azimuth_adj_after)
#         ##
#         # Count how many outside [50, 360-50]
#         ##
#         #          50      360-50
#         # 0--------]//////////[--------360
#         nb_out = sum(abs_diff_adj_i_and_after < 50 |
#                        abs_diff_adj_i_and_after > 360 - 50)
# 
#         if(nb_out > 1) { #?
#           if(output_param) {
#             print(paste(k, i, "!!! changing parameters with Enodeb_id_fre!"))
#           }
#           
#           cellinfo[["azimuth.adjustment"]][idx] = cellinfo[["方位角"]][idx]
#           myvector[idx] = 0
#           break() #?
#         }
#       }
#     }
#   }
# 
#   ##
#   # Cryptic code 2. Raise a "!!!" print if it changes anything.
#   ##
#   grid_helu = unique(cellinfo[["helu"]])
#   if(length(grid_helu) < length(cellinfo[["helu"]])) {
#     for(k in 1:length(grid_helu)) {
#       idx = which(cellinfo[["helu"]] == grid_helu[k])
#       len = length(idx)
#       #方位角 下倾角一起同角度调整
# 
#       # sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE) > 0
#       # ==>
#       # sum(cellinfo[["ECI"]][idx] %in% problem_cell, na.rm = TRUE) > 0
#       nb_problem_eci = sum(cellinfo[["ECI"]][idx[1]] == problem_cell, na.rm = TRUE)
#       if(len > 1 && nb_problem_eci > 0) {
#         
#         if(output_param) {
#           print(paste(k, "!!! changing parameters with helu!"))
#         }
# 
#         rr1 = which.max(abs(myvector[idx]))
#         cellinfo[["azimuth.adjustment"]][idx] = rep(cellinfo[["azimuth.adjustment"]][idx][rr1], len)
#         myvector[idx]= rep(myvector[idx][rr1], len)
# 
#         rr2 = which.max(abs(myvector[idx+size]))
#         cellinfo[["tilt.adjustment"]][idx] = rep(cellinfo[["tilt.adjustment"]][idx][rr2], len)
#         myvector[idx+size] = rep(myvector[idx+size][rr2], len)
#       }
#     }
#   }
#   if(output_param) {
#     cat(myvector[c(index_problem, index_problem + size)])
#   }
#   
#   return(cellinfo)
# }

#######################
# Part II : Update df #
#######################
update_df = function(df, 
                     cellinfo_base, 
                     nb_neighbor_eci,
                     def_delta_azimuth,
                     def_delta_tilt) {
  ## Adding columns in df, according to cellinfo information
  for(i in 0:nb_neighbor_eci) {
    # Rename this cell info 
    cellinfo_i = cellinfo_base
    names(cellinfo_i) = paste0(names(cellinfo_base), i)
    df = merge(df, cellinfo_i, by=paste0("ECI", i), all.x = TRUE, sort = FALSE)
  }

  #df$方位角 索引cell info中小区方位角
  #计算夹角，向下取整，由于方向图基本对称，可以使用对称的方式计算增益所以使用绝对值
  #%调整后的RSRP(dBm)
  for(i in 0:nb_neighbor_eci) {
    #print(i)
    df[[paste0("delta_azimuthAdjust", i)]] = floor_diff(df[[paste0("azimuth.adjustment", i)]], df[[paste0("horizontal_angle", i)]])
    df[[paste0("delta_azimuthAdjust", i)]] = replace_na(df[[paste0("delta_azimuthAdjust", i)]],
                                                        by = def_delta_azimuth)
    
    df[[paste0("delta_tiltAdjust", i)]] = floor_diff(df[[paste0("tilt.adjustment", i)]], df[[paste0("vertical_angle", i)]])
    df[[paste0("delta_tiltAdjust", i)]] = replace_na(df[[paste0("delta_tiltAdjust", i)]],
                                                     by = def_delta_tilt)
    
    df[[paste0("gainAdjust", i)]] =  funcgain(df[[paste0("delta_azimuthAdjust", i)]],
                                              df[[paste0("delta_tiltAdjust", i)]],
                                              df[[paste0("水平波束宽度", i)]],
                                              df[[paste0("垂直波束宽度", i)]],
                                              df[[paste0("天线增益", i)]])
    df[[paste0("after_rsrp", i)]] = df[[paste0("RSRP", i)]] + df[[paste0("gainAdjust", i)]] - df[[paste0("gain", i)]]
  }
  
  df = df[, c("ECI0",
              paste0("carrier_nb", 0:nb_neighbor_eci),
              paste0("after_rsrp", 0:nb_neighbor_eci)), with = FALSE]
  
  return(df)
}

####################################
# Part III : Compute delta overlap #
####################################
delta_frequency_func = function(df_updated, nb_neighbor_eci) {
  ## Which rows have RSRP_i - RSRP_0 > -6, for each neighbor cell i?
  df_rsrp = df_updated[, paste0("after_rsrp", 1:nb_neighbor_eci), with = FALSE]
  df_rsrp0 = df_updated[, paste0("after_rsrp", 0), with = FALSE][[1]]
  df_rsrp_diff_bool = df_rsrp - df_rsrp0 > -6
  
  ## Which rows have carrier_i == carrier_0, for each neighbor cell i?
  df_carrier0 = df_updated[, paste0("carrier_nb", 0), with = FALSE][[1]]
  df_carrier = df_updated[, paste0("carrier_nb", 1:nb_neighbor_eci), with = FALSE]
  df_carrier_same = df_carrier == df_carrier0
  
  ## Which rows have those 2 together
  matrix_index_equal = df_rsrp_diff_bool * df_carrier_same
  
  ## Take the sum, i.e. the number of times there is 
  # RSRP_i - RSRP_0 > -6 and carrier_i == carrier_0
  # for i from 1 to nb_neighbor_eci
  delta = rowSums(matrix_index_equal, na.rm = TRUE)
  return(delta)
}

##########################
# Part IV : Compute cost #
##########################
cost_from_df_updated = function(df_updated, problem_cell_all) {
  df_summarize = df_updated %>%
    group_by(ECI0) %>%
    summarize(sum_cnt = n(),
              # 以下为全局评估函数：弱覆盖小区数+重叠覆盖小区数+各小区弱覆盖率+各小区重叠覆盖率
              bad_cnt_weak_cover = sum(after_rsrp0 < (-110)),
              # 重叠覆盖条件2,3
              bad_cnt_overlap = sum((after_rsrp0 >= -110) & (delta_overlap >= 3)),
              weak_cover_rate = bad_cnt_weak_cover / sum_cnt,
              overlap_rate = bad_cnt_overlap / sum_cnt)
  
  ##　problem_cell 中的cell_type = 1的小区要找到，给予更高的权重
  
  index_pb = which(df_summarize$ECI0 %in% problem_cell_all)
  sum_cnt = df_summarize$sum_cnt
  alpha_cnt = ifelse(sum_cnt > 500, 1, sum_cnt/500)[index_pb]
  
  ##
  # Increase cost if there is large proportion of weak cover for problem cells
  ##
  weak_cover_rate = df_summarize$weak_cover_rate[index_pb]
  a1 = sum((alpha_cnt*weak_cover_rate)[weak_cover_rate >= 0.1], na.rm=TRUE)  
  a2 = sum(10*((alpha_cnt*weak_cover_rate)[weak_cover_rate < 0.1])^2, na.rm=TRUE)
  
  ##
  # Increase cost if there is large proportion of overlap for problem cells
  ##
  overlap_rate = df_summarize$overlap_rate[index_pb]
  b1 = 2*sum((alpha_cnt*overlap_rate)[(2*overlap_rate) >= 0.1], na.rm=TRUE)   
  b2 = sum(10*2*((alpha_cnt*overlap_rate)[(2*overlap_rate) < 0.1])^2, na.rm=TRUE)
  
  ##
  # Increase cost if there is large proportion of weak cover for non problem cells
  ##
  bad_cnt_weak_cover = df_summarize$bad_cnt_weak_cover[-index_pb]
  na1 = sum(bad_cnt_weak_cover) / sum(sum_cnt[-index_pb])
  
  ##
  # Increase cost if there is large proportion of overlap for non problem cells
  ##
  bad_cnt_overlap = df_summarize$bad_cnt_overlap[-index_pb]
  nb2 = 2*sum(bad_cnt_overlap) / sum(sum_cnt[-index_pb])
  
  ##
  # Global cost as a sum
  ##
  ## 可以在这里再加个权重。
  z = sum(c(a1, a2, b1, b2, na1, nb2), na.rm=TRUE)
  #print(c(a1, a2, b1, b2, na1, nb2))
  return(z)
}

## add function about same_antena_tune_same_angle ##

same_prama = function(idx_for_DE,problem_cell_all,param,df_idx){
  if (length(problem_cell_all) > length(idx_for_DE)){
    pcell = data.frame('eci'= problem_cell_all)
    pcell$param_a = 0
    pcell$param_t = 0
    pcell$param_a[idx_for_DE] = param[1:length(idx_for_DE)]
    pcell$param_t[idx_for_DE] = param[(length(idx_for_DE)+1):length(param)]
    for (i in (1:nrow(df_idx))){
      if (grepl(',',df_idx$idx2[i])){
        # print(i)
        # print(is.na(as.numeric(df_idx$idx2[i])))
        # print(as.numeric(df_idx$idx2[i]))
        # print(as.numeric(df_idx$idx1[i]))
        # print(unlist(strsplit(df_idx$idx2[i],',')))
        len_i = unlist(strsplit(df_idx$idx2[i],','))
        for (j in (1:length(len_i))){
          # print(j)
          # print(len_i[j])
          # print(as.numeric(df_idx$idx1[i]))
          # print(pcell$param_a[as.numeric(df_idx$idx1[i])])
          pcell$param_a[as.numeric(len_i[j])] = pcell$param_a[as.numeric(df_idx$idx1[i])]
          pcell$param_t[as.numeric(len_i[j])] = pcell$param_t[as.numeric(df_idx$idx1[i])]
        }
      }else{
        pcell$param_a[as.numeric(df_idx$idx2[i])] = pcell$param_a[as.numeric(df_idx$idx1[i])]
        pcell$param_t[as.numeric(df_idx$idx2[i])] = pcell$param_t[as.numeric(df_idx$idx1[i])]
      }
      
    }
    param = c(pcell$param_a,pcell$param_t)
  }else{
    param = param
  }
  
  return(param)
  # pcell$param[as.numeric(df_idx$idx2[3])] = pcell$param[as.numeric(df_idx$idx1[1])]
}













