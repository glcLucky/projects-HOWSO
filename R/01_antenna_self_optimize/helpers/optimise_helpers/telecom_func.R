##
# Floor of absolute difference between vectors x and y
##
floor_diff = function(x, y) {
  out = floor(abs(x - y))
  idx_modulo = which(out > 180)
  if(length(idx_modulo) > 0) {
    out[idx_modulo] = abs(out[idx_modulo] - 360)
  }
  return(out)
}
# We can keep absolute values because delta_azimuth and delta_tilt are only
# used in the gain function, which only consider square of those values.
# x = c(10, 20, 30, 40)
# y = c(340, 350, 0, 10)
# floor_diff(x,y) # c(30, 30, 30, 30)

##
# Replace NA and infinite values by a fixed number
##
replace_na = function(vect, by) {
  vect[is.na(vect) | is.infinite(vect)] = by
  return(vect)
}

##
# Replace 0 by a fixed number
##
replace_0 = function(vect, by) {
  idx = which(vect == 0)
  if(length(idx) > 0) {
    vect[idx] = by
  }
  return(vect)
}

##
# Replace 0 and NA by the same fixed number
##
replace_0_and_na = function(vect, by) {
  vect = replace_0(vect, by)
  vect = replace_na(vect, by)
  return(vect)
}

##
# Compute horizontal angle between base station position and user position
##
# horizontal_angle_func = function(longitude_cell, longitude_user,
#                                  latitude_cell, latitude_user) {
#   # The 4 parameters are longitude and latitude vectors, typically
#   # - longitude will stand between 115 and 125,
#   # - latitude will stand between 25 and 27.
#   # Check horizontal_angle.png to detail of this formula
#   diff_long = longitude_user - longitude_cell
#   diff_lat = latitude_user - latitude_cell
#   out = (360/(2*pi)) * atan(cos(latitude_cell*(pi/180)) * diff_long / diff_lat)
# 
#   # modulo 360 in quadrant IV
#   t1i= ((longitude_cell < longitude_user) & (latitude_cell > latitude_user))
#   out[t1i] = out[t1i] + 360
# 
#   # modulo 180 in quadrant I and II
#   t2i= (latitude_cell <= latitude_user)
#   out[t2i] = out[t2i] + 180
#   return(out)
# }
## Shanshan version
horizontal_angle_func <- function(longitude_cell, longitude_user,
                                  latitude_cell, latitude_user){
  # The 4 parameters are longitude and latitude vectors, typically
  # - longitude will stand between 115 and 125,
  # - latitude will stand between 25 and 27.
  R=6370*1000
  dx <- (longitude_user-longitude_cell)*110000
  a1 = ((cos(latitude_cell*(pi/180)))*(cos(latitude_cell*(pi/180)))*cos((longitude_user-longitude_cell)*(pi/180))+(sin(latitude_cell*(pi/180)))*(sin(latitude_cell*(pi/180))))
  a1 = ifelse(a1>1,1,a1)
  dx1<- R*acos(a1)
  dx1 <- ifelse(dx1==0,10^(-10),dx1)
  dx1 <- ifelse(dx<0,-dx1,dx1)
  dy <- (latitude_user-latitude_cell)*110000
  dy1 <- R*acos(cos((latitude_user-latitude_cell)*(pi/180)))
  dy1 <- ifelse(dy<0,-dy1,dy1)
  sgn1 <- sign(dx1)
  angle <- round(180-90*sgn1-atan(dy1/dx1)*(180/acos(-1)),0)
  return(angle)
}
# Quadrant:
#       |
#   II  |   I
# ______|________
#       |
#  III  |  IV
#       |
## Unit tests
# Quadrant I
q1 = horizontal_angle_func(longitude_cell = 20, longitude_user = 21,
                           latitude_cell = 20, latitude_user = 21)
checkEquals(q1 > 0, TRUE)
checkEquals(q1 < 90, TRUE)
rm(q1)

# Quadrant II
q2 = horizontal_angle_func(longitude_cell = 20, longitude_user = 19,
                           latitude_cell = 20, latitude_user = 21)
checkEquals(q2 > 270, TRUE)
checkEquals(q2 < 360, TRUE)
rm(q2)

# Quadrant III
q3 = horizontal_angle_func(longitude_cell = 20, longitude_user = 19,
                           latitude_cell = 20, latitude_user = 19)
checkEquals(q3 > 180, TRUE)
checkEquals(q3 < 270, TRUE)
rm(q3)

# Quadrant IV
q4 = horizontal_angle_func(longitude_cell = 20, longitude_user = 21,
                           latitude_cell = 20, latitude_user = 19)
checkEquals(q4 > 90, TRUE)
checkEquals(q4 < 180, TRUE)
rm(q4)

##
# Compute vertical angle between base station position and user position
##
vertical_angle_func = function(antenna_height, dis_m_u, user_height = 1.5) {
  # Input in meters (for antenna_height, dis_m_u and user_height)
  # antenna_height is the height of the antenna
  # dis_m_u is the distance between antenna emission and user reception
  #
  # Output is in [-90, 90].
  # Usually, antenna height is higher than 1.5, and output angle is positive.
  #
  # == Side view == 
  # antenna
  #    *                *---         
  #    |                |\ theta     
  #  h |              h | \          
  #    |  * user        |  *
  #    |__| 1.5         |__| 1.5     
  #   dis_m_u          dis_m_u       
  # tan(theta) = opposed / adjacent = (h - 1.5) / dis_m_u when h >= 1.5
  theta = (360/(2*pi)) * atan((antenna_height - user_height) / dis_m_u)
  return(theta)
}
## Unit tests
# If user if close and antenna is high, the angle is close to 90 
checkEquals(vertical_angle_func(9999, 100) < 90, TRUE)
checkEquals(vertical_angle_func(9999, 100) > 85, TRUE)
# If the antenna is higher than user height, angle is in [0, 90]
checkEquals(vertical_angle_func(50, 1000) < 90, TRUE)
checkEquals(vertical_angle_func(50, 1000) > 0, TRUE)
# If antenna is user height, angle is 0
checkEquals(vertical_angle_func(1.5, 1000), 0)
# If antenna is lower than user height, angle is in [-90, 0]
checkEquals(vertical_angle_func(1, 1000) < 0, TRUE)
checkEquals(vertical_angle_func(1, 1000) > -90, TRUE)
checkEquals(vertical_angle_func(-10, 0.1) < 0, TRUE)
checkEquals(vertical_angle_func(-10, 0.1) > -90, TRUE)

##
# Compute gain after having moved horizontal and vertical angle
##
funcgain = function(delta_azimuth, delta_tilt, azimuth_3db, tilt_3db, gain_antenna) {
  gain_h = 0.5 * gain_antenna * ((delta_azimuth / azimuth_3db)^2)
  gain_h = -pmin(gain_h, gain_antenna)
  
  gain_v = 0.5 * gain_antenna * ((delta_tilt / tilt_3db)^2)
  gain_v = -pmin(gain_v, gain_antenna*(20/25))
  
  gain_terminal = gain_antenna - pmin(-(gain_h+gain_v), gain_antenna)
  
  return(gain_terminal)
}