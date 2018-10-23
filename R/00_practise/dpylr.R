install.packages("nycflights13")
library(nycflights13)
dim(flights)
flights

library(dplyr)
################################################
#################六大基础函数###################
# filter arrange select mutate summarise sample_n

# 1. Filter rows with filter()
filter(flights, month==1, day==1)


# 2. Arrange rows with arrange() 重新排序
arrange(flights, year, desc(month), day)


# 3. Select columns with select()
select(flights, year, month, day) # # Select columns by name
select(flights, year:day) # Select all columns between year and day (inclusive)
select(flights, -(year:day)) # Select all columns except those from year to day (inclusive)
select(flights, tailnum_new = tailnum) # select and rename

# rename
rename(flights, tailnum_new = tailnum)


# 4.mutate
mutate(flights, gain = arr_delay - dep_delay, speed = distance / air_time * 60)

# mutate还可以使用刚刚创建的变量来计算新的变量
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60) # gain是刚刚创建的变量
)

# 如果你仅想保留新建的变量 则可以使用transmute
transmute(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60) # gain是刚刚创建的变量
)


# 5. Summarise values with summarise()
summarise(flights, delay = mean(dep_delay, na.rm = T))

# 6. 随机抽样
sample_n(flights, 10) # 抽取固定个数
sample_frac(flights, 0.01) # 抽取固定比例

# Use replace = TRUE to perform a bootstrap sample

################################################
###################分组计算#####################
by_tail_num <- group_by(flights, tailnum)
delay <- summarise(by_tail_num, 
                   count = n(),
                   dist = mean(distance, na.rm = T),
                   delay = mean(arr_delay, na.rm = T))

delay <- filter(delay, count > 20, dist < 2000)

destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

# 分级逐步汇总
# 按日
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

# 按月 
# daily按年月日汇总1次后就变成了按年月汇总
(per_month <- summarise(per_day, flights= sum(flights)))

# 按年
# daily按年月日汇总2次后就变成了按年汇总 
(per_year <- summarise(per_month, flights= sum(flights)))


# Selecting operations
# select函数可以同时按照纯列名(bare columns names)或者索引来选择列,选择列名其实本质上仍是指代位置 比如如下两种方法完全等效
select(flights, year)
select(flights, 1)

# 但是这样的话就会产生一定的歧义 比如
names(flights)
year = c('year', 'month', 'day')
select(flights, year)

# 当定义的存储变量组合的变量与列名重合时 select函数默认情况下仍将其视为一列 而不会当做变量处理 
# 为了避免这种错误可以使用如下两种方法
year = c('year', 'month', 'day')
select(flights, !!year) # !!告诉R提取year的变量值
select(flights, year, identity(year)) # indentity等价于!!


# Piping
########不使用管道###########
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
#############################

########使用管道###########
flights %>% group_by(year, month, day) %>% select(arr_delay, dep_delay) %>% 
    summarise(arr=mean(arr_delay, na.rm = T), dep = mean(dep_delay, na.rm = T)) %>% filter(arr > 30 | dep > 30)
#############################










