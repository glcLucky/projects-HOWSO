# 数据读写 IO
library(data.table)
# 读取csv
data_OTT_f <- fread("step0422/data/plan_ott_data/plan_ott_data.csv",encoding = 'UTF-8',select=c(paste0("V",c(1:45))))
# 将数据集写出csv
write.table(data_OTT_f_subset, "data_OTT_f_subset.csv", row.names = F, col.names = T, sep = ",")
# 读取excel 
library(readxl)
new_table<- read_excel("step0422/data/table/test.xlsx")

# 基本数据管理
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 57, 75, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age,
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE)
# 创建新变量
mydata <- data.frame(x1 = c(1,2,3,4), x2 = c(5,6,7,8))
mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2) / 2)

# 变量重编码
leadership$age[leadership$age == 99] <- NA

# method1
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 & leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"

# method2 using within
leadership <- within(leadership, {
    agecat <- NA
    agecat[age > 75] <- "Elder"
    agecat[age >= 55 & age <= 75] <- "Middle Aged"
    agecat[age < 55] <- "Young"
})

# 变量重命名
# 交互式
fix(leadership)
# 编程式
# install.packages("plyr")
library(plyr)
leadership <- rename(leadership, c(q4_1="q4", q1="q1_test"))

# 缺失值
y <- c(1,2,3,NA)
is.na(y)

# 删除包含缺失值的行
newdata <- na.omit(leadership)

# 含缺失值的计算
x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4] # NA
z1 <- sum(x) # NA
z2 <- sum(x, na.rm = TRUE) # 6

# 日期值处理
# %d: 数字表示的日期 01 - 31
# %m: 月份 01 - 12
# %Y: 四位数年份 %y: 两位数年份
# %a 缩写的星期名 %A 非缩写的星期名 
# %b 缩写的月份 %B 非缩写的月份
mydates <- as.Date(c("2007-06-22", "2004-02-13"))
mydates
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")

today <- Sys.Date()
format(today, "%B %d %Y")
format(today, format="%A")
dob <- as.Date("1992-08-06")
difftime(today, dob, units = "weeks")
format(dob, "%A") # 出生当天是星期几？

# 数据转换函数
# is.numeric() as.numeric()
# is.character() as.character()
# is.vector() as.vector()
# is.matrix() as.matrix()
# is.data.frame() as.data.frame()
# is.factor() as.factor()
# is.logical() as.logical()

# 数据排序
# 默认为升序  降序加个负号即可
newdata <- leadership[order(leadership$age),]

with(leadership, {
  newdata1 <- leadership[order(gender, -age),]
})


# 数据集的合并
# 横向合并1 merge
total <- merge(dataframeA, dataframeB, by=c("ID","Country"))
# 横向合并2 cbind 无须指定公共索引 但每个对象必须拥有相同的行数 以相同顺序排序
total <- cbind(A, B)

# 纵向合并
total <- rbind(dataframeA, dataframeB)
# 两个数据框必须拥有相同的变量 但顺序不必一定相同

# 给予dplyr包的合并 类似于sql
library(dplyr)
# inner_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
# left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
# right_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
# full_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
# 数据取子集

# 选入变量
leadership[, c(6:10)] # 取所有行 6到10列
myvars <- c("q2", "q3", "q4", "q5")
# 等价于 myvars <- paste("q", 1:5, sep="")
leadership[myvars]

# 剔除变量
# method1 不知道剔除变量具体位置时
myvars <- names(leadership) %in% c("q3", "q4") 
leadership[!myvars] # 使用！将逻辑值反转

# method2 知道踢出变量具体位置时
leadership[c(-8, -9)] # 剔除第8、9个变量

# method3 使用NULL NULL表示没有定义 与NA不同
leadership$q5 <- leadership$q4 <- NULL 

# 选入观测
leadership[1:3,]
leadership[leadership$gender=="M" & leadership$age >30,]

leadership$date <- as.Date(leadership$date, "%m/%d/%y")
startdate <- as.Date("2008-10-24")
enddate <- as.Date("2019-10-31")
leadership[which(leadership$date >= startdate & leadership$date <= enddate)]

which(leadership$date >= startdate & leadership$date <= enddate)
leadership$date >= startdate & leadership$date <= enddate

# 利用subset选择变量和观测
subset(leadership, age >= 35 | age < 24, select = c(q1, q2, q3))
subset(leadership, gender=="M" & age > 25, select = gender:q3) # 选取gender和q3之间的所有列 

# 随机抽样
leadership[sample(1:nrow(leadership), 2, replace = FALSE),]

# 使用SQL
# install.packages("proto")
# install.packages("gsubfn")
# install.packages("RSQLite")
# install.packages("sqldf")
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg", row.names=TRUE)

# 高级数据管理
# 数学函数
abs(-4)
sqrt(16)
ceiling(4.5)
floor(3.6)
trunc(-5.8983) # 向0的方向截取的X中的整数部分
signif(2.34354, 3) # 将x舍入为指定的有效数字位数
log(10, base = 3)
log(10) # 自然对数ln 即base = e
log10(10) # 常用对数 以10为底
exp(2) # e^2
sqrt(c(4,5,6,7,8))

# 统计函数
x = c(1,2,3,4,5,6,7,99,11,4,5,2)
mean(x, trim = 0.1, na.rm = TRUE) # 丢弃最大10%和最小10%的数据 移除缺失值
mean(x)
median(x)
sd(x)
var(x)
mad(x) # 绝对中位差
quantile(x, probs = c(0.25, 0.75)) # 25% 和75%分位数
range(x) # 求值域
diff(range(x)) # 极差
scale(x, center = TRUE, scale = T)

# 概率函数
# d 密度函数 p 分布函数 q 分位数函数 r 生成随机数
# norm 正态分布 ; t T分布 ;unif 均匀分布 ; binom 二项分布
pnorm(1.96) # 位于1.96左侧的标准正态曲线下方面积是多少
qnorm(0.95, mean = 500, sd = 100) # 均值500 标准差100的正态分布的0.9的分位点是多少
rnorm(50, mean = 50, sd = 10) # 生成50个均值为50 标准差为10的正态随机数

# 字符串处理函数
x = '12345678'
nchar(x) # 计算x中的字符数量
substr(x, 3, 5) # 从第3个位置开始取，取到第5个位置为止

# 高级查找与匹配
grep(pattern, x, ignore.case=FALSE, fixed=FALSE) # 在x中搜索pattern 若fix为true 则pattern为一个文本字符串
sub(pattern, replacement, x, ignore.case=FALSE,fixed=FALSE) # 在x中搜索pattern并替换
strsplit(x, split, fixed=FALSE) # 在split处分割字符向量x中的元素
strsplit("a-be-c", "-")

paste(…, sep="") # 连接字符串 分隔符为sep
paste("x", 1:3,sep="")
paste('x','w','e','r',sep = '-')

# 大小写转换
toupper('abc')
tolower('EWd')

# 其他函数
length(x) # 计算对象x的长度 注意计算某个字符串的长度用nchar
seq(1,200,5) # 从1到200 以5为间隔生成序列
rep('andy', 5) # 将x重复n次
cut(x, n) # 将连续型变量x分割为有着n个水平的
cut(c(1,2,3,4,5,6,7), 3) # 
pretty(c(1,2,3,4,5,6,7), 3) # 创建美观分割点 通过选取n+1个等间距的取整值 将一个连续性变量x分割为n个区间

name <- "Bob"
cat( "Hello", name, "\b.\n", "Isn\'t R", "\t", "GREAT?\n") # cat输出连接后的对象时会将每一个对象用空格分开

# apply
mydata <- matrix(rnorm(30), nrow = 6)
apply(mydata, 1, mean) # 1表示行 2表示列
apply(mydata, 2, mean, trim=0.2) # trim=0.2是FUN的参数

# 一个数据处理难题
# 构建数据集
names = c("John Davis",	"Angela Williams",	"Bullwinkle Moose", "David Jones",	"Janice Markhammer",	"Cheryl Cushing",	"Reuven Ytzrhak",	"Greg Knox",	"Joel England",	"Mary Rayburn")
math = c(502,	600,	412,	358,	495,	512,	410,	625,	573,	522)
science = c(95,	99,	80,	82,	75,	85,	80,	95,	89,	86)
english = c(25,	22,	18,	15,	20,	28,	15,	30,	27,	18)
# 一定要加上stringsAsFactors = FALSE 否则姓名将被视为factor 而不是char 影响下面的分割
mydata <- data.frame("names"=names, "数学"=math, "科学"=science, "英语"=english, stringsAsFactors = FALSE)
str(mydata)

# 统计标准化以消除量纲
mydata[2:4] <- scale(mydata[2:4])

# 综合打分
mydata$score <- apply(mydata[2:4], 1, mean)
quan <- quantile(mydata$score,probs = c(0.2, 0.4, 0.6, 0.8))

mydata$grade[mydata$score > quan[4]] <- "A"
mydata$grade[mydata$score > quan[3] & mydata$score <= quan[4]] <- "B"
mydata$grade[mydata$score > quan[2] & mydata$score <= quan[3]] <- "C"
mydata$grade[mydata$score > quan[1] & mydata$score <= quan[2]] <- "D"
mydata$grade[mydata$score <= quan[1]] <- "E"

# 拆分姓名
names_sep <- strsplit(mydata$names, " ")
firstname <- sapply(names_sep, "[", 1)
lastname <- sapply(names_sep, "[", 2)
# mydata[, -1] 表示删除第一列
mydata <- cbind(firstname, lastname, mydata[, -1])

# 根据姓名排序
mydata[order(lastname, firstname),]

# 循环
# for循环
for (i in 1:10)
  print(i)

# while循环
i <- 10
while (i > 0)
{
  print(i);
  i <- i - 1;
}

# 条件执行

# methdo1
# if (cond) statement1 else statement2

if (!is.factor(grade))
  grade <- as.factor(grade) else 
    print("grade already is a factor")

# method2
# ifelse(cond, statement1, statement2) cond为真是执行语句1 否则执行语句2

scores <- 90
ifelse(scores>60, print("PASS"), print("FAILED"))

# method3 switch结构
# switch(expr, ...) ...表示与expr各种可能取值绑定的语句

feelings <- c('sad', 'afraid', 'happy')
for (feeling in feelings)
  print(
    switch(feeling,
            'sad' = "Cheer up",
            'afraid' = "There is nothing to fear",
            "happy" = "I'm glad you are happy",
            "angry" = "calm down now"
          )
      )

# 用户自定义函数
# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }

mystats <- function(x, parametric=T, print=F){
  options(digits = 4)
  if (parametric==T) {center = mean(x); spread = sd(x)} else 
  {center = median(x); spread = mad(x)}
  if (print & parametric) {cat("mean=", center, "\n", "std=", spread, '\n')}
  if (print & parametric==F) {cat("median=", center, "\n", "std=", spread, '\n')}
  result <- list(center=center, spread=spread)
  return(result)
}

x <- c(1,2,3,5,6,3,4,6,7,4,2,5,7,5,8,9,5)
a=mystats(x, parametric = T, print = T)
a$center

mydate <- function(type="long"){
  switch(type,
         long = format(Sys.time(), "%A %B %d %Y"),
         short = format(Sys.time(), "%m-%d-%y"),
         cat(type, "is not a recognized type\n") # 只有在type取值不属于long或short时才会执行
         )
}

mydate()
mydate("short")
mydate("ds")

# 整合与重构
# 转置
install.packages("reshape2")

# 融合(melt)与重铸(cast)
# 融合：是指将数据集重构为这样一种格式:每个测量变量独占一行 行中带有要唯一确定这个测量所需的标识符
library(reshape2)

mydata <- data.frame(
  "ID" = c(1,1,2,2),
  "Time" = c(1,2,1,2),
  "X1" = c(5,3,6,2),
  "X2" = c(6, 5, 1,4),
  stringsAsFactors = F
)

data_melt <- melt(mydata, id=c('ID', 'Time'), )  # id变量可以唯一确定每个测量

# 重铸
# newdata <- dcast(md, formula, fun.aggregate)
# formula格式： rowvar1 + rowvar2 + ... ~ colvar1 + colvar2 + ...
# 左边确定行标识符 即相同取值的只有一个 右边变量集合确定包括哪些列 通常只有一个

dcast(data = data_melt, formula = ID  ~ variable, mean) # 每个人各测量的均值 
dcast(data = data_melt, formula = ID  + Time ~ variable) # 每个人各测量各时间点的值 
dcast(data = data_melt, formula = ID ~ Time , mean) # 每个人各时间点的测量均值 

# 绘图并保存
pdf("C:\\Users\\Administrator\\Desktop\\test.pdf")
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight")
detach(mtcars) # 关闭绘图窗口
dev.off()
choose.files()
dev.new() # 打开新的绘图窗口

dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type = "b") # b: points and lines
help(plot)

# 图形参数
opar <- par(no.readonly = T) # opar即original parameter 将当前的图形参数设置复制一份
par(lty=2, pch=17) # 修改线条为虚线 将点符号改为实心三角
attach(mtcars)
plot(wt, mpg)
abline(lm(mpg ~ wt))
title("Regression of MPG on Weight")
detach(mtcars) # 关闭绘图窗口
par(opar) # 恢复初始绘图设置
plot(dose, drugA, type="b", lty=3, lwd=3, pch=15, cex=1.5)

# 基本调色板
n <- 10
mycolors1 <- rainbow(n) # 彩虹调色板
pie(rep(1, n), labels=mycolors1, col=mycolors1)

mycolors2 <- gray(0:n/n) # 灰色调色板
pie(rep(1, n), labels=mycolors2, col=mycolors2)

mycolors3 <- heat.colors(n) # 热色
pie(rep(1, n), labels=mycolors3, col=mycolors3)

mycolors4 <- terrain.colors(n) # 地形色
pie(rep(1, n), labels=mycolors4, col=mycolors4) 

mycolors5 <- topo.colors(n) 
pie(rep(1, n), labels=mycolors5, col=mycolors5) 

mycolors5 <- cm.colors(n) # 地形色
pie(rep(1, n), labels=mycolors5, col=mycolors5) 

# 高级调色板
install.packages("RColorBrewer")
library(RColorBrewer)
n <- 9
mycolors6 <- brewer.pal(n, "Set1")
pie(rep(1, n), labels=mycolors6, col=mycolors6)
brewer.pal.info # 显示可选调色板


# 文本属性
# 放缩系数cex 
#             cex.axis: 坐标轴刻度文字 cex.lab: 坐标轴标签
#             cex.main：标题 cex.sub: 副标题

# 字体font 1=常规 2=粗体 3斜体 4=粗斜体 5=符号字体
# font.axis font.lab font.main font.sub
# ps：字体磅值.
par(font.lab=3, cex.lab=1.5, font.main=4, cex.main=2)


# 图形尺寸与边界尺寸
# pin 以英寸表示的图形尺寸（宽和高）
# mai 以数值向量表示的边界大小 顺序为“下-左-上-右” 单位为英寸
# mar 以数值向量表示的边界大小 顺序为“下-左-上-右” 单位为英分
opar <- par(no.readonly=TRUE)
par(pin=c(6,4), mai=c(1.5,.5, 1, .2))
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
plot(dose, drugB, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)


# 添加文本、自定义坐标轴和图例
plot(dose, drugA, type="b",
     col="red", lty=2, pch=2, lwd=2,
     main="Clinical Trials for Drug A",
     sub="This is hypothetical data",
     xlab="Dosage", ylab="Drug Response",
     xlim=c(0, 60), ylim=c(0, 70))

# 标题
# 某些高级绘图函数已经包含了默认的标题和标签 可以通过添加ann=FALSE来移除他们
dev.off()
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
title(main="My Title", col.main="red",
      sub="My Subtitle", col.sub="blue",
      xlab="My X label", ylab="My Y label",
      col.lab="green", cex.lab=0.75)

# 坐标轴
# axis(side, at=, labels=, pos=, lty=, col=, las=, tck=, ...)
# side: 表示图形在哪边绘制坐标轴 1=下 2=左 3=上 4=右） 从下开始顺时针旋转
# side: 即画一个正方形 下面的边为1 顺时针旋转
# at: 一个数值型向量 表示需要绘制刻度线的位置
# labels: 一个字符向量 表示置于刻度线旁边的文字标签 若为空,则直接使用at中的值
# pos：坐标轴线绘制位置的坐标（即与另一条坐标轴相交位置的值）
# lty：line type 
# col：线条和刻度线的颜色
# las：标签是否平行于（=0）或垂直于（=2）坐标轴
# tck：刻度线的长度 以相对于绘图区域大小的分数表示 负值表示在图形外侧 正值表示在图形内侧 0表示禁用刻度 1表示绘制网格线 默认值为-0.01

# 一个完整的绘图例子
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = T)

par(pin=c(4,8), mar=c(5, 4, 4, 8) + 0.1)
plot(x, y, type = 'b', pch=21, col='red', yaxt='n', lty=3, ann=F)
lines(x, z, type = 'b', pch=22, col='blue', lty=2)
axis(2, at=x, labels = x, col.axis='red', las=2, font.axis=1)
axis(4, at=z, labels = round(z, digits = 2), col.axis='blue', las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col='blue')
title("An Example of Creative Axes", xlab = 'X vaules', ylab = 'Y=X')
dev.off()

# 参考线
# abline(h=yvalues, v=xvalues)
abline(v=seq(1, 10, 5), lty=2, col="blue")


# 图例
# legend(location, title, legend, ...)

opar <- par(no.readonly = T)
par(pin=c(4,8), mar=c(5, 4, 4, 8) + 0.1)
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type='b', pch=15, lty=1, col='red', ylim=c(0,60),
     main="Drug A vs. Drug B", xlab='Drug Dosage', ylab="Drug Response")
lines(dose, drugB, type='b',pch=17, lty=2, col='blue')
abline(h=c(30), lwd=1.5, lty=2, col='yellow')
install.packages("Hmisc")











