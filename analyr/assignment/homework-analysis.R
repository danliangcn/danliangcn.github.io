### 载入cgssincome数据集，将教育程度（本人、父母）重新进行编码转化为教育年限
###，政治面貌重新进行编码转化为是否党员。
library(haven)
setwd("~/Downloads/rproject/presentation")
load("cgssincome.Rdata")

# 如果用菜单方式（会使用haven包），需要去除变量标签，否则有些函数无法使用。
cgssincome <- zap_labels(cgssincome)

cgssincome$age <- 2015-cgssincome$birth
cgssincome$eduyear[cgssincome$edu==1] <- 0
cgssincome$eduyear[cgssincome$edu==2] <- 3
cgssincome$eduyear[cgssincome$edu==3] <- 6
cgssincome$eduyear[cgssincome$edu==4] <- 9
cgssincome$eduyear[cgssincome$edu==5] <- 12
cgssincome$eduyear[cgssincome$edu==6] <- 12
cgssincome$eduyear[cgssincome$edu==7] <- 13
cgssincome$eduyear[cgssincome$edu==8] <- 13
cgssincome$eduyear[cgssincome$edu==9] <- 15
cgssincome$eduyear[cgssincome$edu==10] <-15
cgssincome$eduyear[cgssincome$edu==11] <- 16
cgssincome$eduyear[cgssincome$edu==12] <- 16
cgssincome$eduyear[cgssincome$edu==13] <- 20
cgssincome$eduyear[cgssincome$edu==14] <- NA

cgssincome$feduyear[cgssincome$fatheredu==1] <- 0
cgssincome$feduyear[cgssincome$fatheredu==2] <- 3
cgssincome$feduyear[cgssincome$fatheredu==3] <- 6
cgssincome$feduyear[cgssincome$fatheredu==4] <- 9
cgssincome$feduyear[cgssincome$fatheredu==5] <- 12
cgssincome$feduyear[cgssincome$fatheredu==6] <- 12
cgssincome$feduyear[cgssincome$fatheredu==7] <- 13
cgssincome$feduyear[cgssincome$fatheredu==8] <- 13
cgssincome$feduyear[cgssincome$fatheredu==9] <- 15
cgssincome$feduyear[cgssincome$fatheredu==10] <-15
cgssincome$feduyear[cgssincome$fatheredu==11] <- 16
cgssincome$feduyear[cgssincome$fatheredu==12] <- 16
cgssincome$feduyear[cgssincome$fatheredu==13] <- 20
cgssincome$feduyear[cgssincome$fatheredu==14] <- NA

cgssincome$meduyear[cgssincome$motheredu==1] <- 0
cgssincome$meduyear[cgssincome$motheredu==2] <- 3
cgssincome$meduyear[cgssincome$motheredu==3] <- 6
cgssincome$meduyear[cgssincome$motheredu==4] <- 9
cgssincome$meduyear[cgssincome$motheredu==5] <- 12
cgssincome$meduyear[cgssincome$motheredu==6] <- 12
cgssincome$meduyear[cgssincome$motheredu==7] <- 13
cgssincome$meduyear[cgssincome$motheredu==8] <- 13
cgssincome$meduyear[cgssincome$motheredu==9] <- 15
cgssincome$meduyear[cgssincome$motheredu==10] <-15
cgssincome$meduyear[cgssincome$motheredu==11] <- 16
cgssincome$meduyear[cgssincome$motheredu==12] <- 16
cgssincome$meduyear[cgssincome$motheredu==13] <- 20
cgssincome$meduyear[cgssincome$motheredu==14] <- NA

cgssincome$party[cgssincome$polstatus!=4] <- 0
cgssincome$party[cgssincome$polstatus==4] <- 1

# 探索数据设定缺失值
summary(cgssincome)

# 收入有异常值，
cgssincome$income[cgssincome$income>40000+1.5*35000]

cgssincome$income[cgssincome$income<0] <-NA
cgssincome$edu[cgssincome$edu<0] <-NA
cgssincome$polstatus[cgssincome$polstatus<0] <-NA
cgssincome$workexp[cgssincome$workexp<0] <-NA
cgssincome$fatheredu[cgssincome$fatheredu<0] <-NA
cgssincome$motheredu[cgssincome$motheredu<0] <-NA
cgssincome$fatherwork[cgssincome$fatherwork<0] <-NA
cgssincome$motherwork[cgssincome$motherwork<0] <-NA

# 剔除掉缺失值
cgssincome <- na.omit(cgssincome)

cgssincome$sex <- factor(cgssincome$sex,levels = c(1,2),labels=c("male","female"))
cgssincome$party <- factor(cgssincome$party,levels = c(0,1),labels=c("nonCCP","CCP"))

tparty <- table(cgssincome$sex, cgssincome$party)
prop.table(tparty)

# 直方图
hist(cgssincome$income,breaks=900,xlim = c(0,500000))
hist(log(cgssincome$income),breaks=50,freq = FALSE)
x <- log(cgssincome$income)
x[is.infinite(x)]<- NA
x <- na.omit(x)
xfit<-seq(min(x),max(x),length=1000)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
lines(xfit,yfit,col="blue",lwd=2)
box()

summary(x)
# 箱线图
boxplot(cgssincome$income)
smalldata <- cgssincome[cgssincome$income<150000,c("income","sex")]
boxplot(smalldata$income)
boxplot(income~sex, data = smalldata)

# 散点图
plot(cgssincome$eduyear, cgssincome$income,ylim = c(0,1000000))

cor(cgssincome$income, cgssincome$eduyear)

# 计算在置信水平为95%的条件下，居民收入的置信区间
inc <- cgssincome$income
n <- length(inc)
estimate <- mean(inc)
se <- sd(inc)/sqrt(n)
int.95 <- estimate + qt(c(.025,.975),n-1)*se
int.95

# 收入的显著性检验
t.test(inc, mu=50000)

# 收入的性别差异

mean(cgssincome$income[cgssincome$sex=="male"])
mean(cgssincome$income[cgssincome$sex=="female"])

# alternatively...
by(cgssincome$income,cgssincome$sex,summary)

t.test(cgssincome$income[cgssincome$sex=="male"],cgssincome$income[cgssincome$sex=="female"])

# 父母教育程度差异成对检验
mean(cgssincome$feduyear);mean(cgssincome$meduyear)
t.test(cgssincome$feduyear,cgssincome$meduyear,paired = TRUE)

save(cgssincome, file = "cgssincome.RData")
