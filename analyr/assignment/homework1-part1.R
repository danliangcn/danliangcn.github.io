# Problem 1
birthrate <- c(.4777, .4875, .4859, .4754, .4874, .4864, .4813, .4787, .4895, .4797,
       .4876, .4859, .4857, .4907, .5010, .4903, .4860, .4911, .4871, .4725,
       .4822, .4870, .4823, .4973)
# 出生率的标准差
sdbirth <- sd(birthrate)
sdbirth
# 平均出生率的标准差（理论标准差）
avgrate <- sum(3900*birthrate)/(3900*24)
sigmabirth <- sqrt(avgrate*(1-avgrate)/3900)

# 原假设为出生率标准差的实际值与理论值没有区别
# 计算统计量和两侧临界值
(test <- 23*(sdbirth^2)/(sigmabirth^2))
(lowerchi2 <- qchisq(p=0.05/2,df=23, lower.tail = T))
(upperchi2 <- qchisq(p=0.05/2,df=23, lower.tail = F))
## 因为检验统计量介于两侧临界值之间，所以无法推翻原假设，
## 即出生率标准差的实际值与理论值的差异不显著。

# Problem 2 
x <- NULL
for(i in 1:10000) {
  xsample <- runif(20)
  x[i] <- sum(xsample)
}
hist(x, freq = F, breaks = 100)
xfit <- seq(min(x),max(x),length.out = 100) 
yfit <- dnorm(xfit, mean=mean(x),sd=sd(x))
lines(xfit,yfit,col="blue",lwd=2)

# Problem 3
diff <- NULL
for(i in 1:10000){
  x <- mean(rnorm(100,mean=69.1,sd=2.9))
  y <- mean(rnorm(100,mean=63.7,sd=2.7))
  diff[i] <- x-y
}
hist(diff)
# mean and standard deviation from simulation
mean(diff)
sd(diff)
# exact value or theoretical value
# E(X-Y)=E(X)-E(Y)=69.1-63.7=5.4
# D(X-Y)=D(X)+D(Y)=(2.9^2)/100+(2.7^2)/100=15.7/100=0.157
# sd(X-Y)=15.7^0.5=0.396
