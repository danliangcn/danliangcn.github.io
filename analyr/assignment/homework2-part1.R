### Problem 1
pyth <- read.table("exercise2.1.dat", header = T)
fit1 <- lm(y~x1+x2,data = pyth[1:40,])
#（a）回归系数显著，R方0.97，拟合度高，自变量能够解释y的97%的变异。
summary(fit1)
#（b）
plot(pyth$x1,pyth$y)
abline(fit1$coefficients[1]+fit1$coefficients[3]*mean(pyth[1:40,]$x2),fit1$coefficients[2])
# 另一种方法
curve(cbind (1, x, mean(pyth[1:40,]$x2)) %*% coef(fit1), col="red",add = T)
#（c）中间部分残差偏大，两端偏小，可能违背线性假定
plot(fitted(fit1),resid(fit1))
#（d）预测值与95%的置信区间
predict(fit1, pyth[41:60,], interval="prediction", level=0.95)

### Problem 2
library(foreign)
childiq <- read.dta("child.iq.dta")
fit2 <- lm(ppvt~momage, data = childiq)
# (a) 没有明显违背假定的地方；母亲生育年龄增加1岁，小孩测试分数增加0.84分
# 建议晚育，前提是没有遗漏变量的问题，值得注意的是R方很小，所以年龄的解释力度并不高。
summary(fit2)
plot(fit2)
# （b）新的回归显示生育年龄不再显著，教育程度每增加一个级别，小孩测试分数增加4.7分
# 不再建议晚育，建议多读几年书，提高教育水平
fit2 <- lm(ppvt~momage + educ_cat, data = childiq)
summary(fit2)
# （c）交互项为正且显著，即教育程度可以减少生育年龄对分数的负面影响，
# 或生育年龄可以抵消教育程度的负面影响
childiq$hs <- ifelse(childiq$educ_cat>=2,1,0)
fit2 <- lm(ppvt~momage + hs + hs:momage, data = childiq)
summary(fit2)

colors <- ifelse(childiq$hs == 1, "blue", "red")
plot(childiq$momage,childiq$ppvt,col=colors)
curve(cbind(1,x,0, 0) %*% coef(fit2), col="blue",add = T)
curve(cbind(1,x,1, x) %*% coef(fit2), col="red",add = T)
# (d) 预测效果并不太好，大多不在45度直线上。
fit2 <- lm(ppvt~momage + educ_cat, data = childiq[1:200,])
summary(fit2)
preiq <- predict(fit2, childiq[201:400,], interval = "prediction", level = 0.05)
plot(preiq[,1],childiq[201:400,]$ppvt,xlab="predicted",ylab="observed")
abline(a=0,b=1)

### problem 3
# (a)
beauty <- read.csv("ProfEvaltnsBeautyPublic.csv",header = T)
fit3 <- lm(courseevaluation~btystdave+female+minority+nonenglish+tenuretrack+lower+onecredit,data = beauty)
summary(fit3)

plot(beauty$btystdave, beauty$courseevaluation, type = 'p', pch=16,cex=0.5,xlab = "Beauty", ylab = "Evaluation")
curve(cbind(1,x,1,0,0,1,0,0) %*% coef(fit3), col="blue", add = T)

plot(fitted(fit3),resid(fit3))

# (b)
plot(beauty$didevaluation,resid(fit3))
# WLS with the number of students completing the evaluation as weights
fit3wls <- lm(courseevaluation~btystdave+female+minority+nonenglish+tenuretrack+lower+onecredit,weights = didevaluation,data = beauty)
summary(fit3wls)

# predictors are X-variables; inputs are the information on the units that goes into the X-variables.
# so an interaction is a predictor but not an input.

fit3 <- lm(courseevaluation~btystdave+female+female:btystdave+minority+minority:btystdave+nonenglish+tenuretrack+lower+onecredit,data = beauty)
summary(fit3)

# optional: to get the same cluster robust standard errors as Table 3
library(miceadds)
res.cluster <- lm.cluster(data=beauty, formula=courseevaluation~btystdave+female+minority+nonenglish+tenuretrack+lower+onecredit, cluster = "profnumber", weights=beauty$didevaluation)
summary(res.cluster)
