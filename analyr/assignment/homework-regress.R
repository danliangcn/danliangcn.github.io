setwd("~/Downloads/rproject/presentation")
load("cgssincome.Rdata")

cgssincome$income[cgssincome$income==0] <- NA
cgssincome <- na.omit(cgssincome)
# linear regression model
fit <- lm(log(income)~eduyear, data = cgssincome)
summary(fit)

# explain the intercept
exp(coef(fit)[1])

# plot the model
plot(cgssincome$eduyear,log(cgssincome$income),xlim=c(0,22),ylim=c(0,20),pch=20,cex=0.5)
curve(coef(fit)[1] + coef(fit)[2]*x, add=TRUE, col="red")

# coefficient's confidence interval
confint(fit)

# predict eduyear=12
x.new <- data.frame (eduyear=12)
inclog <- predict (fit, x.new, interval="prediction", level=0.95)
exp(inclog)

# Plotting a 95% confidence interval
x <- cgssincome$eduyear
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(fit, newdata=data.frame(eduyear=newx), interval="confidence", level = 0.95)
plot(cgssincome$eduyear,log(cgssincome$income),xlim=c(0,18),ylim=c(9,11),pch=20,cex=0.5,xlab="Education",ylab="Income in log")
abline(fit,col="red")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

## alternatively....
library(ggplot2)
ggplot(cgssincome, aes(x=eduyear, y=log(income+0.01))) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')

# mutiple regression
fit1 <- lm(log(income)~eduyear+workexp+I(workexp^2)+party+sex, data = cgssincome)
summary(fit1)

# regression diagnose
plot(fit1)
library(car)
vif(fit1)
anova(fit,fit1)
AIC(fit,fit1)

# compare regression
plot(cgssincome$eduyear,log(cgssincome$income),xlim=c(0,22),ylim=c(5,15),pch=20,cex=0.5,xlab="Education",ylab="Income in log")
abline(fit,col="red")
curve (coef(fit1)[1] + coef(fit1)[2]*x + coef(fit1)[3]*15+coef(fit1)[4]*(15^2)+coef(fit1)[5]*0.14+coef(fit1)[6]*0.47, add=TRUE, col="blue")

# plot by sex
plot(cgssincome$eduyear,log(cgssincome$income),xlim=c(0,22),ylim=c(0,20),type="n",xlab="Education",ylab="Income in log")
curve (coef(fit1)[1] + coef(fit1)[2]*x + coef(fit1)[3]*15+coef(fit1)[4]*(15^2)+coef(fit1)[5]*0.14+coef(fit1)[6], add=TRUE, col="red")
curve (coef(fit1)[1] + coef(fit1)[2]*x+ coef(fit1)[3]*15+coef(fit1)[4]*(15^2)+coef(fit1)[5]*0.14, add=TRUE,col="blue")
points (cgssincome$eduyear[cgssincome$sex=="male"], log(cgssincome$income[cgssincome$sex=="male"]), pch=19,cex=0.5,col="blue")
points (cgssincome$eduyear[cgssincome$sex=="female"]+0.2, log(cgssincome$income[cgssincome$sex=="female"]), pch=19,cex=0.5,col="red")

# plot the relationship betwwen workexp and log(income)
plot(cgssincome$workexp,log(cgssincome$income),xlim=c(0,55),ylim=c(0,20),pch=20,cex=0.5,xlab="work experience",ylab="Income in log")
curve (coef(fit1)[1] + coef(fit1)[2]*12 + coef(fit1)[3]*x+coef(fit1)[4]*(x^2)+coef(fit1)[5]*0.14+coef(fit1)[6]*0.47, add=TRUE, col="red",lwd=2)

# interaction item
fit2 <- lm(log(income)~eduyear+workexp+I(workexp^2)+party+sex+workexp:sex, data = cgssincome)
summary(fit2)
anova(fit1,fit2)

# dummy variables
library(fastDummies)
reg3data <- cgssincome[,c("province","income","eduyear","workexp","party","sex")]
reg3data <- dummy_cols(reg3data,select_columns = c("province"))
fit3 <- lm(log(income)~.+I(workexp^2)-province-province_4, data = reg3data)
summary(fit3)
