## 1. Linear Regression

library(mlbench)
data(BostonHousing)
housing = BostonHousing
house = scale(housing[c(1,3,6,7,12,14)])
house = data.frame(house)
plot(house)

summary(lm(medv~.,house))

house = housing[c(-4,-14)]
pca = princomp(house,cor=TRUE,scores=TRUE)
summary(pca)
score1=pca$s
score=data.frame(pca$scores[,1:5])

summary(lm(housing$medv~score$Comp.1+score$Comp.2+score$Comp.3+score$Comp.4+score$Comp.5))

# 1.1 Simple Linear Regression #

advertising = read.csv('ISLR_dataset/Advertising.csv', head=TRUE)
head(advertising)

lm.fit <- lm(sales ~ TV, data=advertising)
summary(lm.fit)

predict(lm.fit, data.frame(TV = 100), interval = "confidence")    # confidnece interval for mean resaponse
predict(lm.fit, data.frame(TV = 100), interval = "prediction")    # prediction interval for response

plot(advertising$TV, advertising$Sales, main = "Scatterplot of Sales vs. TV", xlab="TV", ylab="Sales", col = "blue")
abline(lm.fit, col = "red")

# 1.2 Multiple Linear Regression #

advertising = advertising[,-1]
head(advertising)

cor(advertising)


lm.fit <- lm(sales ~ TV + radio + newspaper, data=advertising)
summary(lm.fit)


predict(lm.fit, data.frame(TV = 100, radio = 40, newspaper = 60), interval = "confidence")
predict(lm.fit, data.frame(TV = 100, radio = 40, newspaper = 60), interval = "prediction")

# 1.3 Variable Selection #

fit1=lm(St~At+Pt+Et+At1+Pt1,adv)
anova(fit1)
fit2=lm(St~At+Pt+Et,adv)
anova(fit2)

fit1=lm(St~At+Pt+Et+At1+Pt1,adv)
fit2=lm(St~At+Pt+Et,adv)
anova(fit2,fit1)

fit1=lm(St~At+Pt+Et+At1+Pt1,adv)
fit2=lm(St~At+Pt+Et,adv)
fit3=lm(St~Pt+Et,adv)

# mallow's cp
sig=sum(fit1$residual^2)/df.residual(fit1)
n=nrow(adv)
sum(fit1$residual^2)/sig+2*6-n
sum(fit2$residual^2)/sig+2*4-n
sum(fit3$residual^2)/sig+2*3-n

# AIC
c(AIC(fit1),AIC(fit2),AIC(fit3))

# BIC
n=nrow(adv)
c(AIC(fit1,k=log(n)),AIC(fit2,k=log(n)),AIC(fit3,k=log(n)))

# selection procedure
null=lm(ACHV~1,adv)
full=lm(St~.,adv)

step(null,scope=list(lower=null,upper=full),direction='forward')
step(full,scope=list(lower=null,upper=full),direction='backward')
step(null,scope=list(lower=null,upper=full),direction='both')

# 1.4 Diagnosis #

mpg=read.csv(file("e:/MPG.csv"),header=T)
plot(mpg)
mpg_fit=lm(MPG~Weight+Odometer,mpg)
mpg_fit$residuals
mpg_fit$fitted

# standardized residual
rstudent(mpg_fit)

# leverage value
hatvalues(mpg_fit)
mean(hatvalues(mpg_fit))

# cook's distance
cooks.distance(mpg_fit)

plot(mpg_fit, which=5)
plot(mpg_fit, which=2)

# 1.5 Transformation of Variable #

# nonlinear relationship
bacteria=read.csv(file("d:/bacteria.csv"),header=T)
plot(bacteria$t,bacteria$nt)
result1=lm(nt~t,data=bacteria)
plot(result1$fitted,rstandard(result1))

plot(bacteria$t, log(bacteria$nt))
result2=lm(log(nt)~t,data=bacteria)
plot(result2$fitted,rstandard(result2))
summary(result2)

# heteroscedastic problem
data=read.csv(file("d:/artificial1.csv"),header=T)
result1=lm(Y~X,data)
plot(data$X,data$Y)
plot(result1$fitted,rstandard(result2))
layout(matrix(c(1,2,3,4),2,2))
plot(result1)

data$newY=log(data$Y)
result2=lm(newY~X,data)
plot(result@fitted,rstandard(result2))
layout(matrix(c(1,2,3,4),2,2))
plot(result2)

# weighted least squre
result1=lm(Y~x,data)
summary(result1)
plot(result1$fitted,rstandard(result1))

data$TY=data$Y/data$X
data$TX=1/data$X
result3=lm(TY~TX,data)
summary(result3)
layout(matrix(c(1,2,3,4),2,2))
plot(result3)

result4=lm(Y~X,data,weights=1/X^2)
summary(result4)
layout(matrix(c(1,2,3,4),2,2))
plot(result4)

# 1.6 Dummy Variable #

turkey=read.csv(file("d:/Turkey.csv"),header=T)
turkey$V=ifelse(turkey$Origin=='V',1,0)
turkey$W=ifelse(turkey$Origin=='W',1,0)
fit1=lm(Y~X+V+W,turkey)
summary(fit1)

# regression fit with factor
turkey=read.csv(file("d:/Turkey.csv"),header=T)
fit1=lm(Y~X+factor(Origin),turkey)

# with interaction
turkey=read.csv(file("d:/Turkey.csv"),header=T)
fit2=lm(Y~X+Origin+Origin*X,turkey)
summary(fit2)

# 1.7 Correlation Problem #

bacteria=read.csv(file("d:/bacteria.csv"),header=T)
result1=lm(n~t,data=bacteria)
r=rstandard(result1)
sign(r)

install.packages('randtests')
library(randtests)
runs.test(r,"two.sided",threshold=0)

data=read.csv(url("http://www.stat.uiowa.edu/~kchan/TSA/Datasets/rwalk.dat"),header=T)
time=seq(1:60)
result=lm(data$rwalk~time)
plot(time,data$rwalk,type="l")
abline(result)
summary(result)
r=rstandard(result)
runs.test(r,"two.sided",threshold=0)

install.packages("lmtest")
library(lmtest)
dwtest(bacteria$nt~bacteria$t)
dwtest(data$rwalk~time)

new_r=rstudent(result1)[2:15] 
acf(new_r) 
acf(rstudent(result))

# 1.7 Multicollinearity Problem #

fit_F=lm(FAM~PEER+SCHOOL,edu)
summary(fit_F)
install.packages("HH")
library(HH)
vif(fit)

edu=read.csv(file("d:/education.csv"),header=T) #Read data
Y=edu[,1] # Original response variable
X=edu[,2:4] # Original covariate matrix
TX=scale(X,center=TRUE, scale=TRUE) # Standardized covariate
TY=scale(Y,center=TRUE, scale=TRUE) # Standardized response

eig=eigen(t(TX)%*%TX) # Spectral decomposition
P=eig$vectors # Eigenvector
L=eig$values # Eigen
L
L/sum(L)
C=TX%*%P # Principal component
result1=lm(TY~C-1) #Principal component regression
summary(result1)
vif(result1)

alpha=result1$coefficient
theta=P%*%alpha
beta=sd(Y)*theta/sd(X)
beta0=mean(Y)-sum(beta*mean(X))
c(beta0,beta)

# Ridge regression
library("MASS")
lm.ridge(ACHV~.,data=edu,lambda=0.37)
plot(lm.ridge(ACHV~.,data=edu,lambda=seq(0,30,0.01)))
select(lm.ridge(ACHV~.,data=edu,lambda=seq(0,30,0.01)))