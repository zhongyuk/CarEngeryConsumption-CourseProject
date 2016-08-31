setwd('./Documents/course/Regression_Analysis/Assignments/Final_Report')
car <- read.csv('car_data.txt',header=TRUE,sep='')
car <- car[,-1]

library(ggplot2); library(GGally)
colnames(car) = c( "vol","hp","mpg","sp","wt")
tsp <- log10(car$sp);thp <- log(car$hp-35);
tcar <- data.frame(car$vol,thp,car$mpg,tsp,car$wt)
colnames(tcar)=c('vol','thp','mpg','tsp','wt')


## Model Diagonostics
mod1 <- lm(mpg~thp+tsp+wt,data=tcar); summary(mod1)

mod2 <- lm(mpg~thp+tsp+wt+vol,data=tcar); summary(mod2)

mod3 <- lm(mpg~thp+tsp+vol,data=tcar); summary(mod3)

#mod4 <- lm(mpg~thp+tsp,data=tcar); summary(mod4)

#mod5 <- lm(mpg~thp+wt+vol,data=tcar); summary(mod5)

### Model diagonostics on Model1
# 1) Residual plot and qq plot
par(mfrow=c(2,2))
plot(mod1$fit,mod1$res,xlab='Fitted Values', ylab='Residuals',
     main='Model 11-fitted vs. residual',pch=21, col='black', bg='salmon', cex=1.5)
plot(tcar$thp,mod1$res,xlab='tHP',ylab='Residuals',
     main='Model 11-tHP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$tsp,mod1$res,xlab='tSP',ylab='Residuals',
     main='Model 11-tSP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$wt,mod1$res,xlab='WT',ylab='Residuals',
     main='Model 11-WT vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
qqnorm(mod1$res);qqline(mod1$res)
# 2) Transformation for correcting heteroscedasiticity.
plot(tcar$thp,tcar$mpg);
plot(car$hp,log(car$mpg))
plot(tcar$thp,log(tcar$mpg)) #***

plot(car$sp,log(car$mpg))
plot(tcar$tsp,tcar$mpg);
plot(tcar$tsp,log(tcar$mpg))
plot(-1/(car$sp-80),log(car$mpg)) #***

plot(car$wt, car$mpg)
plot(tcar$wt,log(tcar$mpg)) #***

# 3) Examine the residual to see the if the transformation worked
tmpg <- log(tcar$mpg)
thp <- tcar$thp
tsp <- -1/(car$sp-80)
twt <- tcar$wt

modtrail1 <- lm(tmpg ~ thp+tsp+twt);

# 4) Residual Plots on newly obtained model
par(mfrow=c(2,2))
plot(modtrail1$fit,modtrail1$res,xlab='Fitted Values', ylab='Residuals',
     main='Corrected Model 11-fitted vs. residual',pch=21, col='black', bg='salmon', cex=1.5)
plot(thp,modtrail1$res,xlab='tHP',ylab='Residuals',
     main='Corrected Model 11-tHP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tsp,modtrail1$res,xlab='tSP',ylab='Residuals',
     main='Corrected Model 11-tSP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(twt,modtrail1$res,xlab='WT',ylab='Residuals',
     main='Corrected Model 11-WT vs. residual', pch=21, col='black', bg='salmon',cex=1.5)

# 5) Outlier and other model diagnosis
par(mfrow=c(2,3))
plot(modtrail1,which=c(1:6),pch=21,col='black',bg='lightblue')





### Model diagonostics on Model2
# 1) Residual plot and qq plot
par(mfrow=c(2,3))
plot(mod2$fit,mod2$res,xlab='Fitted Values', ylab='Residuals',
     main='Model 15-fitted vs. residual',pch=21, col='black', bg='salmon', cex=1.5)
plot(tcar$thp,mod2$res,xlab='tHP',ylab='Residuals',
     main='Model 15-tHP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$tsp,mod2$res,xlab='tSP',ylab='Residuals',
     main='Model 15-tSP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$wt,mod2$res,xlab='WT',ylab='Residuals',
     main='Model 15-WT vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$vol,mod2$res,xlab='VOL',ylab='Residuals',
     main='Model 15-VOL vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
qqnorm(mod2$res,pch=21,col='black',bg='salmon',cex=1.5, main='Model 15-Normal Q-Q Plot');qqline(mod2$res)

# 2) Transformation for correcting heteroscedasiticity.
plot(tcar$thp,tcar$mpg);
plot(car$hp,log(car$mpg))
plot(tcar$thp,log(tcar$mpg)) #***

plot(car$sp,log(car$mpg))
plot(tcar$tsp,tcar$mpg);
plot(tcar$tsp,log(tcar$mpg))
plot(-1/(car$sp-80),log(car$mpg)) #***

plot(car$wt, car$mpg)
plot(tcar$wt,log(tcar$mpg)) #***

plot(car$vol,car$mpg)
plot(log(car$vol), log(car$mpg))#***


# 3) Examine the residual to see the if the transformation worked
tmpg <- log(tcar$mpg)
thp <- tcar$thp
tsp <- -1/(car$sp-80)
twt <- tcar$wt
tvol <- log(car$vol)

modtrail2 <- lm(tmpg ~ thp+tsp+twt+tvol);

# 4) Residual Plots on newly obtained model
par(mfrow=c(2,3))
plot(modtrail2$fit,modtrail2$res,xlab='Fitted Values', ylab='Residuals',
     main='Corrected Model 15-fitted vs. residual',pch=21, col='black', bg='salmon', cex=1.5)
plot(thp,modtrail2$res,xlab='tHP',ylab='Residuals',
     main='Corrected Model 15-tHP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tsp,modtrail2$res,xlab='tSP',ylab='Residuals',
     main='Corrected Model 15-tSP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(twt,modtrail2$res,xlab='WT',ylab='Residuals',
     main='Corrected Model 15-WT vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tvol,modtrail2$res,xlab='tVOL',ylab='Residuals',
     main='Corrected Model 15-VOL vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
qqnorm(modtrail2$res,pch=21,col='black',bg='salmon',cex=1.5, main='Corrected Model 15-Normal Q-Q Plot');
qqline(modtrail2$res)

# 5) Outlier and other model diagnosis
par(mfrow=c(2,3))
plot(modtrail2,which=c(1:6),pch=21,col='black',bg='lightblue')






### Model diagonostics on Model3
# 1) Residual plot and qq plot
par(mfrow=c(2,2))
plot(mod3$fit,mod3$res,xlab='Fitted Values', ylab='Residuals',
     main='Model 12-fitted vs. residual',pch=21, col='black', bg='salmon', cex=1.5)
plot(tcar$thp,mod3$res,xlab='tHP',ylab='Residuals',
     main='Model 12-tHP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$tsp,mod3$res,xlab='tSP',ylab='Residuals',
     main='Model 12-tSP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tcar$vol,mod3$res,xlab='VOL',ylab='Residuals',
     main='Model 12-VOL vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
qqnorm(mod3$res);qqline(mod3$res)
# 2) Transformation for correcting heteroscedasiticity.
plot(tcar$thp,tcar$mpg);
plot(car$hp,log(car$mpg))
plot(tcar$thp,log(tcar$mpg)) #***

plot(car$sp,log(car$mpg))
plot(tcar$tsp,tcar$mpg);
plot(tcar$tsp,log(tcar$mpg))
plot(-1/(car$sp-80),log(car$mpg)) #***

plot(car$wt, car$mpg)
plot(tcar$wt,log(tcar$mpg)) #***

# 3) Examine the residual to see the if the transformation worked
tmpg <- log(tcar$mpg)
thp <- tcar$thp
tsp <- -1/(car$sp-80)
tvol <- log(tcar$vol)

modtrail3 <- lm(tmpg ~ thp+tsp+tvol);

# 4) Residual Plots on newly obtained model
par(mfrow=c(2,2))
plot(modtrail3$fit,modtrail3$res,xlab='Fitted Values', ylab='Residuals',
     main='Corrected Model 12-fitted vs. residual',pch=21, col='black', bg='salmon', cex=1.5)
plot(thp,modtrail3$res,xlab='tHP',ylab='Residuals',
     main='Corrected Model 12-tHP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tsp,modtrail3$res,xlab='tSP',ylab='Residuals',
     main='Corrected Model 12-tSP vs. residual', pch=21, col='black', bg='salmon',cex=1.5)
plot(tvol,modtrail3$res,xlab='tVOL',ylab='Residuals',
     main='Corrected Model 12-tVOL vs. residual', pch=21, col='black', bg='salmon',cex=1.5)

# 5) Outlier and other model diagnosis
par(mfrow=c(2,3))
plot(modtrail3,which=c(1:6),pch=21,col='black',bg='lightblue')




### Adjusted R Square and AIC

summary(modtrail1)$adj.r.squared
summary(modtrail2)$adj.r.squared
summary(modtrail3)$adj.r.squared

AIC(modtrail1)
AIC(modtrail2)
AIC(modtrail3)