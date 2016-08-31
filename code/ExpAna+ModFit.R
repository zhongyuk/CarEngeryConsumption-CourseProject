setwd('./Documents/course/Regression_Analysis/Assignments/Final_Report')
car <- read.csv('car_data.txt',header=TRUE,sep='')

## Exploratory Analysis
library(ggplot2); library(GGally)
car <- car[,-1]
ggpairs(car,lower=list(continuous='smooth'),title='Pairwise Exploratory Analysis',
        params=c(method='loess'))
colnames(car) = c( "vol","hp","mpg","sp","wt")

## Transforming SP, HP
### 1) transformation on SP
par(mfrow=c(2,2))
plot(car$sp,car$mpg)
plot(log10(car$sp),car$mpg)
tsp <- log10(car$sp)

### 2) Transformation on HP
plot(car$hp,car$mpg)
plot(log(car$hp-35),car$mpg)
thp <- log(car$hp-35)

### 3) Form new data frame with transformed data
tcar <- data.frame(car$vol,thp,car$mpg,tsp,car$wt)
colnames(tcar)=c('vol','thp','mpg','tsp','wt')
ggpairs(tcar,lower=list(continuous='smooth'),title='Pairwise Exploratory Analysis',
        params=c(method='loess'))

## Model Fitting
mod1a <- lm(mpg~wt,data=tcar);summary(mod1a)  #R^2=0.8192
mod1b <- lm(mpg~thp,data=tcar);summary(mod1b) #R^2=0.83
mod1c <- lm(mpg~tsp,data=tcar);summary(mod1c) #0.5158
mod1d <- lm(mpg~vol,data=tcar);summary(mod1d) #0.1359

mod2a <- lm(mpg~wt+thp,data=tcar);summary(mod2a) #0.8763
mod2b <- lm(mpg~wt+tsp,data=tcar);summary(mod2b) ##0.83
mod2c <- lm(mpg~wt+vol,data=tcar);summary(mod2c) #0.8151
mod2d <- lm(mpg~thp+tsp,data=tcar);summary(mod2d) #0.8895
mod2e <- lm(mpg~thp+vol,data=tcar);summary(mod2e) #0.8553
mod2f <- lm(mpg~tsp+vol,data=tcar);summary(mod2f) #0.648

mod3a <- lm(mpg~wt+thp+tsp,data=tcar);summary(mod3a) #0.903
mod3b <- lm(mpg~wt+thp+vol,data=tcar);summary(mod3b) #0.8827
mod3c <- lm(mpg~wt+tsp+vol,data=tcar);summary(mod3c) #0.8345
mod3d <- lm(mpg~tsp+thp+vol,data=tcar);summary(mod3d) #0.8907

mod4 <- lm(mpg~wt+thp+tsp+vol,data=tcar);summary(mod4) #0.9023

    
## ***********Some Plotting

par(mfrow=c(2,3))
plot(tcar$wt, tcar$mpg, pch=21, col='black',bg='lightblue',cex=1.5, main='Scatter Plot: wt vs mpg',
     xlab='weight',ylab='mpg')
plot(tcar$wt, tcar$mpg, main='Scatter Plot and Regression Line for ModelFit1',
     xlab='weight',ylab='mpg')
abline(mod1$coe[1], mod1$coe[2],lwd=2,col='black')
points(tcar$wt, tcar$mpg, pch=21, col='black', bg='lightblue', cex=1.5)
plot(mod1,pch=21,col='black',bg='lightblue',cex=1.5)

### Fitted Model 2
g = ggplot(tcar, aes(y = mpg, x = wt, colour = thp))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black", lwd=1.5) 
g + geom_point(size = 4) + ggtitle("Scatter Plot and Regression Line for ModelFit2")

