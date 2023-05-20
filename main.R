#Author : LAKSHMI SHRAVYA RAMARAJU
library(car)
library(MASS)
library(leaps)
library(fBasics)

cars<-read.csv("cars_used.csv",header=TRUE)
cars <- na.omit(cars) 
attach(cars)

diagnostic_plots <- function(model) {
  fits <- model$fitted.values
  residuals <-model$residuals
  plot(fits,residuals,xlab='estimates log(price) ',ylab='residuals',pch=20,col='blue')
  abline(h=0)
  
  qqnorm(residuals, pch = 1, frame = FALSE)
  qqline(residuals, col = "steelblue", lwd = 2)
  
  boxcox(model)
  
  stud_residuals <-sqrt(abs(studres(model)))
  plot(fits,stud_residuals,xlab='estimates log(price)  ',
       ylab='sqrt of studentised residuals',pch=20,col='red')
}


colinearity_in_data <- function(data){
  plot(data.frame(data$mileage,data$year,data$mpg,data$tax,data$engineSize))
  res <- cor(data.frame(data$mileage,data$year,data$mpg,data$tax,data$engineSize))
  round(res, 2)
}


before_after_log <- function(data){
  par(mfrow=c(1,2))
  hist(data)
  hist(log(data))
}



####### EDA ########
model_counts <- table(model)
barplot(model_counts, main='Frequency of car models in the dataset')
min(model_counts)
pie(model_counts)


year_counts <- table(year)
barplot(year_counts, main = 'Bar plot of year_counts')
pie(year_counts,main = 'Piechart of year_counts')


x = basicStats(price)
format(x, scientific=FALSE)
hist(price,xlim=c(0,max(price)),probability = TRUE)
lines(density(price),col = 4, lwd = 2)
b = boxplot(price, main= 'Boxplot for price')
length(b$out)



transmission_count <- table(transmission)
barplot(transmission_count,main = 'Barplot of transmission_count' )
pie(transmission_count, main = 'Piechart of transmission_count')


x = basicStats(mileage)
format(x, scientific=FALSE)
hist(mileage,xlim=c(0,max(mileage)),probability = TRUE)
lines(density(mileage),col = 4, lwd = 2)
b = boxplot(mileage, main= 'Boxplot for mileage')
length(b$out)

fuel_counts <- table(fuelType)
barplot(fuel_counts, main= 'Barplot for fuel_counts')
pie(fuel_counts, main= 'Piechart for fuel_counts')


x = basicStats(tax)
hist(tax,xlim=c(0,max(tax)),probability = TRUE)
lines(density(tax),col = 4, lwd = 2)
boxplot(tax,main= 'Boxplot for tax')


tax_count <- table(tax)
barplot(tax_count)
pie(tax_count)


x = basicStats(mpg)
format(x, scientific=FALSE)
hist(mpg,xlim=c(0,max(mpg)),probability = TRUE)
lines(density(mpg),col = 4, lwd = 2)
b = boxplot(mpg, main= 'Boxplot for mpg')
length(b$out)


engineSize_counts <- table(engineSize)
barplot(engineSize_counts,main = 'Barplot of engineSize_counts' )
pie(engineSize_counts, main = 'Piechart of engineSize_counts' )

make_counts <- table(Make)
barplot(make_counts,main = 'Barplot of make_counts')
pie(make_counts, main = 'Pie chart of make_counts')

boxplot(price~Make,main='Box plots of the Makes vs price')
boxplot(price~Make+model,main='Box plots of the Models vs price')



##### Model 1: super_cars ########
super_cars_data <- subset(cars, Make == 'audi' & model == ' R8', select = -c(Make,model,fuelType))

before_after_log(super_cars_data$price)
before_after_log(super_cars_data$year)
before_after_log(super_cars_data$tax)
before_after_log(super_cars_data$mileage)
before_after_log(super_cars_data$engineSize)




# only apply log to vars where skewness is reduced
super_cars_data$price <- log(super_cars_data$price)
super_cars_data$mileage <- log(super_cars_data$mileage + 1)
super_cars_data$year <- log(super_cars_data$year + 1)
colinearity_in_data(super_cars_data)

super_cars.lm <- lm(price~., data=super_cars_data)
summary(super_cars.lm)
#diagnostic_plots(super_cars.lm)

#variable selection
step.model <- stepAIC(super_cars.lm, direction = "both", trace = FALSE)
summary(step.model)
best_subsets = regsubsets(price~., data= super_cars_data, nbest=1, 
                          nvmax = NULL, force.in = NULL, force.out = NULL,
                          method='exhaustive')
summary(best_subsets)
data.frame(
  Adj.R2 = which.max(summary(best_subsets)$adjr2),
  CP = which.min(summary(best_subsets)$cp),
  BIC = which.min(summary(best_subsets)$bic)#shows sem-auto to take off
)

super_cars_sig <- subset(super_cars_data, select = c(year,tax,price,engineSize))
super_cars_best.lm <- lm(price~., data=super_cars_sig)
summary(super_cars_best.lm)
vif(super_cars_best.lm)
diagnostic_plots(super_cars_best.lm)

durbinWatsonTest(super_cars_best.lm)

confint(super_cars_best.lm, level=0.95)



##### Model 2: luxury_cars ########
BMW_cars <- subset(cars, Make == 'BMW')
audi_cars <- subset(cars, Make == 'audi' & model != ' R8') 
luxury_cars_data <- subset(rbind(BMW_cars,audi_cars),select = -c(model))
before_after_log(luxury_cars_data$price)
before_after_log(luxury_cars_data$year)
before_after_log(luxury_cars_data$tax)
before_after_log(luxury_cars_data$mileage)
before_after_log(luxury_cars_data$engineSize)

luxury_cars_data$price <- log(luxury_cars_data$price)
luxury_cars_data$year <- log(luxury_cars_data$year + 1)
luxury_cars_data$mpg <- log(luxury_cars_data$mpg + 1)
luxury_cars_data$tax <- log(luxury_cars_data$tax + 1)
colinearity_in_data(luxury_cars_data)

luxury_cars.lm <- lm(price~., data=luxury_cars_data)
summary(luxury_cars.lm)
#diagnostic_plots(super_cars.lm)

#variable selection
step.model <- stepAIC(luxury_cars.lm, direction = "both", trace = FALSE)
summary(step.model)
best_subsets = regsubsets(price~., data= luxury_cars_data, nbest=1, nvmax = NULL,
                          force.in = NULL, force.out = NULL, method='exhaustive')
summary(best_subsets)
data.frame(
  Adj.R2 = which.max(summary(best_subsets)$adjr2),
  CP = which.min(summary(best_subsets)$cp),
  BIC = which.min(summary(best_subsets)$bic)#shows sem-auto to take off
)

luxury_cars_best.lm <- lm(price~., data=subset(luxury_cars_data,transmission !='Semi-Auto'))
summary(luxury_cars_best.lm)
vif(luxury_cars_best.lm)
diagnostic_plots(luxury_cars_best.lm)

durbinWatsonTest(luxury_cars_best.lm)

confint(luxury_cars_best.lm, level=0.95)



###### Model 3: mid-range cars ######
skoda_cars <- subset(cars, Make == 'skoda')
hyundai_cars <- subset(cars, Make == 'Hyundai')
toyota_cars <- subset(cars, Make == 'toyota')
vw_cars <- subset(cars, Make == 'vw')
ford_cars <- subset(cars, Make == 'Ford')
midRange_cars_data =  rbind(skoda_cars,hyundai_cars,vw_cars,toyota_cars,ford_cars)
midRange_cars_data <- subset(midRange_cars_data,select = -c(model))
before_after_log(midRange_cars_data$price)
before_after_log(midRange_cars_data$year)
before_after_log(midRange_cars_data$tax)
before_after_log(midRange_cars_data$mileage)
before_after_log(midRange_cars_data$engineSize)

midRange_cars_data$price <- log(midRange_cars_data$price)
midRange_cars_data$mpg <- log(midRange_cars_data$mpg + 1)
midRange_cars_data$year <- log(midRange_cars_data$year + 1)
midRange_cars_data$tax <- log(midRange_cars_data$tax + 1)

colinearity_in_data(midRange_cars_data)

midRange_cars.lm <- lm(price~., data=midRange_cars_data)
summary(midRange_cars.lm)
#diagnostic_plots(midRange_cars.lm)

step.model <- stepAIC(midRange_cars.lm, direction = "both", trace = FALSE)
summary(step.model)
best_subsets = regsubsets(price~., data= midRange_cars_data, nbest=1,
                          nvmax = NULL, force.in = NULL, force.out = NULL,
                          method='exhaustive')
summary(best_subsets)
data.frame(
  Adj.R2 = which.max(summary(best_subsets)$adjr2),#10
  CP = which.min(summary(best_subsets)$cp),#10
  BIC = which.min(summary(best_subsets)$bic)#shows 9
)

midRange_cars_best.lm <- lm(price~., data=subset(midRange_cars_data,transmission!='Other'&fuelType!='Electric'))
summary(midRange_cars_best.lm)
vif(midRange_cars_best.lm)
diagnostic_plots(midRange_cars_best.lm)

durbinWatsonTest(midRange_cars_best.lm)

confint(midRange_cars_best.lm, level=0.95)



### extra ###
#rectification for durbin watson by making response difference in y's
#no imporvement adjusted R2 comes to 0.2107
# diffprice <- midRange_cars$price[2:nrow(midRange_cars)]-midRange_cars$price[1:(nrow(midRange_cars)-1)]
# diffprice <- append(diffprice, 10550, 0)
# midRange_cars$diffprice <- diffprice 
# midRange_cars2.lm <- lm(diffprice~., data=subset(midRange_cars, select = -c(price,model)))
# summary(midRange_cars2.lm)
# durbinWatsonTest(midRange_cars2.lm)
