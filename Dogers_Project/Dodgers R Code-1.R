# Partial R code for Dodgers Revisited Regression
# Use these two lines to import from the Web
install.packages("readr")
library(readr)
# dodgers <- read.csv("dodgers.csv")
dodgers <- read.csv("D:/MBAN/Predictive Analytics/Group Assignment 2/dodgers.csv")

# Data summary
summary(dodgers)
# Preliminary plots requested in the case charge
# fivenum gives minimum, 25%, 50% 75% and maximum of attendance
fivenum(dodgers$attend)
# put plots of attendance vs temp here vvvv
#plotting the correlation between thw attendance and temperature
plot(dodgers$temp,dodgers$attend)

plot(attend~temp,data = dodgers)

# now some boxplots to help you see what to expect in the regression
#Box plot for day of the week
boxplot(attend~day_of_week,data=dodgers)
#Box plot for bobblehead
boxplot(attend~bobblehead,data=dodgers)
#Box plot for cap
boxplot(attend~cap,data=dodgers)
#Box plot for shirt
boxplot(attend~shirt,data=dodgers)
#Box plot for fireworks
boxplot(attend~fireworks,data=dodgers)

# Scale temperature to plus/minus 1 call it stemp vvvv
scalpm1 = function (x ){( x -( min (x )+ max ( x ))/2)/
    (.5*( max ( x)- min (x )))}
dodgers$stemp = scalpm1(dodgers$temp) 


# Create the dummy variables for day_night, cap, shirt, fireworks, etc
#   (dummy for bobblehead already done) and dropping the character column
dodgers$cBob = ifelse(dodgers$bobblehead=="YES",1,0)
dodgers = dodgers[,!names(dodgers)%in% c("bobblehead")]
#Create the dummy variable for day_night and dropping the character column
dodgers$cDay_Night = ifelse(dodgers$day_night=="Day",1,0)
dodgers = dodgers[,!names(dodgers)%in% c("day_night")]
#Create the dummy variable for cap and dropping the character column
dodgers$ccap = ifelse(dodgers$cap=="YES",1,0)
dodgers = dodgers[,!names(dodgers)%in% c("cap")]
#Create the dummy variable for shirt and dropping the character column
dodgers$cshirt = ifelse(dodgers$shirt=="YES",1,0)
dodgers = dodgers[,!names(dodgers)%in% c("shirt")]
#Create the dummy variable for fireworks and dropping the character column
dodgers$cfireworks = ifelse(dodgers$fireworks=="YES",1,0)
dodgers = dodgers[,!names(dodgers)%in% c("fireworks")]
# Create the day_of_week dummy variables and dropping the character column
dodgers$Sun = ifelse(dodgers$day_of_week=="Sunday",1,0)
dodgers$Mon = ifelse(dodgers$day_of_week == "Monday", 1, 0)
dodgers$Tue = ifelse(dodgers$day_of_week == "Tuesday", 1, 0)
dodgers$Wed = ifelse(dodgers$day_of_week == "Wednesday", 1, 0)
dodgers$Thu = ifelse(dodgers$day_of_week == "Thursday", 1, 0)
dodgers$Fri = ifelse(dodgers$day_of_week == "Friday", 1, 0)
dodgers = dodgers[,!names(dodgers)%in% c("day_of_week")]
summary(dodgers)

#Fitting the regression model
regModel <- lm(attend ~ stemp + I(stemp^2) + cBob + cDay_Night + ccap + cshirt + cfireworks + Sun + Mon + Tue + Wed + Thu + Fri, data = dodgers)
summary(regModel)$coefficients[,1]
plot(predict(regModel, newdata = dodgers), dodgers$attend)
abline(0,1) 
summary(regModel)$r.squared
#Checking VIF
install.packages("rms")
library(rms)
vif(regModel)

#regression model after dropping Friday because it has the highest VIF
regModel <- lm(attend ~ stemp + I(stemp^2) + cBob + cDay_Night + ccap + cshirt + cfireworks + Sun + Mon + Tue + Wed + Thu, data = dodgers)
#Checking the vif again
vif(regModel)
#summarising the values
summary(regModel)$coefficients[,1]
plot(predict(regModel, newdata = dodgers), dodgers$attend)
abline(0,1) 
#Checking r square
summary(regModel)$r.squared


#Validity Checks
plot(regModel)
#individual residual plots with different variables
plot(dodgers$cfireworks, regModel$residuals)
plot(dodgers$cBob, regModel$residuals)
plot(dodgers$cDay_Night, regModel$residuals)
plot(dodgers$ccap, regModel$residuals)
plot(dodgers$cshirt, regModel$residuals)
plot(dodgers$Wed, regModel$residuals)
plot(dodgers$Sun, regModel$residuals)
plot(dodgers$Mon, regModel$residuals)
plot(dodgers$Tue, regModel$residuals)
plot(dodgers$Thu, regModel$residuals)
plot(dodgers$Fri, regModel$residuals)
acf(regModel$residuals)

#summary
summary(regModel)
#anova
anova(regModel)
#confin
confint(regModel)

#installing package for backward approach
install.packages("olsrr")
library(olsrr)
#dropping variables with a higher p value (base of 6%)
beModel = ols_step_backward_p(regModel, details = TRUE, prem = 0.06)

#Updated model keeping the criteria of 6% in mind
regModel <- lm(attend ~ I(stemp^2) + stemp + cBob + cshirt + Mon, data = dodgers)
summary(regModel)$coefficients[,1]
plot(predict(regModel, newdata = dodgers), dodgers$attend)
abline(0,1) 
#Checking r square
summary(regModel)$r.squared
#summary for the new model
summary(regModel)
#confint for the new model
confint(regModel)

#When bob were given
new_data <- data.frame(cBob = 1, 'I(stemp^2)' = 0, stemp = 0, cshirt = 0, Mon = 0)
#confidence and prediction intervals for Tuesday Night when bob was given 
predict(regModel, newdata = new_data, interval = 'confidence')
predict(regModel, newdata = new_data, interval = 'prediction')

#When bob were not given
new_data <- data.frame(cBob = 0, 'I(stemp^2)' = 0, stemp = 0, cshirt = 0, Mon = 0)
#confidence and prediction intervals for Tuesday Night when bob was not given
predict(regModel, newdata = new_data, interval = 'confidence')
predict(regModel, newdata = new_data, interval = 'prediction')

