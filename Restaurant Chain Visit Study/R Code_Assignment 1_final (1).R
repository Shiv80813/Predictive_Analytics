library(readxl) # needed to read in the Excel data file
RestVisitDataFull <- read_excel("D:/MBAN/Predictive Analytics/Group Assignment/RestVisitDataFull.xlsx")  # read the Excel file
# Summarise data statistics
summary(RestVisitDataFull)
#Histogram
hist(RestVisitDataFull$Income)
hist(log(RestVisitDataFull$Income))
# Show scatter plots via pairs() command
pairs(RestVisitDataFull)
plot(RestVisitDataFull$Income, RestVisitDataFull$residuals)

#Fitting the regression model with x
RegressionModel = lm (Visits ~ Income , data = RestVisitDataFull)
summary(RegressionModel)$coefficients[,1]
plot(predict(RegressionModel, newdata = RestVisitDataFull), RestVisitDataFull$Visits)

#Fitting the regression model with log x
RegressionModelx = lm (Visits ~ log(Income) , data = RestVisitDataFull)
summary(RegressionModelx)$coefficients[,1]
plot(predict(RegressionModelx, newdata = RestVisitDataFull), RestVisitDataFull$Visits)
abline(0,1) 
summary(RegressionModelx)$r.squared

#Regression Model Validity Checks
plot(RegressionModelx, add.smooth = F)
#Check residual vs predictor
plot(RestVisitDataFull$Income, RegressionModelx$residuals)
#autocorrelation factor
acf(RegressionModelx$residuals)
#Anova
anova(RegressionModelx)
summary(RegressionModelx)
confint(RegressionModelx)

#Predict values with annual income of 50,000
Forecast1 = data.frame(Income = 50)
predict(RegressionModelx, newdata = Forecast1, interval = 'confidence')
predict(RegressionModelx, newdata = Forecast1, interval = 'prediction')
#Predict values with annual income of 100,000
Forecast2 = data.frame(Income = 100)
predict(RegressionModelx, newdata = Forecast2, interval = 'confidence')
predict(RegressionModelx, newdata = Forecast2, interval = 'prediction')

#Predict values with annual income of 200,000
Forecast3 = data.frame(Income = 200)
predict(RegressionModelx, newdata = Forecast3, interval = 'confidence')
predict(RegressionModelx, newdata = Forecast3, interval = 'prediction')