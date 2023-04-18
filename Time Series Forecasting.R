#install.packages('lmtest')
library(forecast)
library(lmtest)
library(tseries)
library(readr)


setwd("C:/Users/chowd/Documents/NEU/Projects/Immigrants_in_USA")
foreign_born<- read_csv("Foreign_Born_Population.csv", show_col_types = FALSE)

tsforeignBorn = ts(foreign_born$`Foreign Born Population`, start = 1850, deltat = 10)
plot(tsforeignBorn)

adf.test(tsforeignBorn)

acf(tsforeignBorn)

pacf(tsforeignBorn)

foreignBorn_arima1 <- arima(tsforeignBorn, order=c(0, 0, 1))
foreignBorn_arima1

confint(foreignBorn_arima1) 

foreignBorn_arima1.res <- residuals(foreignBorn_arima1)
acf(foreignBorn_arima1.res)
pacf(foreignBorn_arima1.res)

foreignBorn_autoarima <- auto.arima(tsforeignBorn)
summary(foreignBorn_autoarima)

residuals_autoArima <- residuals(foreignBorn_autoarima)
acf(residuals_autoArima)
pacf(residuals_autoArima)

Box.test(residuals_autoArima, lag = 1, type = "Ljung-Box")

forecast_auto_Arima <- forecast(foreignBorn_autoarima, h=2)
forecast_auto_Arima
plot(forecast_auto_Arima)

setwd("C:/Users/chowd/Documents/NEU/Projects/Immigrants_in_USA")
employed_immigrants <- read_csv("Employed_Immigrants.csv", show_col_types = FALSE)

tsEmployed = ts(employed_immigrants$Employee_Rate, start = 1995, deltat = 1)
plot(tsEmployed)

Employement_Rate <- auto.arima(tsEmployed)
summary(Employement_Rate)

residuals_Employement_Rate <- residuals(Employement_Rate)
acf(residuals_Employement_Rate)
pacf(residuals_Employement_Rate)

forecast_employement_rate <- forecast(Employement_Rate, h=19)
forecast_employement_rate
plot(forecast_employement_rate)

tsPoverty = ts(employed_immigrants$Poverty_Rate, start = 1995, deltat = 1)
plot(tsPoverty)

Poverty_Rate <- auto.arima(tsPoverty)
summary(Poverty_Rate)

residuals_Poverty_Rate <- residuals(Poverty_Rate)
acf(residuals_Poverty_Rate)
pacf(residuals_Poverty_Rate)

forecast_Poverty_Rate <- forecast(Poverty_Rate, h=19)
forecast_Poverty_Rate
plot(forecast_Poverty_Rate)




tsImmigrantPer = ts(employed_immigrants$Immigrant_Percentage, start = 1995, deltat = 1)
plot(tsImmigrantPer)

Immigrant_Rate <- auto.arima(tsImmigrantPer)
summary(Immigrant_Rate)

residuals_Immigrant_Rate <- residuals(Immigrant_Rate)
acf(residuals_Immigrant_Rate)
pacf(residuals_Immigrant_Rate)

forecast_Immigrant_Rate <- forecast(Immigrant_Rate, h=19)
forecast_Immigrant_Rate
plot(forecast_Immigrant_Rate)

#Fitting the Regression Model

regression_model_linear <- lm(Immigrant_Percentage ~ Poverty_Rate + Employee_Rate, data = employed_immigrants)

#Logistic regression model

regression_model_logistic <- glm(Immigrant_Percentage ~ Poverty_Rate + Employee_Rate, data = employed_immigrants)

summary(regression_model_linear)
summary(regression_model_logistic)

library(ggplot2)

ggplot(employed_immigrants, aes(x = Poverty_Rate, y = Immigrant_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Poverty Rate", y = "Immigrant Percentage")

ggplot(employed_immigrants, aes(x = Employee_Rate, y = Immigrant_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Employement Rate", y = "Immigrant Percentage")
