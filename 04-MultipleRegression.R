#set workspace
setwd("F:/EikDenYeoh/Documents/Research Paper/R")

#load library
library(readxl)
library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
library(animation)
library(Metrics)
library(vtreat)
library(forecast)
library(xts)
library(astsa)

#Assign top 4 csv file to each variables
ytlreit<-read.csv(file="Top 4/5109.YTLREIT.csv", header=TRUE, sep=",")
mqreit<-read.csv(file="Top 4/5123.MQREIT.csv", header=TRUE, sep=",")
atrium<-read.csv(file="Top 4/5130.ATRIUM.csv", header=TRUE, sep=",")
sunreit<-read.csv(file="Top 4/5176.SUNREIT.csv", header=TRUE, sep=",")

#convert date from factor to date format
ytlreit$Date <- as.Date(ytlreit$Date, format="%m/%d/%Y")
mqreit$Date <- as.Date(mqreit$Date, format="%m/%d/%Y")
atrium$Date <- as.Date(atrium$Date, format="%m/%d/%Y")
sunreit$Date <- as.Date(sunreit$Date, format="%m/%d/%Y")

#create model - remove date for multiple regression model
ytlreit_model <- lm(Adj.Close ~ ., data = ytlreit[,2:7])
mqreit_model <- lm(Adj.Close ~ ., data = mqreit[,2:7])
atrium_model <- lm(Adj.Close ~ ., data = atrium[, 2:7])
sunreit_model <- lm(Adj.Close ~., data = sunreit[, 2:7])

#Summary to identify the Multiple-R Square to find the significant result
summary(ytlreit_model)
summary(mqreit_model)
summary(atrium_model)
summary(sunreit_model)

#Generating a random test/train split
ytl_n <- nrow(ytlreit)
mqreit_n <- nrow(mqreit)
atrium_n <-nrow(atrium)
sunreit_n <- nrow(sunreit)

#assign 75% of rows for train data
ytl_target <- round(ytl_n * 0.75)
mqreit_target <- round(mqreit_n * 0.75)
atrium_target <- round(atrium_n * 0.75)
sunreit_target <- round(sunreit_n * 0.75)

#create vector N uniform random variables
ytl_gp <- runif(ytl_n)
mqreit_gp <- runif(mqreit_n)
atrium_gp <- runif(atrium_n)
sunreit_gp <- runif(sunreit_n)


#use gp to create training set
ytl_train <- ytlreit[ytl_gp<0.75,]
ytl_test <- ytlreit[ytl_gp>=0.75,]

mq_train <- mqreit[mqreit_gp<0.75,]
mq_test <- mqreit[mqreit_gp>=0.75,]

atrium_train <- atrium[atrium_gp<0.75,]
atrium_test <- atrium[atrium_gp>=0.75,]

sunreit_train <- sunreit[sunreit_gp<0.75,]
sunreit_test <- sunreit[sunreit_gp>=0.75,]

#create train model
ytlreit_model <- lm(Adj.Close ~ ., data = ytl_train[,2:7])
summary(ytlreit_model)
#evaluate a model
#predict for the training set
ytl_train$predict <- predict(ytlreit_model, level=0.95)
#predict for the test set
ytl_test$predict <- predict(ytlreit_model, newdata=ytl_test, level=0.95)

mqreit_model <- lm(Adj.Close ~ ., data = mq_train[,2:7])
summary(mqreit_model)
#evaluate a model
#predict for the training set
mq_train$predict <- predict(mqreit_model, level=0.95)
#predict for the test set
mq_test$predict <- predict(mqreit_model, newdata=mq_test, level=0.95)

atrium_model <- lm(Adj.Close ~ ., data = atrium_train[,2:7])
summary(atrium_model)
#evaluate a model
#predict for the training set
atrium_train$predict <- predict(atrium_model, level=0.95)
#predict for the test set
atrium_test$predict <- predict(atrium_model, newdata=atrium_test, level=0.95)

sunreit_model <- lm(Adj.Close ~ ., data = sunreit_train[,2:7])
summary(sunreit_model)
#evaluate a model
#predict for the training set
sunreit_train$predict <- predict(sunreit_model, level=0.95)
#predict for the test set
sunreit_test$predict <- predict(sunreit_model, newdata=sunreit_test, level=0.95)


#Generally, model performance is better on the training data than the
#test data (though sometimes the test set "gets lucky"). 
#A slight difference in performance is okay; 
#if the performance on training is significantly better, there is a problem.

# Evaluate the rmse on both training and test data and print them
(ytl_rmse_train <- rmse(ytl_train$predict,ytl_train$Adj.Close))
(ytl_rmse_test <- rmse(ytl_test$predict, ytl_test$Adj.Close))

(mq_rmse_train <- rmse(mq_train$predict,mq_train$Adj.Close))
(mq_rmse_test <- rmse(mq_test$predict, mq_test$Adj.Close))

(atrium_rmse_train <- rmse(atrium_train$predict,atrium_train$Adj.Close))
(atrium_rmse_test <- rmse(atrium_test$predict, atrium_test$Adj.Close))

(sunreit_rmse_train <- rmse(sunreit_train$predict,sunreit_train$Adj.Close))
(sunreit_rmse_test <- rmse(sunreit_test$predict, sunreit_test$Adj.Close))

# Evaluate the r-squared on both training and test data.and print them
r_squared <- function(predict, adjusted_close){
  cor(predict, adjusted_close)^2
}

(ytl_rsq_train <- r_squared(ytl_train$predict,ytl_train$Adj.Close))
(ytl_rsq_test <- r_squared(ytl_test$predict, ytl_test$Adj.Close))

(mq_rsq_train <- r_squared(mq_train$predict,mq_train$Adj.Close))
(mq_rsq_test <- r_squared(mq_test$predict, mq_test$Adj.Close))

(atrium_rsq_train <- r_squared(atrium_train$predict,atrium_train$Adj.Close))
(atrium_rsq_test <- r_squared(atrium_test$predict, atrium_test$Adj.Close))

(sunreit_rsq_train <- r_squared(sunreit_train$predict,sunreit_train$Adj.Close))
(sunreit_rsq_test <- r_squared(sunreit_test$predict, sunreit_test$Adj.Close))

# Plot the predictions (on the x-axis) against the outcome (cty) on the test data
ytl_plot<-ggplot(ytl_test, aes(x = Date)) + 
  geom_line(aes(y=Adj.Close, colour="Actual"))+
  geom_line(aes(y=predict, colour="Predict"))+
  labs(y="Stock Price", x="Date")+
  ggtitle("YTLREIT Predict vs Actual stock price")

mq_plot<-ggplot(mq_test, aes(x = Date)) + 
  geom_line(aes(y=Adj.Close, colour="Actual"))+
  geom_line(aes(y=predict, colour="Predict"))+
  labs(y="Stock Price", x="Date")+
  ggtitle("MQREIT Predict vs Actual stock price")

atrium_plot<-ggplot(atrium_test, aes(x = Date)) + 
  geom_line(aes(y=Adj.Close, colour="Actual"))+
  geom_line(aes(y=predict, colour="Predict"))+
  labs(y="Stock Price", x="Date")+
  ggtitle("ATRIUM Predict vs Actual stock price")

sunreit_plot<- ggplot(sunreit_test, aes(x = Date)) + 
  geom_line(aes(y=Adj.Close, colour="Actual"))+
  geom_line(aes(y=predict, colour="Predict"))+
  labs(y="Stock Price", x="Date")+
  ggtitle("SUNREIT Predict vs Actual stock price")

multiplot(ytl_plot,mq_plot,atrium_plot,sunreit_plot,cols=2)

#create forecast
ytl_arima <- auto.arima(xts(ytl_test$predict,ytl_test$Date))
ytl_forecast <- forecast(ytl_arima, h=60)
plot(ytl_forecast, main="YTL Forecast from Prediction Result (60 days)")

mq_arima <- auto.arima(xts(mq_test$predict,mq_test$Date))
mq_forecast <- forecast(mq_arima, h=60)
plot(mq_forecast,main="MQREIT Forecast from Prediction Result (60 days)")

atrium_arima <- auto.arima(xts(atrium_test$predict,atrium_test$Date))
atrium_forecast <- forecast(atrium_arima, h=60)
plot(atrium_forecast, main="ATRIUM Forecast from Prediction Result (60 days)")

sunreit_arima <- auto.arima(xts(sunreit_test$predict,sunreit_test$Date))
sunreit_forecast <- forecast(sunreit_arima, h=60)
plot(sunreit_forecast, main="SUNREIT Forecast from Prediction Result (60 days)")
