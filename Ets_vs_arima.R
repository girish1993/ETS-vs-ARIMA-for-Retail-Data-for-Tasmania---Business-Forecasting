
#including the libraries
library(fpp2)
library(timeSeries)
library(zoo)
library(readxl)
library(ggplot2)
library(fpp2)
library(readxl)
library(tidyverse)
library(urca)



#reading data and creating a time series
retail_ts <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>% pull("29270863") %>% ts(start = c(1982,4), frequency = 12)



#plotting the time series
autoplot(retail_ts)+xlab("Time in years")+ylab("Turnoer (in millions)")+ggtitle("Total turnover for Newspaper and book retailing from 1982 to 2016")

#seasonal plot
ggseasonplot(retail_ts)+ylab("Turnover of Footwear and accessory in Tasmania (in millions)")+ggtitle("Seasonal variation of turnover for each month from 1982 to 2016")

#subseries plot
ggsubseriesplot(retail_ts)+ylab("Turnover of Footwear and accessory in Tasmania (in millions)")+ggtitle("Sub series plot variation of turnover for each month")

#lag plots
gglagplot(retail_ts)+ylab("Turnover of Footwear and accessory in Tasmania (in millions)")+ggtitle("Lag plot for time series of turnover for Tasmania")

##second question. Transformations on data
autoplot(BoxCox(retail_ts,lambda = 1/2))+ggtitle("Square Root transformation of time series data") # square root transformation
autoplot(BoxCox(retail_ts,lambda = 1/3))+ggtitle("Cube Root transformation of time series data") # cube root transformation
autoplot(BoxCox(retail_ts,lambda = 0))+ggtitle("Log transformation of time series data") #log transformation
autoplot(BoxCox(retail_ts,lambda = -1))+ggtitle("Inverse transformation of time series data") #no transformation

lambda <- BoxCox.lambda(retail_ts) #lambda proposed by BoxCoX
autoplot(BoxCox(retail_ts,lambda = lambda))+ggtitle("Box Cox Transformation of time series data with suggested lambda value")

retail_ts <- ts(BoxCox(retail_ts,lambda = lambda),start = c(1982,4),frequency = 12)
autoplot(retail_ts)+ggtitle("Transformed time series data with the suggested lambda value")+ylab("Turnover(in millions)")

## third question. Splitting the data into train and test
retail_train <- window(retail_ts,start=c(1982,4),end=c(2014,12))
retail_test <- window(retail_ts,start=c(2015,1))

autoplot(retail_ts)+autolayer(retail_train,series = "Training")+autolayer(retail_test,series = "Testing")+ylab("Turnover of footwear and accessories in Tasmania(in millions)")+ggtitle("Plotting of Training and Testing Dataset in a single plot")

##-----------------Benchmarking
fc1 <- rwf(retail_train, drift=TRUE)
fc3 <- meanf(retail_train,h = 24)
fc2 <- snaive(retail_train)


autoplot(retail_train) +
  autolayer(fc1, series="Drift", PI=FALSE) +
  autolayer(fc2, series="Seasonal Naive", PI=FALSE) + ggtitle("Application of benchmarking methods and plotting them on the same plot")+ylab("Turnover(in millions)")+autolayer(fc3,series = "Mean benchmarking",PI = FALSE)

# Forecasting using the benchmarking
f1 <- rwf(retail_train,drift = TRUE,h=length(retail_test),lambda = lambda)
f3 <- meanf(retail_train,h=length(retail_test),lambda = lambda)
f2 <- snaive(retail_train,h=length(retail_test),lambda = lambda)
autoplot(retail_test) +autolayer(f3,series = "Mean",PI = FALSE) +autolayer(f1,series = "Random walk forecasts",PI = FALSE)+autolayer(f2,series = "Seasonal naive forecasts",PI = FALSE)+ggtitle("Plotting of the forecasts against the test data")+ylab("Turnover of accessories and footwear sales for Tasmania(in millions)")

performance_random_walk <- data.frame(accuracy(f1,retail_test))
performance_mean <- data.frame(accuracy(f3,retail_test))
performance_seasonal_naive <-data.frame(accuracy(f2,retail_test))

write.csv(performance_seasonal_naive,"seaonal_naive.csv")
write.csv(performance_random_walk,"random_walk.csv")
#-------------------------------------------------------------------------
##------residual analysis on Seasonal naive
checkresiduals(f3)

#-------------------------------------------------------------------------

#--------Generating point forecasts for the next 2 years
predictions1 <- snaive(retail_ts,h = 24,lambda = lambda)
write.csv(predictions1,"forecasts_for_2_years.csv")
predictions2 <- meanf(retail_test,h = 24)
#predictions <- ts(predictions,start = c(2017,1))


point_forecasts <- data.frame(InvBoxCox(predictions1$mean,lambda = lambda))
lower_forecasts <- data.frame(InvBoxCox(predictions1$lower,lambda = lambda))
higher_forecasts <- data.frame(InvBoxCox(predictions1$upper,lambda = lambda))
write.csv(point_forecasts,"point_forecasts.csv")
write.csv(lower_forecasts,"lower_forecasts.csv")
write.csv(higher_forecasts,"higher_forecasts.csv")

autoplot(retail_ts)+autolayer(predictions1,series="predictions",PI=FALSE)+ggtitle("Forecasts for the next 2 years using seasonal naive as the benchmark")+ylab("Turnover of accessories and footwear sales for Tasmania(in millions)")

#-------------------------------------------------------------------------





#reading data and creating a time series
retail_ts <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>% pull("29270863") %>% ts(start = c(1982,4), frequency = 12)

#plotting the time series
autoplot(retail_ts)+xlab("Time in years")+ylab("Turnoer (in millions)")+ggtitle("Total turnover for footwear and accessories from 1982 to 2016")   

#seasonal plot
ggseasonplot(retail_ts)+ylab("Turnover of Footwear and accessory in Tasmania (in millions)")+ggtitle("Seasonal variation of turnover for each month from 1982 to 2016")

#subseries plot
ggsubseriesplot(retail_ts)+ylab("Turnover of Footwear and accessory in Tasmania (in millions)")+ggtitle("Sub series plot variation of turnover for each month")

#lag plots
gglagplot(retail_ts)+ylab("Turnover of Footwear and accessory in Tasmania (in millions)")+ggtitle("Lag plot for time series of turnover for Tasmania")

# dcompose the plots
p <- retail_ts %>% stl(s.window = 12,robust = TRUE) 
autoplot(p)+ggtitle("Decomposition of the time series into components")

retail_ts %>% autoplot() + autolayer(trendcycle(p),series="Trend Cycle of data")+autolayer(seasadj(p),series = "Seasanlly adjusted data") +
  ggtitle("Trend Cycle and Seasinally adjusted data in conjunction with time series")





# Ets model
fcastHoltDamp <- ets(retail_ts,model = "MAM")
summary(fcastHoltDamp)
autoplot(fcastHoltDamp)


# checking the residuals 
checkresiduals(fcastHoltDamp)


#  looking at the ets model to suggest data model for the question
ets(retail_ts, model="ZZZ", damped=TRUE, alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=BoxCox.lambda(retail_ts), biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=FALSE) %>% summary()

ets(retail_ts, model="ZZZ", damped=TRUE, alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=BoxCox.lambda(retail_ts), biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=FALSE) %>% checkresiduals()




# Fitting another ets model with MAM
second_method <-  ets(retail_ts,model = "MAM",damped = TRUE)
summary(second_method)
autoplot(second_method)


# Plotting the forecasts and data on the same file
forecasts <- forecast(ets(retail_ts,model = "MAM"),h = 24)
retail_ts %>% autoplot()+autolayer(forecasts,series = "Forecasts")+ggtitle("Forecasting using MAM ets model for 2017 and 2018")+ylab("Sales for footwear and accessories in millions")
write.csv(forecasts$mean,"Point_forecasts.csv")
write.csv(forecasts$upper,"Upper_bound.csv")
write.csv(forecasts$lower,"Lower_bound.csv")


# question 7
last_few_years <- window(retail_ts,start = c(2014,1))
forecasts_for_method <-  forecast(ets(retail_ts,model = "AAA",damped = TRUE),h = 24)
autoplot(last_few_years) + autolayer(forecasts,series = "Forecasts for 17-18 using chosed method") + autolayer(forecasts_for_method,series = "Forecasts for 17-18 using damped method")+ggtitle("Comparisons of forecasts from 2 methods")+ylab("Sales of footwear and accessories in millions")

autoplot(last_few_years) + autolayer(forecasts,series = "Forecasts for 17-18 using chosed method",PI=FALSE) + autolayer(forecasts_for_method,series = "Forecasts for 17-18 using damped method",PI=FALSE)+ggtitle("Comparisons of forecasts from 2 methods")+ylab("Sales of footwear and accessories in millions")


#---------------------------





# reading the data
org_retail_ts <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>% pull("29270863") %>% ts(start = c(1982,4), frequency = 12)


# 1. performing transformation and plotiting the tranfromed time series data.
lambda <- BoxCox.lambda(org_retail_ts)
autoplot(org_retail_ts) + ggtitle("Retail sales for footwear accessories for Tasmania")+ylab("Turnover in Millions")+xlab("Year")
retail_ts <- ts(BoxCox(org_retail_ts,lambda = lambda),start = c(1982,4),frequency = 12)
autoplot(retail_ts)+ggtitle("Transformed time series data with the suggested lambda value")+ylab("Turnover(in millions)")

# performing seasonal and subseries plots
ggseasonplot(retail_ts)
ggsubseriesplot(retail_ts) + ggtitle("Sub series Plot")

# comnbining the actual data , transformed data, the seasonally differenced data and lag differenced data as a facet
cbind("Retail Data" = org_retail_ts,
      "BoxCox Transformed" = retail_ts,
      "Seasonally\n differenced Box_Cox" =
        diff(retail_ts,12),
      "First Differencing of data" = diff(diff(retail_ts,12))
) %>%
  autoplot(facets=TRUE) +
  ylab("Turnover in Millions")+
  xlab("Year")  +
  ggtitle("Monthly Retail for sales of Tasmania")





#perfomring the seasonal differencing and lag differencing and persisting it in a variable. 
final_stat_data <- retail_ts %>% diff(12) %>% diff()

# plotting the ACF and PACF plots
final_stat_data %>% ggtsdisplay()

# looking at the ACF AND PACF, lags at 12 and 24 : AR(2) spike at lag 12 in ACF as well. MA(1)
# fitting an appropriate model in accordance to ACF and PACf plots
arima_model_fit <- Arima(final_stat_data,order = c(3,0,2),seasonal = c(2,0,1))





# checking the residuals of the intially suggested model
checkresiduals(arima_model_fit)



# initiaing alternative models and performing  
arima_1 <- Arima(final_stat_data,order = c(4,0,5),seasonal = c(3,0,1))
arima_2 <- Arima(final_stat_data,order = c(4,0,5),seasonal = c(2,0,1))
arima_3 <- Arima(final_stat_data,order = c(3,1,3),seasonal = c(2,0,2))

# checking the model diagnostics and information criteria scores
arima_1
arima_2
arima_3


# using auto.arima() to suggest a model for us.
model_sugg <- auto.arima(final_stat_data)

# checking the model diagnostics and comparing the information criteria scores
model_sugg

# performing residual diagnostics of the data 
checkresiduals(model_sugg)


# using the auto.arima() to suggest a model for us, but with stepwise and approxiamtion set to FALSE
model_sugg1 <- auto.arima(final_stat_data,stepwise = FALSE,approximation = FALSE)

# checking the residuals of the model.
checkresiduals(model_sugg1)



# splitting the data into train and test data 
test_data <- window(final_stat_data,start = c(2014,1))
train_data <- window(final_stat_data,end = c(2013,12))

#  fitting all the models selected to previosuly to the train data 
train1 <- train_data %>% Arima(order = c(3,0,2),seasonal = c(2,0,1))
train2 <- train_data %>% Arima(order = c(4,0,5),seasonal = c(3,0,1))
train3 <- train_data %>% Arima(order = c(4,0,5),seasonal = c(2,0,1))
train4 <- train_data %>% Arima(order = c(3,1,3),seasonal = c(2,0,2))
train5 <- auto.arima(train_data)

# forecasting on the train data with length equivalent to test data 
test1 <- train1 %>% forecast(h=24)
test2 <- train2 %>% forecast(h=24)
test3 <- train3 %>% forecast(h=24)
test4 <- train4 %>% forecast(h=24)
test5 <- train5 %>% forecast(h=24)


# checking the residuals of the all the models

# performing accuracy of all the models used previously
accuracy(test1,test_data)
accuracy(test2,test_data)
accuracy(test3,test_data)
accuracy(test4,test_data)
accuracy(test5,test_data)


# from inspection we can see that train4, train5 are not good enough. train1 , train2 and train3 come from white noise
# AICc scores suggest train2 and train3 are the best. 
# considering all the paramters we can use train 3 as a good model







# forecasing on the data using the selected method
fore_cast1 <- org_retail_ts %>% Arima(order = c(4,0,5),seasonal = c(2,0,1),lambda = lambda) %>% forecast(h=24)

# plotting the forecasts with intervals in the same plot
autoplot(fore_cast1) +
  autolayer(fore_cast1$mean, series="Forecast") +
  xlab("Year") +
  ylab("Turnover in Millions") +
  guides(fill=guide_legend(title="Prediction interval"))



# reading the new data with latest data observation and plotting them on the same plot
latest_data_ts <- read_xlsx("RetailDataIndividualFull.xlsx", skip=3) %>% pull("29270863") %>% ts(start = c(1982,4), frequency = 12)
last_2_yrs <- window(latest_data_ts,start = c(2017,1))
latest_data_ts %>% autoplot() + autolayer(last_2_yrs) + ggtitle("Retail sales data for Tasmanina including financial years 2017-2018")+xlab("Year")+ylab("Turnover in Millions")



# forecasting on the benchmark method of snaive
fore_bench_snaive <- snaive(InvBoxCox(retail_ts,lambda = lambda),h=24)

# forecasting using ets(MAM) model
fore_ets <-  forecast(ets(InvBoxCox(retail_ts,lambda = lambda),model = "MAM"),h = 24)

# forecasting using ARIMA model
forecasts_arima <- org_retail_ts %>% Arima(order = c(4,0,5),seasonal = c(2,0,1),lambda = lambda) %>% forecast(h=24)

# plotting all the point forecasts in the same plot
autoplot(latest_data_ts)+autolayer(last_2_yrs,series="last_2_years")+autolayer(fore_bench_snaive,series = "Benchmark(snaive)",PI=FALSE)+autolayer(fore_ets,series = "ETS(MAM)",PI=FALSE)+autolayer(forecasts_arima,series = "ARIMA forecast",PI=FALSE)+ ggtitle("Comparisons of forecasts for several models")+xlab("Year")+ylab("Turnover in Millions")



# creating a matrix to hold the tabulated/summarised results
tab_results <- matrix(nrow = 3,ncol = 3)

# creating a split of the latest data 
test_data_compete <- window(latest_data_ts,start = c(2017,1))

# calculating the accuracy and recording them in a variable.
acc_naive <- accuracy(fore_bench_snaive,test_data_compete)
acc_ets <- accuracy(fore_ets,test_data_compete)
acc_arima <- accuracy(forecasts_arima,test_data_compete)

# applying the required results of RMSE, MASE, MAPE in the matrix
tab_results[1,1] <- acc_naive[1,2]
tab_results[1,2] <- acc_naive[1,5]
tab_results[1,3] <- acc_naive[1,6]

tab_results[2,1] <- acc_ets[1,2]
tab_results[2,2] <- acc_ets[1,5]
tab_results[2,3] <- acc_ets[1,6]

tab_results[3,1] <- acc_arima[1,2]
tab_results[3,2] <- acc_arima[1,5]
tab_results[3,3] <- acc_arima[1,6]

rownames(tab_results) <- c("Seasonal_Naive","ETS(MAM)","ARIMA")
colnames(tab_results) <- c("RMSE","MAPE","MASE")



# choosing the top model.

# produce forecasts for the year 2019-2020 using the selected method
forecasts_arima <- latest_data_ts  %>% Arima(order = c(4,0,5),seasonal = c(2,0,1),lambda = lambda) %>% forecast(h=24,level = 80)

# plotting the results in the graph.
autoplot(forecasts_arima) +
  autolayer(forecasts_arima$mean, series="Forecast") +
  xlab("Year") +
  ylab("Turnover in Millions") +
  ggtitle("Forecasts for the year 2019-20 using ARIMA model")+
  guides(fill=guide_legend(title="Prediction interval"))
