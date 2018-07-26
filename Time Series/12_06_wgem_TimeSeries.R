wgem <- read.csv("F:\\ML_Class\\Machine Learning\\Datasets\\WGEM-IND_CPTOTNSXN.csv")

#coverting it into time series data

wgem_ts <- ts(wgem$Value,frequency = 1,start = 1987)

# Number of Observations in Validation data
nValid <- 5
# Number of Observations in Training data
nTrain <- 31 - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(wgem_ts, start = 1987, end = 1987 + nTrain - 1)
valid.ts <- window(wgem_ts, start = 1987 + nTrain)

library(forecast)
#### Smoothing Methods
model_ets <- ets(train.ts)
pred.ets <- forecast(model_ets,h=nValid)
accuracy(pred.ets$mean, valid.ts)

## Arima model
model_arima <- auto.arima(train.ts)
pred.arima <- forecast(model_arima,h=nValid)
accuracy(pred.arima$mean,valid.ts)


##########Predicting for next 2 years
#taking whole data into consideration
model_ets <-ets(wgem_ts)
pred.ets <- forecast(model_ets,h=nValid)
## Arima model
model_arima <- auto.arima(train.ts)
pred.arima <- forecast(model_arima,h=nValid)
accuracy(pred.arima$mean,valid.ts)
