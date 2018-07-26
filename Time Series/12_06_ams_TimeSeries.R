amf <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\time series\\AMFI-131057.csv")

#coverting it into time series data
#since we can'nt have daily data we considered weekly data by specifying frequency =7
amfNAV <- ts(amf$Net.Asset.Value,frequency = 7)

# Number of Observations in Validation data
nValid <- 12
# Number of Observations in Training data
nTrain <- 258 - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(amfNAV, start = c(1,1), end = c(1, nTrain + 1))
valid.ts <- window(amfNAV, start = c(1, nTrain + 2))

library(forecast)
#### Smoothing Methods
model_ets <- ets(train.ts)
pred.ets <- forecast(model_ets,h=nValid)
accuracy(pred.ets$mean, valid.ts)

## Arima model
model_arima <- auto.arima(train.ts)
pred.arima <- forecast(model_arima,h=nValid)
accuracy(pred.arima$mean,valid.ts)
#############################################################################################

##############################################################################################
amfSP <- ts(amf$Sale.Price,frequency = 7)

# Number of Observations in Validation data
nValid <- 12
# Number of Observations in Training data
nTrain <- 258 - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(amfSP, start = c(1,1), end = c(1, nTrain + 1))
valid.ts <- window(amfSP, start = c(1, nTrain + 2))

library(forecast)
#### Smoothing Methods
model_ets <- ets(train.ts)
pred.ets <- forecast(model_ets,h=nValid)
accuracy(pred.ets$mean, valid.ts)

## Arima model
model_arima <- auto.arima(train.ts)
pred.arima <- forecast(model_arima,h=nValid)
accuracy(pred.arima$mean,valid.ts)