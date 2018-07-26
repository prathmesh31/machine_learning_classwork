library(Quandl)

QD <- Quandl("YALE/RBCI", api_key="qe7EP2xLG7u95Xofv-TN",type = "ts")

q_ts <- ts(QD$`Long Rate`,frequency = 1,start = 1890)
# Number of Observations in Validation data
nValid <- 16
# Number of Observations in Training data
nTrain <- 121 - nValid
# Training data and Validation data partitioned using window()
train.ts <- window(q_ts, start = 1890, end = 1890+ nTrain - 1)
valid.ts <- window(q_ts, start = 1890 + nTrain)

library(forecast)
#### Smoothing Methods
model_ets <- ets(train.ts)
pred.ets <- forecast(model_ets,h=nValid)
accuracy(pred.ets$mean, valid.ts)
