#Random Forest on concrete dataset
concrete <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Concrete Strength\\Concrete_Data.csv")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=concrete$Strength , 
                             p=0.7,list=FALSE)

training   <- concrete[ intrain , ]
validation <- concrete[-intrain , ]


library(randomForest)
model.RF <- randomForest(Strength ~ . , data = training ,
                         na.action=na.roughfix, importance=TRUE)

model.RF

#importance(model.RF)
varImpPlot(model.RF)
importance(model.RF,type = 2)

pred.RF <- predict(model.RF, newdata = validation[,-9])
postResample(pred.RF , validation$Strength)
pred <- predict(model.RF, newdata = validation[,-9])

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

MAPE(validation$Strength , pred.RF)

RMSPE<- function(y, yhat) {
  sqrt(mean((y-yhat)/y)^2)
}

RMSPE(validation$Strength , pred.RF)

#with tuning
myGrid <- data.frame(mtry=seq(1,4))
tuned.model <- train(Strength~.,data = training, 
                     method="rf",tuneGrid=myGrid)
plot(tuned.model)

pred.RF <- predict(tuned.model, newdata = validation[,-9],
                   type = "raw")

postResample(pred = pred.RF, obs = validation$Strength)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

MAPE(validation$Strength , pred.RF)

RMSPE<- function(y, yhat) {
  sqrt(mean((y-yhat)/y)^2)
}

RMSPE(validation$Strength , pred.RF)


##############
myGrid <- data.frame(mtry=seq(1,4))
tuned.model <- train(Strength~.,data = training, 
                     method="cforest",tuneGrid=myGrid)
plot(tuned.model)

pred.RF <- predict(tuned.model, newdata = validation[,-9],
                   type = "raw")

postResample(pred = pred.RF, obs = validation$Strength)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

MAPE(validation$Strength , pred.RF)

RMSPE<- function(y, yhat) {
  sqrt(mean((y-yhat)/y)^2)
}

RMSPE(validation$Strength , pred.RF)