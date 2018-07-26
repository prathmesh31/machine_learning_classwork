#Regression Tree

#loading concrete dataset
concrete <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Concrete Strength\\Concrete_Data.csv")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=concrete$Strength , 
                             p=0.7,list=FALSE)

training   <- concrete[ intrain , ]
validation <- concrete[-intrain , ]

library(rpart)
fitRT <- rpart( Strength ~ . , data = training , method = "anova" )

library(rpart.plot)

rpart.plot(fitRT,type = 4,extra = 1, digits = 5)

pred.RT <- predict(fitRT,newdata = validation )

postResample(pred.RT , validation$Strength)
# OR
RMSE <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}
RMSE(validation$Strength, pred.RT)

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}
MAPE(validation$Strength , pred.RT)

RMSPE<- function(y, yhat) {
  sqrt(mean((y-yhat)/y)^2)
}
RMSPE(validation$Strength , pred.RT)

##### Pruning ######

plotcp(fitRT)
fitRT$cptable


fitRT.pruned <- prune(fitRT , cp= 0.01040598   )

rpart.plot(fitRT.pruned,type = 4,extra = 1, digits = 5)

pred.RT.pruned <- predict(fitRT.pruned , newdata = validation)

postResample(pred.RT.pruned , validation$Strength)
MAPE(validation$Strength , pred.RT.pruned)  
RMSPE(validation$Strength , pred.RT.pruned)

###### Conditional Inference Tree ########

library(party)

fitCT <- ctree(Strength ~ . , data = training )

plot(fitCT , type="simple")

plot(fitCT , type="extended" )

pred.CT <- predict(fitCT , newdata=validation)

postResample(pred.CT , validation$Strength)
MAPE(validation$Strength , pred.CT)  
RMSPE(validation$Strength , pred.CT)  


############### LM ####################

fitLM <- lm(Strength ~ . , data = training )
pred.LM <- predict(fitLM , newdata=validation)
postResample(pred.LM , validation$Strength)
MAPE(validation$Strength , pred.LM)  
RMSPE(validation$Strength , pred.LM)  

fit.svm <- svm(Strength~., type="eps",data=training)
svm.pred <- predict(fit.svm, newdata=validation)
postResample(svm.pred,validation$Strength)
MAPE(validation$Strength , svm.pred)  
RMSPE(validation$Strength , svm.pred) 