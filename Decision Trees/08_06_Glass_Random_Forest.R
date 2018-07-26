
library(mlbench)
data(Glass)

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type, p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]
#######################################################################################


library(randomForest)
model.RF <- randomForest(Type ~ . , data = training ,
                         na.action=na.roughfix, importance=TRUE)

model.RF

#importance(model.RF)
varImpPlot(model.RF)
importance(model.RF,type = 2)

pred.RF <- predict(model.RF, newdata = validation[,-10])
postResample(pred.RF , validation$Type)
pred <- predict(model.RF, newdata = validation[,-10])

confusionMatrix( table(pred, validation$Type) )

#with tuning
myGrid <- data.frame(mtry=seq(1,4))
tuned.model <- train(Type~.,data = training, 
                     method="rf",tuneGrid=myGrid)
plot(tuned.model)

pred.RF <- predict(tuned.model, newdata = validation,
                   type = "raw")

postResample(pred = pred.RF, obs = validation$Type)




################## cforest ############################
library(party)
model.RF <- cforest(Type ~ ., data = training, 
                    control = cforest_unbiased(ntree = 50))

model.RF

pred.RF <- predict(model.RF, newdata = validation[,-10])

postResample(pred.RF , validation$Type)
confusionMatrix(pred.RF, validation$Type) 

################ Ranger ###################
library(ranger)
model.RF <- ranger(Type ~ . , data = training ,importance='impurity')

model.RF
#plot(model.RF)
importance(model.RF,type = 2)
#varImpPlot(model.RF)
model.RF$variable.importance


pred <- predict(model.RF, data = validation[,-10])

confusionMatrix( pred$predictions, validation$Type)
