#SVM in glass dataset
library(mlbench)
data(Glass)

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type, p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

library(e1071)
#training the model
#type=c indicates it's type is class
#without tuning
fit.svm <- svm(Type~., type="C",data=training, kernel="linear")
svm.pred <- predict(fit.svm, newdata=validation)
svm.perf <- table(svm.pred, validation$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 

#Tuning the cost of our model here for polynomial
#with tuning
tune.out <- tune(svm,Type~.,data = training, kernel="linear",
                 ranges=list(cost=c(seq(0.1,2,by=0.1))))
summary(tune.out)
svm.pred <- predict(tune.out$best.model, newdata=validation)
svm.perf <- table(svm.pred, validation$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 
######################################################################################
#for radial
#with tuning
tune.outR <- tune(svm,Type~.,data = training, kernel="radial",
                  ranges=list(gamma=c(seq(0.1,2,by=0.1)),
                              cost=c(seq(0.1,2,by=0.1))))
summary(tune.outR)
tune.outR$best.model
svm.pred <- predict(tune.outR$best.model, newdata=validation)
svm.perf <- table(svm.pred, validation$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf,positive = "Malignant") 
#without tuning
fit.svm <- svm(Type~., type="C",data=training, kernel="radial")
svm.pred <- predict(fit.svm, newdata=validation)
svm.perf <- table(svm.pred, validation$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 
###########################################################################################
#for polynomial
#with tuning
tune.outP <- tune(svm,Type~.,data = training, kernel="polynomial",
                  ranges=list(degree=c(seq(1,5,by=1)),
                              cost=c(seq(0.1,2,by=0.1))))
summary(tune.outP)
tune.outP$best.model
svm.pred <- predict(tune.outP$best.model, newdata=validation)
svm.perf <- table(svm.pred, validation$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 
#without tuning
fit.svm <- svm(Type~., type="C",data=training, kernel="polynomial")
svm.pred <- predict(fit.svm, newdata=validation)
svm.perf <- table(svm.pred, validation$Type, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 