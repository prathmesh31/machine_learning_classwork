#using svm on cancer dataset

cancer <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Wisconsin\\BreastCancer.csv")
cancer <- cancer[,-1]

#creating data partition
library(caret)
set.seed(2018)
intrain<-createDataPartition(y=cancer$Class , p=0.7,list=FALSE)
training   <- cancer[ intrain , ]
validation <- cancer[-intrain , ]

library(e1071)
#training the model
#type=c indicates it's type is class
fit.svm <- svm(Class~., type="C",data=training, kernel="linear")
svm.pred <- predict(fit.svm, newdata=validation)
svm.perf <- table(svm.pred, validation$Class, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf) 
#Tuning the cost of our model here
tune.out <- tune(svm,Class~.,data = training, kernel="linear",
                 ranges=list(cost=c(seq(0.1,2,by=0.1))))
summary(tune.out)
#################################################################################
tune.outR <- tune(svm,Class~.,data = training, kernel="radial",
                  ranges=list(gamma=c(seq(0.1,2,by=0.1)),
                              cost=c(seq(0.1,2,by=0.1))))
summary(tune.outR)
tune.outR$best.model
svm.pred <- predict(tune.outR$best.model, newdata=validation)
svm.perf <- table(svm.pred, validation$Class, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf,positive = "Malignant") 
###########################################################################################
tune.outP <- tune(svm,Class~.,data = training, kernel="polynomial",
                  ranges=list(degree=c(seq(1,5,by=1)),
                              cost=c(seq(0.1,2,by=0.1))))
summary(tune.outP)
tune.outP$best.model
svm.pred <- predict(tune.outP$best.model, newdata=validation)
svm.perf <- table(svm.pred, validation$Class, dnn=c("Predicted","Actual"))
confusionMatrix(svm.perf,positive = "Malignant") 