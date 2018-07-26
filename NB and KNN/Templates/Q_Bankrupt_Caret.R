brupt <- read.csv("F:\\Statistics\\Cases\\Qualitative_Bankruptcy\\Qualitative_Bankruptcy.data.txt")

library(caret)

set.seed(333)
intrain <- createDataPartition(y=brupt$Class,p=0.7,list = FALSE)

training <- brupt[intrain,   ]
validation <- brupt[-intrain,]


classifier <- train(training[,1:6], training[,7],method = "nb") 

PredY <- predict(classifier, newdata=validation[,-7], type="raw")

tbl <- table(PredY, validation[,7],dnn=list('predicted','actual'))
confusionMatrix(tbl)

library(pROC)
ProbY <- predict.train(classifier, newdata = validation[,-7] , type="prob")
plot.roc(validation[,7],ProbY[,1],legacy.axes=TRUE,print.auc=TRUE )


#### Predicting

tp <- read.csv("E:\\Statistics\\Cases\\Qualitative_Bankruptcy\\ToPredict.csv")
predBR <- predict.train(classifier, newdata = tp,type = "raw")
predicted <- data.frame(tp,predBR)
