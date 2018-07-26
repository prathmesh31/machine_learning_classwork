#find Breast cancer paitent
#QDA and LDA
cancer <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Wisconsin\\BreastCancer.csv")
cancer <- cancer[,-1]

#creating data partition
library(caret)
set.seed(2018)
intrain<-createDataPartition(y=cancer$Class , p=0.7,list=FALSE)
training   <- cancer[ intrain , ]
validation <- cancer[-intrain , ]

#applying LDA function
library(MASS)
fit.lda <- lda(Class ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)
#pred.lda gives you 3 coloumns which are class(a factor),posterrior probability and x(scores)
confusionMatrix(pred.lda$class,validation$Class)

#using qda
fit.qda <- qda(Class ~ . , data = training)

pred.qda <- predict(fit.qda , newdata = validation)

confusionMatrix(pred.qda$class,validation$Class)

