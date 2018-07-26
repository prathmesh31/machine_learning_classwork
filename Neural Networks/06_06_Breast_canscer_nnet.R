cancer <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Wisconsin\\BreastCancer.csv")
cancer <- cancer[,-1]

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=cancer$Class , p=0.7,list=FALSE)
training   <- cancer[ intrain , ]
validation <- cancer[-intrain , ]
#using nnet
library(nnet)
fit.nn <- nnet(Class ~ . , data = cancer , subset = intrain,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
#provid the the predicted to factor that map our response variable
pred.nn <- factor(predict(fit.nn, newdata = validation, 
                          type = "class"), 
                  levels = c("Benign","Malignant"))
confusionMatrix(pred.nn, validation$Class)


