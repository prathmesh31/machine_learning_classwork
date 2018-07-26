#Using LDA and QDA
#finding out bankcrupt

#loading data set
bank <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Bankruptcy\\Bankruptcy.csv")
bank <- bank[,c(-1,-3)]
#making the response variable from int to a factor type
bank$D <- factor(bank$D, levels = c(0,1), 
                 labels = c("bankrupt","Not bankrupt"))
#creating data partition
library(caret)
set.seed(2018)
intrain<-createDataPartition(y=bank$D , p=0.7,list=FALSE)

training   <- bank[ intrain , ]
validation <- bank[-intrain , ]

#applying LDA function
library(MASS)
fit.lda <- lda(D ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)
#pred.lda gives you 3 coloumns which are class(a factor),posterrior probability and x(scores)
confusionMatrix(pred.lda$class,validation$D)

#using qda
fit.qda <- qda(D ~ . , data = training)

pred.qda <- predict(fit.qda , newdata = validation)

confusionMatrix(pred.qda$class,validation$D)

