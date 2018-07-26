#KNN excerise 1
#to find out bankruptcy
library(caret)
bankCrupt <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Bankruptcy\\bankruptcy.csv")
#Removing irrelevent column from our data set
bankCrupt <- bankCrupt[,c(-1,-3)]
#Since our response variable was a integer we need to change it to a factor first 
#Coz knn required catagorical response variable
bankCrupt$D <- factor(bankCrupt$D)
set.seed(2018)
intrain<-createDataPartition(y=bankCrupt$D,p=0.7,list=FALSE)
training <- bankCrupt[intrain, ]
validation <- bankCrupt[-intrain, ]

# Using knn3 function

fitKNN1 <- knn3(D ~ .,data=training, k=1)
pred.knn1 <- predict(fitKNN1,newdata=validation,type = "class")
tbl_1 <- table(pred.knn1 , validation$D  )
confusionMatrix( tbl_1 )

#####k=3 will be optimal since we got better accuracy############
fitKNN3 <- knn3(D ~ .,data=training, k=3)                       #
pred.knn3 <- predict(fitKNN3,newdata=validation,type = "class") #
tbl_3 <- table(pred.knn3 , validation$D  )                      #
confusionMatrix( tbl_3 )                                        #
#################################################################

fitKNN5 <- knn3(D ~ .,data=training, k=5)
pred.knn5 <- predict(fitKNN5,newdata=validation,type = "class")
tbl_5 <- table(pred.knn5 , validation$D  )
confusionMatrix( tbl_5 )



fitKNN7 <- knn3(D ~ .,data=training, k=7)
pred.knn7 <- predict(fitKNN7,newdata=validation,type = "class")
tbl_7 <- table(pred.knn7 , validation$D  )
confusionMatrix( tbl_7 )

library(pROC)

pred.prob1 <- predict(fitKNN1,newdata=validation,type="prob")
plot.roc(validation$D, pred.prob1[,1], print.auc=TRUE , 
         col="magenta", main="K=1",legacy.axes=TRUE)

pred.prob3 <- predict(fitKNN3,newdata=validation,type="prob" )
plot.roc(validation$D, pred.prob3[,1] , print.auc=TRUE , col="blue", main="K=3",legacy.axes=TRUE )

pred.prob5 <- predict(fitKNN5,newdata=validation,type="prob")
plot.roc(validation$D, pred.prob5[,1] , print.auc=TRUE ,
         col="red" , main="K=5",legacy.axes=TRUE)

pred.prob7 <- predict(fitKNN7,newdata=validation,type="prob")
plot.roc(validation$D, pred.prob7[,1] , print.auc=TRUE , col="green" , main="K=7",legacy.axes=TRUE)

# All in One

plot.roc(validation$D, pred.prob1[,1] , col="magenta", 
         main = "ROC for Each K",legacy.axes=TRUE)
#just put add = true to draw all the lines in one graph
plot.roc(validation$D, pred.prob3[,1] , col="blue" , add=TRUE )

plot.roc(validation$D, pred.prob5[,1] , col="red" , add=TRUE )

plot.roc(validation$D, pred.prob7[,1] , col="green" , add=TRUE )

legend("bottomright", legend=c("K=1","K=3", "K=5","K=7"),
       col=c("magenta", "blue", "red","green"), lwd=3)

