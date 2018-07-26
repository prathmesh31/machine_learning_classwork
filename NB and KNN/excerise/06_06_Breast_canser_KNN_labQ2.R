#KNN excerise 2
#to find out breast cancer paitent
library(caret)
cancer <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Wisconsin\\BreastCancer.csv")
#Removing irrelevent column from our data set
cancer <- cancer[,-1]

set.seed(2018)
intrain<-createDataPartition(y=cancer$Class,p=0.7,list=FALSE)
training <- cancer[intrain, ]
validation <- cancer[-intrain, ]

# Using knn3 function
#for knn3
#here in predict in validation we will still get the prediction even we pass all variable it will take only the features
fitKNN1 <- knn3(Class ~ .,data=training, k=1)
pred.knn1 <- predict(fitKNN1,newdata=validation,type = "class")
tbl_1 <- table(pred.knn1 , validation$Class  )
confusionMatrix( tbl_1 ,positive = "Malignant")


fitKNN3 <- knn3(Class ~ .,data=training, k=3)                       
pred.knn3 <- predict(fitKNN3,newdata=validation,type = "class") 
tbl_3 <- table(pred.knn3 , validation$Class  )                      
confusionMatrix( tbl_3,positive = "Malignant")                                        

####Here K= 5 is optimal since accuracy is dropping after k = 5#####
fitKNN5 <- knn3(Class ~ .,data=training, k=5)                     ##
pred.knn5 <- predict(fitKNN5,newdata=validation,type = "class")   ##
tbl_5 <- table(pred.knn5 , validation$Class  )                    ##
confusionMatrix( tbl_5,positive = "Malignant" )                   ##
####################################################################


fitKNN7 <- knn3(Class ~ .,data=training, k=7)
pred.knn7 <- predict(fitKNN7,newdata=validation,type = "class")
tbl_7 <- table(pred.knn7 , validation$Class  )
confusionMatrix( tbl_7,positive = "Malignant")

library(pROC)

pred.prob1 <- predict(fitKNN1,newdata=validation,type="prob")
                  #****here our +ve class is at 2nd column****
plot.roc(validation$Class, pred.prob1[,2], print.auc=TRUE , 
         col="magenta", main="K=1",legacy.axes=TRUE)

pred.prob3 <- predict(fitKNN3,newdata=validation,type="prob" )
plot.roc(validation$Class, pred.prob3[,2] , print.auc=TRUE , col="blue", main="K=3",legacy.axes=TRUE )

pred.prob5 <- predict(fitKNN5,newdata=validation,type="prob")
plot.roc(validation$Class, pred.prob5[,2] , print.auc=TRUE ,
         col="red" , main="K=5",legacy.axes=TRUE)

pred.prob7 <- predict(fitKNN7,newdata=validation,type="prob")
plot.roc(validation$Class, pred.prob7[,2] , print.auc=TRUE , col="green" , main="K=7",legacy.axes=TRUE)

# All in One

plot.roc(validation$Class, pred.prob1[,2] , col="magenta", 
         main = "ROC for Each K",legacy.axes=TRUE)
#just put add = true to draw all the lines in one graph
plot.roc(validation$Class, pred.prob3[,2] , col="blue" , add=TRUE )

plot.roc(validation$Class, pred.prob5[,2] , col="red" , add=TRUE )

plot.roc(validation$Class, pred.prob7[,2] , col="green" , add=TRUE )

legend("bottomright", legend=c("K=1","K=3", "K=5","K=7"),
       col=c("magenta", "blue", "red","green"), lwd=3)

