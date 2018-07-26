#Using Ctree
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

library(rpart)

dtree <- rpart(D ~ ., data=training, 
               method="class")
#we will select the cp value which after which we are getting constant/steady value of error
dtree$cptable
plotcp(dtree)
#pruned tree by manually selecting the
dtree.pruned <- prune(dtree, cp= 0.08510638  ) 

######################Descsion tree########################
library(rpart.plot)
##ploting tree
rpart.plot(dtree , type=4, extra=1,main="Before Pruning")
rpart.plot(dtree.pruned, type = 4, extra = 1,main="After Pruning")



dtree.pred <- predict(dtree, validation, type="class")
tbl <- table(dtree.pred, validation$D,
             dnn=c("Predicted", "Actual"))
confusionMatrix(tbl)


dtree.pred1 <- predict(dtree.pruned, validation, type="class")
tbl_pruned <- table(dtree.pred1, validation$D, 
                    dnn=c("Predicted", "Actual"))
confusionMatrix(tbl_pruned)


##### Conditional Inference Tree ########

library(party)
fit.ctree <- ctree(D~., data=training)

plot(fit.ctree, main="Conditional Inference Tree",
     type="simple")

plot(fit.ctree, main="Conditional Inference Tree",
     type="extended")

ctree.pred <- predict(fit.ctree, validation, type="response")
ctree.perf <- table( ctree.pred,validation$D ,
                     dnn=c("Predicted", "Actual"))

confusionMatrix(ctree.perf)
