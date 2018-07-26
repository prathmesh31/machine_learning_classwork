#Dtree and ctree in glass dataset
library(mlbench)
data(Glass)

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=Glass$Type, p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

library(rpart)

dtree <- rpart(Type ~ ., data=training, 
               method="class")
#we will select the cp value which after which we are getting constant/steady value of error
dtree$cptable
plotcp(dtree)
#pruned tree by manually selecting the
dtree.pruned <- prune( dtree, cp= 0.05050505  ) 

######################Descsion tree########################
library(rpart.plot)
##ploting tree
rpart.plot(dtree , type=4, extra=1,main="Before Pruning")
rpart.plot(dtree.pruned, type = 4, extra = 1,main="After Pruning")



dtree.pred <- predict(dtree, validation, type="class")
tbl <- table(dtree.pred, validation$Type,
             dnn=c("Predicted", "Actual"))
confusionMatrix(tbl)


dtree.pred1 <- predict(dtree.pruned, validation, type="class")
tbl_pruned <- table(dtree.pred1, validation$Type, 
                    dnn=c("Predicted", "Actual"))
confusionMatrix(tbl_pruned,positive = "pos")


##### Conditional Inference Tree ########

library(party)
fit.ctree <- ctree(Type~., data=training)

plot(fit.ctree, main="Conditional Inference Tree",
     type="simple")

plot(fit.ctree, main="Conditional Inference Tree",
     type="extended")

ctree.pred <- predict(fit.ctree, validation, type="response")
ctree.perf <- table( ctree.pred,validation$Type ,
                     dnn=c("Predicted", "Actual"))

confusionMatrix(ctree.perf,positive = "pos")

