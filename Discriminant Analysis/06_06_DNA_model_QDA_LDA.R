#Use DNA from mlbench library
#find out model for LDA and QDA
library(mlbench)
data(DNA)
library(caret)
set.seed(2018)
intrain<-createDataPartition(y=DNA$Class , p=0.7,list=FALSE)
training   <- DNA[ intrain , ]
validation <- DNA[-intrain , ]

#applying LDA function
library(MASS)
fit.lda <- lda(Class ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)
#pred.lda gives you 3 coloumns which are class(a factor),posterrior probability and x(scores)
confusionMatrix(pred.lda$class,validation$Class)

#using qda
###########################################################################
#*********NOTE HERE WE CAN'T CALCUATE QDA HERE BECAUSE INVERSE DOES"NT ####
#****EXITS FOR OUR RESPONSE VARIABLE IN OUR DATA*******# ##################
#fit.qda <- qda(Class ~ . , data = training)           ####################
#                                                      ####################
#pred.qda <- predict(fit.qda , newdata = validation)   ####################
#                                                      ####################
#confusionMatrix(pred.qda$class,validation$Class)      ####################