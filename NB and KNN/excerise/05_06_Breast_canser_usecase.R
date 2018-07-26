#To predict Breast cancer paitient
#using Naive bayes

#loading the data set
cancer <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Cancer\\Cancer.csv")
cancer <- cancer[,-1]

library(caret)
#we set seed to make the random variable consistent
set.seed(2018)
#taking train model (here we take 70% of the total data) and response varible as Class cloumn of dataset
intrain <- createDataPartition(y=cancer$Class,p=0.7,list = FALSE)

training <- cancer[intrain,   ]
validation <- cancer[-intrain,]

library(e1071)

#we are creating our prediction model with naiveBayes
#                   <independent variables>,<dependent variable>
classifier <- naiveBayes(training[,1:9], training[,10]) 

#predict() will create the prediction vector
#type = class will show the lables
PredY <- predict(classifier, newdata=validation[,-10], 
                 type="class")
#type = raw will give probabilty
PredYProb <- predict(classifier, newdata=validation[,-10],
                     type="raw")


#it will create a frequency table for our predicted data vs actual data
tbl <- table(PredY, validation[,10],dnn=list('predicted','actual'))

#by default it will take the positive class which lable came first in alphabatical order
confusionMatrix(tbl)
#selecting +ve class which we desire
confusionMatrix(tbl,positive = "recurrence-events")

library(pROC)
plot.roc(validation[,10],PredYProb[,2],
         legacy.axes=TRUE,print.auc=TRUE )

#to plot the ROC graph
objROC <- plot.roc(validation[,10],PredYProb[,2],
                   legacy.axes=TRUE,print.auc=TRUE )
names(objROC)
objROC$thresholds
objROC$sensitivities
objROC$specificities

# Predicting
tsttel <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Cancer\\Cancer.csv")
PredY <- predict(classifier, newdata=tsttel, type="class")

predt <- data.frame(tsttel,PredY)
