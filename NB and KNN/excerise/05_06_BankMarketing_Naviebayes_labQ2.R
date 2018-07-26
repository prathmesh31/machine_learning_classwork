#excerise 2
#To predict whether a customer will subscribed a term deposit or not
#using Naive bayes

#loading the data set
#use either read.csv2 or just add parameter sep=";" in the read.csv function
bank <- read.csv2("F:\\ML_Class\\Machine Learning\\Cases\\bank\\bank-full.csv")


library(caret)
#we set seed to make the random variable consistent
#you will get same result for same seed value 
set.seed(2018)
#taking train model (here we take 70% of the total data) and response varible as Class cloumn of dataset
intrain <- createDataPartition(y=bank$y,p=0.7,list = FALSE)

training <- bank[intrain,   ]
validation <- bank[-intrain,]

library(e1071)

#we are creating our prediction model with naiveBayes
#                   <independent variables>,<dependent variable>
classifier <- naiveBayes(training[,1:16], training[,17]) 

#predict() will create the prediction vector
#type = class will show the lables
PredY <- predict(classifier, newdata=validation[,-17], 
                 type="class")
#type = raw will give probabilty
PredYProb <- predict(classifier, newdata=validation[,-17],
                     type="raw")


#it will create a frequency table for our predicted data vs actual data
tbl <- table(PredY, validation[,17],dnn=list('predicted','actual'))

#by default it will take the positive class which lable came first in alphabatical order
confusionMatrix(tbl)
#selecting +ve class which we desire
confusionMatrix(tbl,positive = "yes")

library(pROC)
plot.roc(validation[,17],PredYProb[,2],
         legacy.axes=TRUE,print.auc=TRUE )

#to plot the ROC graph
#    <Actual value of response variable>,<Predicted value of response variable by our model>
objROC <- plot.roc(validation[,17],PredYProb[,2],
                   legacy.axes=TRUE,print.auc=TRUE )  #PredYprob 2nd column have the +ve class sucess probablity (may change)
names(objROC)
objROC$thresholds
objROC$sensitivities
objROC$specificities

# Predicting
tsttel <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\bank\\bank-full.csv")
PredY <- predict(classifier, newdata=tsttel, type="class")

predt <- data.frame(tsttel,PredY)
