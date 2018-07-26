#Use iris data
#create a neural net model using nnet and neuralnet
#note:iris data get lazy loaded when you call it, it will promise the user that it will load and loads when you call it
data("iris")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=iris$Species , p=0.7,list=FALSE)
training   <- iris[ intrain , ]
validation <- iris[-intrain , ]

library(nnet)

fit.nn <- nnet(Species ~ . , data = iris , subset = intrain,size=5,rang = 0.1,
               decay = 5e-4, maxit = 200)
pred.nn <- factor(predict(fit.nn, newdata = validation, 
                          type = "class"), 
                  levels = c("setosa","versicolor","virginica"))
confusionMatrix(pred.nn, validation$Species)


