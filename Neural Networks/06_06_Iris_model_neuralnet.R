#using neuralnet
data("iris")

set.seed(2018)
intrain<-createDataPartition(y=iris$Species , p=0.7,list=FALSE)
training   <- iris[ intrain , ]
validation <- iris[-intrain , ]

n <- names(training)
#since we had multiple catageries we have to make <responseVariable<factor1>+....+<responseVariable<factorN>
f <- as.formula(paste("Speciessetosa+Speciesversicolor+Speciesvirginica ~", paste(n[!n %in% "Species"], 
                                         collapse = " + ")))

library(neuralnet)
#since we can use data frame in the neuralnet(), we can use matrix
#we excluded the first auto generated column which created automatical when we generate matrix 
mat <- model.matrix(~.-1, data = training)
## build model on training dataset; set hidden layers of 4 & 2
model.neural <- neuralnet(formula = f, data = mat, stepmax = 1e+9,
                          linear.output = F, hidden = c(4,2))
plot(model.neural)
val <- as.matrix(validation[,-5])
## use the above model on testing dataset to predict
pred.neural <- compute(model.neural,validation[,-5])
str(pred.neural)
#with help of generate out put we will convert 
#..the response matrix(which is created when we create matrix i.e it 
#...will append response variable with it's fators and make a new column)
#....into the labels 
generateOutput <- function(input){
  gtypes <- c("setosa","versicolor","virginica")
  l <-length(input)
  for (i in 1:l) {
    if(input[i] == max(input)) output <- gtypes[i] #what ever spices have the max value that will return by the funvtion
  }
  output
}

pred.class <- factor(apply(pred.neural$net.result, 1, generateOutput))

confusionMatrix(as.factor(pred.class),
                as.factor(validation$Species))