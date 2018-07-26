library(caret)
brupt <- read.csv("F:/Statistics/Cases/Bankruptcy/Bankruptcy.csv")

set.seed(2018)
intrain<-createDataPartition(y=brupt$D,p=0.7,list=FALSE)
training <- brupt[intrain,-c(1,3) ]
validation <- brupt[-intrain,-c(1,3) ]

n <- names(training)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], 
                                   collapse = " + ")))
f

library(neuralnet)
mat <- model.matrix(~.-1, data = training)
## build model on training dataset; set hidden layers of 4 & 2
model.neural <- neuralnet(formula = f, data = mat, stepmax = 1e+9,
                          linear.output = F, hidden = c(4,2))
plot(model.neural)
val <- as.matrix(validation[,-1])
## use the above model on testing dataset to predict
pred.neural <- compute(model.neural,validation[,-1])
str(pred.neural)
pred.class <- ifelse(pred.neural$net.result[,1]<0.5, 0,1)

confusionMatrix(as.factor(pred.class),
                as.factor(validation$D))
