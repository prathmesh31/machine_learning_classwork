cancer <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Wisconsin\\BreastCancer.csv")
cancer <- cancer[,-1]

#Since we have only two catagories in our response variable
#..we will replace the +ve respose factor with the 1 anything else
#...will be replace by 0, hence no new column would be created when
#....we covert it to matrix thus we have not to write (in this case)"ClassBenign","ClassMalignant"
cancer$Class = ifelse(cancer$Class == "Benign",0,1)

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=cancer$Class , p=0.7,list=FALSE)
training   <- cancer[ intrain , ]
validation <- cancer[-intrain , ]


#using neuralnet
n <- names(training)
f <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], 
                                       collapse = " + ")))

library(neuralnet)
#since we can use data frame in the neuralnet(), we can use matrix
#we excluded the first auto generated column which created automatical when we generate matrix 
mat <- model.matrix(~.-1, data = training)
## build model on training dataset; set hidden layers of 4 & 2
model.neural <- neuralnet(formula = f, data = mat, stepmax = 1e+9,
                          linear.output = F, hidden = c(4,2))
plot(model.neural)
val <- as.matrix(validation[,-10])
## use the above model on testing dataset to predict
pred.neural <- compute(model.neural,validation[,-10])
str(pred.neural)
pred.class <- ifelse(pred.neural$net.result[,1]<0.5, "Benign","Malignant")
#here we are turning the Class back from 0,1 to "Benign","Malignant"
validation$Class = factor(validation$Class,
                          levels = c(0,1),
                          labels = c("Benign","Malignant"))
confusionMatrix(as.factor(pred.class),
                as.factor(validation$Class))
