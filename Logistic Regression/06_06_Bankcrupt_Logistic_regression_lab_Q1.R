bank <- read.csv("F:\\ML_Class\\Machine Learning\\Cases\\Bankruptcy\\Bankruptcy.csv")
bank <- bank[,c(-1,-3)]

bank$D <- factor(bank$D, levels = c(0,1), 
                  labels = c("bankrupt","Not bankrupt"))


fit.lg <- glm(D ~ . , 
              data = bank , family = binomial())
summary(fit.lg)
exp(coef(fit.lg))



###################################################################################

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=bank$D , p=0.7,list=FALSE)
training   <- bank[ intrain , ]
validation <- bank[-intrain , ]
#applying logistic regression using glm function
fit.lg <- glm(D ~ . , data = training , family = binomial())
pred.lg <- predict(fit.lg, newdata = validation , 
                   type = "response")
#giving lables to our response variables
pred.lg.cat <- factor(ifelse(pred.lg < 0.5 , "bankrupt" , "Not bankrupt"), 
                      levels = c("bankrupt","Not bankrupt"))

confusionMatrix(pred.lg.cat , validation$D)
confusionMatrix(pred.lg.cat , validation$D, 
                positive = "bankrupt")

library(pROC)
plot.roc(validation$D, pred.lg, print.auc=TRUE , 
         col="magenta", main="Logistic Regression",
         legacy.axes=TRUE)
