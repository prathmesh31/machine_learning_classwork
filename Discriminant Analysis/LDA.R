library(mlbench)
data("Glass")

Glass$Type <- factor(Glass$Type)

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=Glass$Type , p=0.7,list=FALSE)

training   <- Glass[ intrain , ]
validation <- Glass[-intrain , ]

library(MASS)
fit.lda <- lda(Type ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)

confusionMatrix(pred.lda$class,validation$Type)

#######################################################################

StdGlass <- as.data.frame(scale(Glass[,-10]))
WholeStd <- data.frame(StdGlass,Type=Glass$Type)

library(caret)
set.seed(1992)
intrain<-createDataPartition(y=WholeStd$Type , p=0.7,list=FALSE)

training   <- WholeStd[ intrain , ]
validation <- WholeStd[-intrain , ]

library(MASS)
fit.lda <- lda(Type ~ . , data = training)

pred.lda <- predict(fit.lda , newdata = validation)

confusionMatrix(pred.lda$class,validation$Type)
