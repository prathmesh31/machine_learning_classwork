imageSeg <- read.csv("F:/Statistics/Cases/Image Segmentation/Image_Segmention.csv")

library(caret)
set.seed(2018)
intrain<-createDataPartition(y=imageSeg$Class,p=0.7,list=FALSE)

training   <- imageSeg[ intrain , ]
validation <- imageSeg[-intrain , ]

library(xgboost)
mat_train <- as.matrix(training[,-1])
mat_validation <- as.matrix(validation[,-1])
lbl_spec <- as.integer(training$Class)-1
model.xgb <- xgboost(data=mat_train,label =lbl_spec ,
                     nrounds = 10,
                     num_class=7, objective="multi:softmax")

pred.xgb <- predict(model.xgb,newdata=mat_validation)
lbl_spec_val <- factor(pred.xgb,
                       labels = levels(imageSeg$Class))
confusionMatrix(lbl_spec_val,validation$Class)

myGrid=data.frame(nrounds=seq(10,40,by = 5),
                  max_depth=seq(2,8),
                  gamma=seq(0.1,0.7,by = 0.1),
                  colsample_bytree=seq(0.2,0.8,by=0.1),
                  subsample=seq(0.2,0.8,by=0.1),
                  rate_drop=seq(0.2,0.8,by=0.1),
                  skip_drop=seq(0.2,0.8,by=0.1),
                  min_child_weight=seq(1,7),
                  eta=seq(0.1,0.7,by = 0.1))
model.xgb <- train(x = mat_train,y = lbl_spec,
                   method = "xgbDART",tuneGrid = myGrid)

pred.xgb <- round(predict(model.xgb,newdata=mat_validation))
lbl_spec_val <- factor(pred.xgb,
                       labels = levels(imageSeg$Class))
confusionMatrix(lbl_spec_val,validation$Class)
