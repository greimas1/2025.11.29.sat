library(dplyr)
library(randomForest)
library(caret)

set.seed(42)
rdata <- read.csv("product.csv", fileEncoding = "Euc-kr")
str(rdata)
colSums(is.na(rdata))

rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
rdata$성별 <- factor(rdata$성별, levels=c(0,1))

str(rdata)

data <- rdata %>% select(4:9)

idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]


md <- randomForest(성별~., data = train, ntree = 300)
pred <- predict(md, newdata = test,  type = "response")
cm <- confusionMatrix(pred, test$성별, mode = "everything")
print(round(cm$byClass["F1"],3))
#0.308

recall <- cm$byClass["Sensitivity"]
precision <- cm$byClass["Pos Pred Value"]
f1 <- 2*precision*recall/(precision + recall)
print(round(f1,3))
#0.222

library(xgboost)

data$성별 <- as.numeric(as.character(data$성별))

idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]

train_y <- train$성별
train_x <- as.matrix(train %>% select(-성별))
test_y <- test$성별
test_x <- as.matrix(test %>% select(-성별))

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

params <- list(objective = "binary:logistic",
               eval_metric = "logloss",
               max_depth = 4,
               eta = 0.1,
               nthread = 2)

md_tune <- xgb.train(param = params,
                     data = dtrain,
                     nrounds = 100)

pred_tune <- predict(md_tune, newdata = test_x)
str(pred_tune)

pred <- ifelse(pred_tune>0.5, 1, 0) 

cm_tune <- confusionMatrix(factor(pred, levels=c(0,1)), factor(test_y, levels=c(0,1)))

print(round(cm_tune$byClass["F1"],3))
#0.167 

