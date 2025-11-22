library(dplyr)
library(caret)
library(randomForest)
library(xgboost)

rdata <- read.csv("product.csv", fileEncoding="Euc-kr")

str(rdata)
colSums(is.na(rdata))


rdata$성별 <- ifelse(rdata$성별=="남자",1,0)

data <- rdata %>% select(4:9)

str(data)


idx <- sample(1:nrow(data), nrow(data)*0.8)
train<- data[idx,]
test <- data[-idx,]


train_y <- train$성별
train_x <- as.matrix(train %>% select(-성별))

test_y <- test$성별
test_x <- as.matrix(test %>% select(-성별))

dtrain <- xgb.DMatrix(data = train_x, label = train_y )
dtest <- xgb.DMatrix(data = test_x, label = test_y)


param <- list(objective = "binary:logistic",
              eval_metric = "logloss",
              max_depth = 4,
              eta = 0.1,
              nthread = 2)


md <- xgb.train(params = param,
                data =  dtrain,
                nrounds = 100)


pred_prob <- predict(md, dtest)
pred <- ifelse(pred_prob>0.5,1,0)


cm <- confusionMatrix(factor(pred, levels=c(0,1)), factor(test$성별, levels=c(0,1)), mode="everything")

print(cm)

print(cm$byClass["F1"])

































