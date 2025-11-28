rdata <- read.csv("product.csv", fileEncoding="Euc-kr")

library(dplyr)
library(caret)
library(randomForest)
library(xgboost)

#str(rdata)
#print(colSums(is.na(rdata)))
#head(rdata)
#tail(rdata)

rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
#rdata$성별 <- as.factor(rdata$성별)
rdata$성별 <- factor(rdata$성별, levels = c(0,1))

data <- rdata %>% select(쇼핑액1월, 쇼핑액2월, 쇼핑액3월, 대표제품이름, 소득, 성별)

#str(data)

#set.seed(42)

set.seed(123)
idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]

md <- randomForest(성별~., data = train, ntree = 300)
pred <- predict(md, newdata = test, type = "response")
cm <- confusionMatrix(pred, test$성별, mode="everything")

precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]

#print(cm$byClass)

f1 <- 2*precision*recall/(precision + recall)
f1 <- round(f1,3)
print(f1)
#
print(precision)
print(recall)


-------------------------------------------------


#rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
#rdata$성별 <- as.factor(rdata$성별)
#rdata$성별 <- factor(rdata$성별, levels = c(0,1))
rdata$성별 <- as.numeric(as.character(rdata$성별))

data <- rdata %>% select(쇼핑액1월, 쇼핑액2월, 쇼핑액3월, 대표제품이름, 소득, 성별)

set.seed(123)
idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]

train_x <- model.matrix(성별~.-1, data = train)
train_y <- train$성별

test_x <- model.matrix(성별~.-1, data = test)
test_y <- test$성별

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

params <- list(objective = "binary:logistic",
               eval_metric = "logloss",
               eta = 0.1,
               max_depth = 4,
               nthread = 2)

md_tune <- xgb.train(param = params,
                     data = dtrain,
                     nrounds = 100)


pred_tune <- predict(md_tune, newdata = dtest)
pred_tune <- ifelse(pred_tune>0.5,1,0)

cm_tune <- confusionMatrix(factor(pred_tune, levels=c(0,1)),factor(test_y, levels=c(0,1)), 
                           mode="everything")

precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]

#print(cm$byClass)

f1_tune <- 2*precision*recall/(precision + recall)
f1_tune <- round(f1,3)
print(f1_tune)

  

