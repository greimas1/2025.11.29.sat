library(dplyr)
library(randomForest)
library(caret)
library(ModelMetrics)

rdata <- read.csv("P210204-01.csv")

str(rdata)
#print(colSums(is.na(rdata)))

rdata$Warehouse_block <- as.factor(rdata$Warehouse_block)
rdata$Customer_rating <- as.factor(rdata$Customer_rating)
rdata$Product_importance <- as.factor(rdata$Product_importance)
rdata$Gender <- as.factor(rdata$Gender)

rdata$Reached.on.Time_Y.N <- as.factor(rdata$Reached.on.Time_Y.N)

set.seed(42)
idx <- sample(1:nrow(rdata), nrow(rdata)*0.8)
train <- rdata[idx,-1]
test <- rdata[-idx,-1]

md <- randomForest(Reached.on.Time_Y.N~., data = train, ntree = 300)
pred <- predict(md, newdata = test, type = "response")
cm <- confusionMatrix(pred, test$Reached.on.Time_Y.N, mode="everything")

precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
f1 <- 2*precision*recall/(precision + recall)
f1 <- round(f1,3)
print(f1)
#0.618


#auc지표 
pred_prob <- predict(md, newdata = test, type = "prob")
auc_result <- auc(test$Reached.on.Time_Y.N, pred_prob)
auc_result <- round(auc_result,3)
print(auc_result)
#0.192



rdata2 <- read.csv("P210204-02.csv")
#str(rdata2)
rdata2$Warehouse_block <- as.factor(rdata2$Warehouse_block)
rdata2$Customer_rating <- as.factor(rdata2$Customer_rating)
rdata2$Product_importance <- as.factor(rdata2$Product_importance)
rdata2$Gender <- as.factor(rdata2$Gender)


md2 <- randomForest(Reached.on.Time_Y.N~., data = rdata[,-1], ntree = 300)
pred2 <- predict(md2, newdata = rdata2[,-1], type = "prob")
#str(pred2)

df <- data.frame(ID=rdata2[,1], pred=pred2[,1])
#str(df)


write.csv(df, "result2.2_2025.11.27.csv", row.names=FALSE)

#하이퍼파라미터 튜닝


library(xgboost)

rdata <- read.csv("P210204-01.csv")

#str(rdata)
#print(colSums(is.na(rdata)))

rdata$Warehouse_block <- as.character(rdata$Warehouse_block)
rdata$Customer_rating <- as.integer(rdata$Customer_rating)
rdata$Product_importance <- as.character(rdata$Product_importance)
rdata$Gender <- as.character(rdata$Gender)

rdata$Reached.on.Time_Y.N <- as.integer(rdata$Reached.on.Time_Y.N)

set.seed(42)
idx <- sample(1:nrow(rdata), nrow(rdata)*0.8)
train <- rdata[idx,-1]
#train_x <- as.matrix(train %>% select(-Reached.on.Time_Y.N))
train_x <-model.matrix(Reached.on.Time_Y.N~.-1, data = train)
train_y <- train$Reached.on.Time_Y.N

test <- rdata[-idx,-1]
test_x <- model.matrix(Reached.on.Time_Y.N~.-1, data = test)
test_y <- test$Reached.on.Time_Y.N

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)



params <- list(objective="binary:logistic",
               eval_metric = "logloss",
               max_depth = 4,
               eta = 0.1,
               n_threat = 2)

md_tune <- xgb.train(param = params,
                     data = dtrain,
                     nrounds = 100)


pred_tune <- predict(md_tune, newdata = dtest)

str(pred_tune)

auc_tune <- auc(test$Reached.on.Time_Y.N, pred_tune)
auc_tune <- round(auc_tune,3)
print(auc_tune)
#0.796

