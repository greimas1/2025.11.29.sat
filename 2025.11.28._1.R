library(dplyr)
library(randomForest)
library(caret)

rdata <- read.csv("gas.csv")

#str(rdata)
#print(colSums(is.na(rdata)))

data <- rdata %>% na.omit() %>% 
  filter(roof!=0, wall!=0, cooling!=0, heating!=0)


#str(data)


md <- randomForest(gas~., data = data, ntree = 300)
input <- data.frame(roof = 185, wall = 210, cooling = 84, heating = 0.2)
pred <- predict(md, newdata = input, type="response") 
pred <- round(pred,3)
print(pred)
#172.81


idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]

md2 <- randomForest(gas~., data = train, ntree = 300)
pred2 <- predict(md2, newdata = test, type = "response")
mae <- round(mean(abs(pred2-test$gas)),3)
print(mae)
#31.824


#하이퍼파라미터 튜닝
tune_grid <- expand.grid(mtry=c(2,3,4))
ctrl <- trainControl(method="cv", number = 3)


md_tune <- train(gas~., data = train,
                 method = "rf",
                 metric = "RMSE",
                 trControl = ctrl,
                 tuneGrid = tune_grid,
                 ntree=300)

pred_tune <- predict(md_tune, newdata = test)


mae_tune <- mean(abs(pred_tune-test$gas))
mae_tune <- round(mae_tune,3)
print(mae_tune)
#20.516


result <- data.frame(pred = pred_tune)
write.csv(result,"result20251128.csv", row.names=FALSE)









