library(dplyr)
library(randomForest)
library(caret)

rdata <- read.csv("gas.csv")

str(rdata)
colSums(is.na(rdata))

data <- rdata %>% na.omit() %>% filter(roof!=0, wall!=0, cooling!=0, heating!=0)
str(data)


md <- randomForest(gas~., data = data, ntree = 300)
input <- data.frame(roof = 185, wall = 210, cooling = 84, heating = 0.2)
output <- predict(md, newdata = input, type = "response")

print(round(output,3))
#170.593


idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]

md2 <- randomForest(gas~., data = train, ntree = 300)
pred2 <- predict(md2, newdata = test, type = "response")
rmse <- (mean((pred2-test$gas)^2))^(1/2)
print(round(rmse,3))

#36.489



ctrl <- trainControl(method = "cv", number = 3)
tune_grid <- expand.grid(mtry = c(2,3,4))


md3 <- train(gas~., data = train,
             method = "rf",
             metric = "RMSE",
             trControl = ctrl,
             tuneGrid = tune_grid,
             ntree = 300)

pred_tune <- predict(md3, newdata = test)

rmse_tune <- (mean((pred_tune-test$gas)^2))^(1/2)

print(round(rmse_tune,3))

#27.512

result <- data.frame(pred = pred_tune)

write.csv(result, "gas_result20251124.csv", row.names = FALSE)



