library(dplyr)
library(Metrics)
library(randomForest)
library(caret)

rdata <- read.csv("gas.csv")

str(rdata)
colSums(is.na(rdata))

data <- rdata %>% na.omit() %>% filter(roof!=0, wall!=0, cooling!=0, heating!=0)

str(data)
colSums(is.na(data))

md <- randomForest(gas~., data=data, ntree = 300)
input <- data.frame(roof=185, wall=210, cooling=84, heating=0.2)
print(input)
output <- predict(md, newdata=input, type="response")
print(output)
print(round(output, 3))#176.913 



set.seed(42)
idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx, ]
test <- data[-idx, ] 

md2 <- randomForest(gas~., data  = train, ntree = 300)
pred2 <- predict(md2, newdata = test, type="response")
rmseresult <- rmse(test$gas, pred2)
print(round(rmseresult,3))

#39.532


ctrl <- trainControl(method = "cv", number = 3)
tune_grid <- expand.grid(mtry = c(2,3,4))


set.seed(42)
md_tuned <- train(gas~., data=train, method = "rf", metric = "RMSE", trControl = ctrl, tuneGrid = tune_grid, ntree = 300) 
pred_tuned <- predict(md_tuned, newdata = test)
resulttuned <- rmse(test$gas, pred_tuned)
print(round(resulttuned,3))

#28.105


result <- data.frame(pred = pred_tuned)
str(result)
write.csv(result, "gasresult20251116.csv", row.names=FALSE)







