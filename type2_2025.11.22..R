library(dplyr)
library(randomForest)
library(caret)

rdata <- read.csv("product.csv", fileEncoding="Euc-kr")

str(rdata)
colSums(is.na(rdata))

rdata$성별 <- ifelse(rdata$성별=="남자",1,0)
rdata$성별 <- factor(rdata$성별, levels=c(0,1))
str(rdata$성별)

data <- rdata %>% select(4:9)
str(data)

idx <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[idx,]
test <- data[-idx,]


md <- randomForest(성별~., data = train, ntree = 300)
pred <- predict(md, newdata = test, type="response")
cm <- confusionMatrix(pred, test$성별, mode="everything")

print(cm)

precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
f1 <- 2*precision*recall/(precision + recall)
print(f1)

print(round(f1,3))
# 0.571 