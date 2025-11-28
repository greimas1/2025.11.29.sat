rdata <- read.csv("turnover.csv")

str(rdata)
print(colSums(is.na(rdata)))

md <- glm(turnover~., data=rdata, family = "binomial")

#print(summary(md)$coefficients[-1,4])
#"age"

#print(summary(md)$coefficients[-1,1])

result1 <- summary(md)$coefficients["age","Estimate"]
result1 <- round(result1,3)
#print(result1)
# -0.035


result2 <- exp(summary(md)$coefficients["age","Estimate"])
result2 <- round(result2,3)
#print(result2)
#0.996
set.seed(42)
idx <- sample(1:nrow(rdata), nrow(rdata)*0.8)
train <- rdata[idx,]
test <- rdata[-idx,]

md2 <- glm(turnover~., data = train, family = "binomial")
pred2 <- predict(md2, newdata = test, type="response")
pred2 <- ifelse(pred2>0.5, 1, 0)

acc <- mean(pred2==test$turnover)
acc <- round(acc,3)
print(acc)
#0.667






