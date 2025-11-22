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

--------------------------
  
  param <- list(
    objective = "binary:logistic",   # 0/1 이진 분류 문제, 예측값을 확률로 출력
    eval_metric = "logloss",         # 모델 성능 평가 지표(작을수록 좋음), 이진 분류 기본 손실함수
    max_depth = 4,                   # 개별 트리가 가질 수 있는 최대 깊이(과적합 방지, 3~6 추천)
    eta = 0.1,                       # 학습률: 작을수록 안정적(과적합↓), 크면 빠르지만 불안정
    nthread = 2                      # CPU 사용 개수(2개 코어만 사용하여 속도/안정성 조절)
  )


md <- xgb.train(
  params = param,   # 위에서 설정한 파라미터 묶음을 전달
  data = dtrain,    # 학습할 데이터(xgb.DMatrix 형태)
  nrounds = 100,    # 반복 학습 횟수 = 트리 개수(적으면 빠르고 덜 복잡)
  verbose = 0       # 0: 학습 과정 출력 안 함, 1: 출력함
)


library(dplyr)
library(xgboost)
library(caret)

# 1. 데이터 불러오기
rdata <- read.csv("product.csv", fileEncoding="Euc-kr")

# 2. 결측치 확인
str(rdata)
colSums(is.na(rdata))

# 3. 성별을 0/1 로 변환 (xgboost는 numeric 0/1 사용)
rdata$성별 <- ifelse(rdata$성별 == "남자", 1, 0)

# 4. 분석 변수 선택
data <- rdata %>% select(4:9)

# 5. 훈련/테스트 분할
set.seed(123)
idx <- sample(1:nrow(data), nrow(data) * 0.8)
train <- data[idx, ]
test  <- data[-idx, ]

#-------------------------------
# 6. xgboost용 DMatrix 생성
#-------------------------------
train_y <- train$성별
train_x <- as.matrix(train %>% select(-성별))

test_y <- test$성별
test_x <- as.matrix(test %>% select(-성별))

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x,  label = test_y)

#-------------------------------
# 7. xgboost 모델 학습 (60초 내 빠른 설정)
#-------------------------------
param <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 4,
  eta = 0.1,
  nthread = 2
)

md <- xgb.train(
  params = param,
  data = dtrain,
  nrounds = 100,      # 속도 빠름
  verbose = 0
)

#-------------------------------
# 8. 예측 및 혼동행렬
#-------------------------------
pred_prob <- predict(md, dtest)
pred <- ifelse(pred_prob > 0.5, 1, 0)

cm <- confusionMatrix(
  factor(pred, levels = c(0,1)),
  factor(test_y, levels = c(0,1)),
  mode = "everything"
)

print(cm)

#-------------------------------
# 9. F1 Score 계산 (시험에 반드시 필요)
#-------------------------------
precision <- cm$byClass["Pos Pred Value"]
recall    <- cm$byClass["Sensitivity"]

f1 <- 2 * precision * recall / (precision + recall)
print(f1)
print(round(f1, 3))
