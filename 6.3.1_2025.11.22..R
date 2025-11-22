library(dplyr)

rdata <- read.csv("P230605.csv", fileEncoding="Euc-kr")
str(rdata)
colSums(is.na(rdata))

ntot <- nrow(rdata)

data <- rdata %>% group_by(코드) %>%
  summarise(건수=n(), 확률=건수/ntot)

str(data)
result1 <- round(data$확률[4],3)
print(result1)

#0.787
str(data$건수)
table(data$코드, data$건수)

chi_out <- chisq.test(data$건수, p=c(0.05, 0.1, 0.05, 0.8))
print(round(chi_out$statistic,3))
#0.997

print(round(chi_out$p.value,3))
#0.802
#chi_result <- chisq.test(table(data$건수), p=c(0.05, 0.1 ,0.05, 0.8))







