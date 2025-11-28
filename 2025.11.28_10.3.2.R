rdata <- read.csv("energy.csv")

library(dplyr)

str(rdata)
print(colSums(is.na(rdata)))

md <- lm(totalenergy~., data = rdata)

#print(summary(md)$coefficients[-1,4]<0.05)
#print(summary(md)$coefficients[-1,4])

result1 <- sum(summary(md)$coefficients[-1,1])
result1 <- round(result1,3)
#print(result1)
#2

#print(summary(md)$coefficients[-1,4])
md2 <- lm(totalenergy~cooling + heating, data = rdata)
result2 <- summary(md2)$r.squared
result2 <- round(result2,3)
#print(result2)
#1


input <- data.frame(size = 185, height = 3, glazing = 70, exteriorwall = 163,
                    roof = 0.22, wall = 0.6, cooling = 25, heating = 0.003)

result3 <- predict(md2, newdata = input)
result3 <- round(result3, 3)
print(result3)
#25.003




