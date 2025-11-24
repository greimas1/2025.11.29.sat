library(dplyr)

rdata <- read.csv("answer.csv", fileEncoding = "Euc-kr")
str(rdata)
colSums(is.na(rdata))

data <- rdata %>% group_by(대주제, 소주제) %>%
  summarise(정답합계=sum(정답), 응시수 = n(),
            정답률 = round(정답합계/응시수,3), .groups="drop")

str(data)

data_agg <- data %>% arrange(대주제, desc(정답률), 소주제) 

top_topic <- data_agg %>% group_by(대주제) %>%
  filter(정답률==max(정답률)) %>% ungroup()

str(top_topic)
  
  
  
  
  
  
  