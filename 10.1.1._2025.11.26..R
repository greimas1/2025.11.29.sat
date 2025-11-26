library(dplyr)

rdata <- read.csv("answer.csv", fileEncoding="Euc-kr")

#str(rdata)
#print(colSums(is.na(rdata)))

#대주제

topic_rate <- rdata %>% group_by(대주제) %>%
  summarise(정답률=round(mean(정답),3)) %>% arrange(desc(정답률))

str(topic_rate)

#소주제
sub_rate <- rdata %>% group_by(대주제, 소주제) %>%
  summarise(정답합계=sum(정답), 
            응시수=n(),
            정답률= round(정답합계/응시수,3), .groups="drop")
#소주제 정렬
sub_sorted <- sub_rate %>% arrange(대주제, desc(정답률), 소주제)


#대주제 최고 정답률 소주제

top_sorted <- sub_sorted %>% group_by(대주제) %>%
  filter(정답률==max(정답률)) %>% arrange(대주제, 소주제) %>%
  ungroup()

#정답률3

top3_sorted <- sub_sorted %>% group_by(대주제) %>%
  slice_max(정답률, n=3) %>% arrange(대주제, desc(정답률), 소주제) %>%
  ungroup()

print(top3_sorted)





  
  
  





