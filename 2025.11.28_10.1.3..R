library(dplyr)
library(stringr)


rdata <- read.csv("email.csv")

#str(rdata)
#print(colSums(is.na(rdata)))

data <- rdata %>% mutate(words = str_count(Email.Text, " ")+1)

nospam_mean <- data %>% 
  filter(type=="nospam") %>% summarise(mean_words = mean(words)) %>% pull(mean_words)

spam_mean <- data %>% 
  filter(type=="spam") %>% summarise(mean_words = mean(words)) %>% pull(mean_words)

result <- round(abs(nospam_mean - spam_mean),3)
print(result)
# 62.242