library(dplyr)
library(stringr)

rdata <- read.csv("email.csv")

str(rdata)
names(rdata)

data <- rdata %>% mutate(words = str_count(Email.Text, " ")+1)

data_spam <- data %>% filter(type=="spam") %>% summarise(avg_spam = mean(words)) %>% pull(avg_spam)
str(data_spam)


data_nospam <- data %>% filter(type=="nospam") %>% summarise(avg_nospam = mean(words)) %>% pull(avg_nospam)
str(data_nospam)

print(round(abs(data_spam-data_nospam),3))
#62.242