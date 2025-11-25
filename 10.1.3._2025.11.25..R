library(dplyr)
library(stringr)

rdata <- read.csv("email.csv")
str(rdata)
print(colSums(is.na(rdata)))

data <- rdata %>% mutate(words = str_count(Email.Text, " ")+1)
str(data)

nospam_mean <- data %>% filter(type=="nospam") %>% summarise(mean_words=mean(words)) %>% pull(mean_words)
str(nospam_mean)
str(data)
spam_mean <- data %>% filter(type=="spam") %>% summarise(mean_words=mean(words)) %>% pull(mean_words)
str(spam_mean)
print(round(abs(nospam_mean-spam_mean),3))
#62.242


