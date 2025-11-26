rdata <- read.csv("email.csv")

library(dplyr)
library(stringr)

#str(rdata)
#print(colSums(is.na(rdata)))

data <- rdata %>% mutate(words = str_count(Email.Text, " ")+1)

#str(data)


data_spam <- data %>% filter(type=="spam") %>% summarise(mean_words=mean(words)) 
data_spam_spam <- data_spam["mean_words"]
#print(data_spam_spam)


data_nospam <- data %>% filter(type=="nospam") %>% summarise(mean_words=mean(words)) 
data_nospam_nospam <- data_nospam["mean_words"]
#print(data_nospam_nospam)


print(round(abs(data_spam_spam - data_nospam_nospam),3))
#  62.242