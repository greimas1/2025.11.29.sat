library(dplyr)
rdata <- read.csv("drinks.csv")

#str(rdata)
#print(colSums(is.na(rdata)))

data <- rdata %>% na.omit()

data_conti <- data %>% group_by(continent) %>% 
  summarise(beer_sum=sum(beer)) %>% arrange(desc(beer_sum))

str(data_conti)
print(data_conti$continent[1])
#EU

data_count <- data %>% filter(continent=="EU") %>% arrange(desc(beer))

data_count_1 <- data_count %>% slice(1)
print(data_count_1$beer)
print(data_count_1$country)

print(data_count$beer[1]) #361
print(data_count$country[1]) #Czech Republic

#print(data_count_1)

#print(data_count_1$beer)







