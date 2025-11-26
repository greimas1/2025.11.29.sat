rdata <- read.csv("totalsale.csv")

library(dplyr)

#str(rdata)
#print(colSums(is.na(rdata)))

data <- rdata %>% mutate(year=substr(ym,1,4),
                         month=substr(ym,7,8))

#str(data)

data_sort <- data %>% group_by(year, month) %>%
  summarise(sum_sales=sum(sales), .groups="drop") %>%
  arrange(desc(sum_sales))


print(data_sort$year[2])#"2009"
print(data_sort$month[2])#"06"
print(data_sort$sum_sales[2])#32100

print(data_sort$year[4])#2008"
print(data_sort$month[4])#""03"
print(data_sort$sum_sales[4])# 29500





