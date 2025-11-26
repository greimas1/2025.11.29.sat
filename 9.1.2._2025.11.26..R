rdata <- read.csv("arrest.csv", fileEncoding="Euc-kr")

library(dplyr)

str(rdata)
print(colSums(is.na(rdata)))


data <- rdata %>% mutate(year=substr(ym,1,4), month=substr(ym,7,8), 
                         arrest_rate = arrest/cases)

str(data)

data_sort <- data %>% group_by(year, crime) %>% 
  summarise(mean_arrest_rate = mean(arrest_rate), 
            arrest_sum=sum(arrest), .groups="drop")

str(data_sort)

data_sort_arrest_rate <- data_sort %>% group_by(year) %>%
  slice_max(mean_arrest_rate) %>% summarise(total_arrest_sum=sum(arrest_sum))

print(sum(data_sort_arrest_rate$total_arrest_sum))
#50





