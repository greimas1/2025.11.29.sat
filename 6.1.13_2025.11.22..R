library(dplyr)
library(lubridate)

rdata <- read.csv("P230601.csv")
str(rdata)
colSums(is.na(rdata))


data <- data.frame(call_time = rdata$신고일시, arrive_time = rdata$출동일시,
                   fire_station = rdata$출동소방서)
str(data)

data$call_time <- ymd_hm(data$call_time)
data$arrive_time <- ymd_hm(data$arrive_time)
data$diff_time <- difftime(data$arrive_time, data$call_time, units="secs")

df <- data %>% group_by(fire_station, year=year(call_time),
                        month=month(call_time)) %>%
  summarise(mean_time = mean(diff_time), .groups="drop") %>% arrange(desc(mean_time))
str(df)

print(as.numeric(df$mean_time[1])/60)
#64
            
            
            





