rdata <- read.csv("P230601.csv")

#str(rdata)
#print(colSums(is.na(rdata)))

library(dplyr)
library(lubridate)

data <- data.frame(calltime=rdata$신고일시, arrivetime=rdata$출동일시, station=rdata$출동소방서)

str(data)

data$calltime <- ymd_hm(data$calltime)
data$arrivetime <- ymd_hm(data$arrivetime)

str(data)

#datadiff_time <- data %>% mutate(diff_time = difftime(arrivetime, calltime, units="secs"))
data_diff_time <- data %>% mutate(diff_time = difftime(arrivetime, calltime, units="secs"))

#str(data_diff_time)

data_sort <- data_diff_time %>% group_by(station, year=year(calltime), month=month(calltime)) %>%
  summarise(mean_diff_time=mean(diff_time), .groups="drop") %>% arrange(desc(mean_diff_time))

print(data_sort$mean_diff_time[1])

print(as.numeric(data_sort$mean_diff_time[1])/60)
#64