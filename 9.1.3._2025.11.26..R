rdata <- read.csv("years.csv")

library(dplyr)

#str(rdata)
#print(colSums(is.na(rdata)))

rdata$satisfy <- ifelse(is.na(rdata$satisfy), mean(rdata$satisfy, na.rm=TRUE), rdata$satisfy)

data <- rdata %>% group_by(dept, grade) %>%
  mutate(mean_year=mean(year, na.rm=TRUE),
         year = ifelse(is.na(year), mean_year, year))

#str(data)
#print(colSums(is.na(data)))


data <- data %>% mutate(a=amount/year,
                        b=amount/satisfy)

data_a <- data %>% arrange(desc(a))
aa <- data_a$year[3]

data_b <- data %>% arrange(desc(b))
bb <- data_b$satisfy[2]
print(as.integer(aa+bb))
#13
