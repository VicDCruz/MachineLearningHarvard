library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Q2
cutoff <- c("inclass", "online")
accuracy <- map_dbl(cutoff, function(x) {
  yHat <- ifelse(dat$type == x, "Male", "Female")
  mean(yHat == dat$sex)
})

bestCutoff <- cutoff[which.max(accuracy)]
yHat <- ifelse(dat$type == bestCutoff, "Male", "Female") %>% 
  factor(levels = levels(y))

# Q3
table(yHat, y)

# Q4
sensitivity(data = yHat, reference = y)

# Q5
specificity(data = yHat, reference = y)

# Q6
confusionMatrix(data = yHat, reference = y) # OR
mean(y == "Female")
