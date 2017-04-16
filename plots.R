library(tidyverse)
library(lubridate)
library(prophet)

bikes <- read_csv('bikes.csv')
bikes_week <- bikes %>% select(datetime, count) %>%
    mutate(datetime = date(datetime)) %>%
    group_by(datetime) %>%
    summarise(total = sum(count)) %>%
    mutate(weekday = wday(datetime, label = T)) %>%
    group_by(weekday) %>%
    summarise(total = sum(total)) %>%
    ggplot(aes(x = weekday, y = total)) + 
    geom_col()
