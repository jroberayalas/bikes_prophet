library(prophet)
library(tidyverse)

df <- read.table('example_wp_peyton_manning.txt', header = T, sep = ',') %>%
    mutate(y = log(y))

m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)
#tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
prophet_plot_components(m, forecast)
