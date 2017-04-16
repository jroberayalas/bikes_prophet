# Libraries
library(tidyverse)
library(lubridate)
library(prophet)
library(forecast)

# Read data
bikes <- read_csv('bikes.csv') %>%
    mutate(datetime = date(datetime))

# Separate data
train <- bikes %>% select(datetime, count) %>%
    filter(datetime < as.Date("2012-01-01")) %>%
    group_by(datetime) %>%
    summarise(y = sum(count))
names(train) <- c('ds', 'y')

valid <- bikes %>% select(datetime, count) %>%
    filter(datetime >= as.Date("2012-01-01") & datetime < as.Date("2012-07-01")) %>%
    group_by(datetime) %>%
    summarise(y = sum(count))
names(valid) <- c('ds', 'y')

test <- bikes %>% select(datetime, count) %>%
    filter(datetime >= as.Date("2012-07-01")) %>%
    group_by(datetime) %>%
    summarise(y = sum(count))
names(test) <- c('ds', 'y')

# Holidays (from http://www.officeholidays.com/countries/usa/2011.php)
holidays <- filter(bikes, holiday == 1) %>% 
    select(datetime) %>%
    distinct()
holidays$holiday = c('Martin Luther King', 'Emancipation Day', 'Independence Day',
                     'Labor Day', 'Columbus Day', 'Veterans Day', 'New Year', 
                     'Martin Luther King', 'Emancipation Day', 'Independence Day',
                     'Labor Day', 'Columbus Day', 'Veterans Day')
names(holidays) <- c('ds', 'holiday')

# Plot for the training set
p <- ggplot(train, aes(x = ds, y = y))
p <- p + geom_point(size = 0.5)
p

# Search grid
prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05, 0.5, 0.001),
                           seasonality_prior_scale = c(100, 10, 1),
                           holidays_prior_scale = c(100, 10, 1),
                           capacity = c(6043, 6500, 7000, 8000),
                           growth = c('linear', 'logistic'))
prophetGrid <- prophetGrid[-c(28:108), ]
prophetGrid[prophetGrid$growth == 'linear', 'capacity'] <- NA
prophetGrid$growth <- as.character(prophetGrid$growth)
results <- vector(mode = 'numeric', length = nrow(prophetGrid))

# Search best parameters
for (i in seq_len(nrow(prophetGrid))) {
    parameters <- prophetGrid[i, ]
    if (parameters$growth == 'logistic') {train$cap <- parameters$capacity}
    set.seed(12321)
    m <- prophet(train, growth = parameters$growth, holidays = holidays,
                 seasonality.prior.scale = parameters$seasonality_prior_scale, 
                 changepoint.prior.scale = parameters$changepoint_prior_scale,
                 holidays.prior.scale = parameters$holidays_prior_scale)
    
    future <- make_future_dataframe(m, periods = 184)
    if (parameters$growth == 'logistic') {future$cap <- parameters$capacity}
    
    # NOTE: There's a problem in function names with library(caret)
    forecast <- predict(m, future)
    #tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    #forecast$ds <- as.POSIXct(forecast$ds)
    #forecast$ds[seq_len(nrow(train))] <- train$datetime
    #forecast$ds[5423:nrow(forecast)] <- seq(ymd_hms('2011-12-20 01:00:00'), by = 'day', length.out = 364)
    
    results[i] <- forecast::accuracy(forecast[forecast$ds %in% valid$ds, 'yhat'], valid$y)[ , 'MAE']
}

prophetGrid <- cbind(prophetGrid, results)
best_params <- prophetGrid[prophetGrid$results == min(results), ]

# Retrain using train and validation set
retrain <- bind_rows(train, valid)
retrain$cap <- best_params$capacity
m <- prophet(retrain, growth = 'logistic', holidays = holidays,
             seasonality.prior.scale = best_params$seasonality_prior_scale, 
             changepoint.prior.scale = best_params$changepoint_prior_scale,
             holidays.prior.scale = best_params$holidays_prior_scale)

future <- make_future_dataframe(m, periods = 184)
future$cap <- best_params$capacity

forecast <- predict(m, future)
forecast::accuracy(forecast[forecast$ds %in% test$ds, 'yhat'], test$y)[ , 'MAE']

# Prophet plots
plot(m, forecast)
prophet_plot_components(m, forecast)

# Final plot
p <- ggplot()
p <- p + geom_point(data = train, aes(x = ds, y = y), size = 0.5)
p <- p + geom_line(data = forecast, aes(x = ds, y = yhat), color = "#0072B2")
p <- p + geom_ribbon(data = forecast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "#0072B2", alpha = 0.3)
p <- p + geom_point(data = valid, aes(x = ds, y = y), size = 0.5, color = '#4daf4a')
p <- p + geom_point(data = test, aes(x = ds, y = y), size = 0.5, color = 'red')
p
