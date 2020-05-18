# Libraries
library(tidyverse)
library(lubridate)
library(prophet)
library(forecast)

# Read data
bikes <- read_csv('bikes.csv') %>%
    mutate(datetime = date(datetime))

# Separate data
train <- bikes %>% select(datetime, count, weather, atemp, humidity, windspeed) %>%
    filter(datetime < ymd("2012-01-01")) %>%
    group_by(datetime) %>%
    summarise(y = sum(count),
              weather = mean(weather),
              atemp = mean(atemp),
              humidity = mean(humidity),
              windspeed = mean(windspeed)) %>%
    rename(ds = datetime)

valid <- bikes %>% select(datetime, count, weather, atemp, humidity, windspeed) %>%
    filter(between(datetime, ymd("2012-01-01"), ymd("2012-06-30"))) %>%
    group_by(datetime) %>%
    summarise(y = sum(count),
              weather = mean(weather),
              atemp = mean(atemp),
              humidity = mean(humidity),
              windspeed = mean(windspeed)) %>%
    rename(ds = datetime)

test <- bikes %>% select(datetime, count, weather, atemp, humidity, windspeed) %>%
    filter(datetime >= ymd("2012-07-01")) %>%
    group_by(datetime) %>%
    summarise(y = sum(count),
              weather = mean(weather),
              atemp = mean(atemp),
              humidity = mean(humidity),
              windspeed = mean(windspeed)) %>%
    rename(ds = datetime)

# Holidays
holidays <- filter(bikes, holiday == 1) %>% 
    select(datetime) %>%
    distinct()
holidays$holiday = c('Martin Luther King', 'Emancipation Day', 'Independence Day',
                     'Labor Day', 'Columbus Day', 'Veterans Day', 'New Year', 
                     'Martin Luther King', 'Emancipation Day', 'Independence Day',
                     'Labor Day', 'Columbus Day', 'Veterans Day')
names(holidays) <- c('ds', 'holiday')

# Plot for the training set
(p <- ggplot(train,
             aes(x = ds, y = y)) +
        geom_point(size = 0.5) +
        theme_bw())

(p <- ggplot(train %>%
                 mutate(weekday = weekdays(ds)), 
             aes(x = ds, y = y)) +
        geom_point(size = 0.5) +
        facet_wrap(weekday ~ .) +
        theme_bw())

# Search grid
prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05, 0.5),
                           seasonality_prior_scale = c(1, 10),
                           holidays_prior_scale = c(1, 10),
                           regressor_prior_scale = c(1, 10),
                           capacity = c(6000, 7000, 8000),
                           growth = 'logistic')
#prophetGrid <- prophetGrid[-c(28:108), ]
#prophetGrid[prophetGrid$growth == 'linear', 'capacity'] <- NA
prophetGrid$growth <- as.character(prophetGrid$growth)
results <- vector(mode = 'numeric', length = nrow(prophetGrid))

# Search best parameters
for (i in seq_len(nrow(prophetGrid))) {
    # Select the parameters to try
    parameters <- prophetGrid[i, ]
    
    if (parameters$growth == 'logistic') {
        train$cap <- parameters$capacity
    }
    
    # Initialize Prophet model
    m <- prophet(growth = parameters$growth, 
                 holidays = holidays,
                 seasonality.prior.scale = parameters$seasonality_prior_scale, 
                 changepoint.prior.scale = parameters$changepoint_prior_scale,
                 holidays.prior.scale = parameters$holidays_prior_scale,
                 yearly.seasonality = TRUE,
                 fit = FALSE)
    
    # Add exogenous features
    m <- add_regressor(m, 'weather', prior.scale = parameters$regressor_prior_scale)
    m <- add_regressor(m, 'atemp', prior.scale = parameters$regressor_prior_scale)
    m <- add_regressor(m, 'humidity', prior.scale = parameters$regressor_prior_scale)
    m <- add_regressor(m, 'windspeed', prior.scale = parameters$regressor_prior_scale)
    m <- fit.prophet(m, train)
    
    #future <- make_future_dataframe(m, periods = 184)
    
    if (parameters$growth == 'logistic') {
        valid$cap <- parameters$capacity
    }
    
    # NOTE: There's a problem in function names with library(caret)
    forecast <- predict(m, bind_rows(train, valid))
    
    # Compute MAE and save it
    results[i] <- forecast::accuracy(forecast[ymd(forecast$ds) %in% valid$ds, ]$yhat, valid$y)[ , 'MAE']
}

prophetGrid <- cbind(prophetGrid, results)
best_params <- prophetGrid[prophetGrid$results == min(results), ]

# Retrain using train and validation set
retrain <- bind_rows(train, valid)
retrain$cap <- best_params$capacity
m <- prophet(growth = best_params$growth, 
             holidays = holidays,
             seasonality.prior.scale = best_params$seasonality_prior_scale,
             changepoint.prior.scale = best_params$changepoint_prior_scale,
             holidays.prior.scale = best_params$holidays_prior_scale,
             yearly.seasonality = TRUE,
             fit = FALSE)

m <- add_regressor(m, 'weather')
m <- add_regressor(m, 'atemp')
m <- add_regressor(m, 'humidity')
m <- add_regressor(m, 'windspeed')
m <- fit.prophet(m, retrain)

test$cap <- best_params$capacity

forecast <- predict(m, bind_rows(retrain, test))
forecast::accuracy(forecast[ymd(forecast$ds) %in% test$ds, ]$yhat, test$y)[ , 'MAE']

# Prophet plots
plot(m, forecast)
prophet_plot_components(m, forecast)

# Final plot
ggplot() + 
    geom_point(data = train, 
               aes(x = as.POSIXct(ds), y = y), 
               size = 0.5) +
    geom_point(data = valid, 
               aes(x = as.POSIXct(ds), y = y), 
               size = 0.5, color = '#4daf4a') +
    geom_point(data = test, 
               aes(x = as.POSIXct(ds), y = y), 
               size = 0.5, color = 'red') +
    geom_line(data = forecast, aes(x = ds, y = yhat), 
              color = "#0072B2") +
    geom_ribbon(data = forecast, 
                aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), 
                fill = "#0072B2", 
                alpha = 0.3) +
    labs(subtitle = 'Model with exogenous information', x = 'Date') +
    theme_bw()

# Save the plot
#ggsave("Images/prophet_part2.png", height = 3, width = 7, units = 'in')

# Diagnostics
df.cv <- cross_validation(m, horizon = 30, units = 'days')
df.p <- performance_metrics(df.cv)
plot_cross_validation_metric(df.cv, metric = 'mae')
