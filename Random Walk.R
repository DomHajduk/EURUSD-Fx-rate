library(tidyverse)
library(tsibble)
library(fable)
library(ggplot2)
library(lubridate)
library(fpp3)
library(forecast)
library(usethis)

Daily_fx <- read_csv("fx.csv")

Daily_ts <- as_tsibble(Daily_fx)
Daily_ts <- fill_gaps(Daily_ts)

StartDate <- Sys.Date()
EndDate <- as.Date("2022-10-14")

Forecast_horizon <-
  sum(!weekdays(seq(StartDate, EndDate, "days")) %in% c("Saturday", "Sunday"))

naive <- Daily_ts %>% 
  model(NAIVE(Close)) %>% 
  forecast(h = Forecast_horizon, bootstrap = TRUE, times = 100000, level = c(30,50,80,95))

Distributions <- hilo(naive)

autoplot(naive, Daily_ts) +
  xlim(as.Date(c("2022-01-01", "2022-10-31"), format="%Y-%m-%d")) +
  labs(
    title = "Random walk simulation of EUR/USD close",
    subtitle = "Based on data since 12/2003",
    y = "Daily close",
    x = "Date",
    caption = "Note: Forecast horizon seemingly does not extend to question closing date due to skipping weekends",
)
  