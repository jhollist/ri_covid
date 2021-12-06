library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(tsibble)

us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
ri <- us %>%
  filter(state == "Rhode Island") %>%
  mutate(daily_cases = zoo::rollapply(.data$cases, 2, function(x) x[2] - x[1], fill = NA),
         daily_deaths = zoo::rollapply(.data$deaths, 2, function(x) x[2] - x[1], fill = NA)) %>%
  filter(daily_cases > 0, daily_deaths > 0) %>%
  mutate(percent_daily_cases = daily_cases/max(daily_cases, na.rm = TRUE),
         percent_daily_deaths = daily_deaths/max(daily_deaths, na.rm = TRUE),
         percent_weekly_cases = zoo::rollapply(.data$percent_daily_cases, 7, 
                                               function(x) 
                                                 mean(x, na.rm = TRUE), 
                                               fill = NA),
         percent_weekly_deaths = zoo::rollapply(.data$percent_daily_deaths, 7, 
                                               function(x) 
                                                 mean(x, na.rm = TRUE), 
                                               fill = NA))
         

ri %>%
  mutate(year_week = yearweek(date)) %>%
  group_by(year_week) %>%
  summarize(weekly_cases = sum(daily_cases), weekly_deaths = sum(daily_deaths)) %>%
  ungroup() %>%
  mutate(percent_cases = weekly_cases/max(weekly_cases, na.rm = TRUE),
         percent_deaths = weekly_deaths/max(weekly_deaths, na.rm = TRUE)) %>%
  ggplot(aes(x = year_week, y = weekly_cases)) +
  geom_line(aes(x = year_week, y = percent_cases), color = "red", size = 1) +
  geom_line(aes(x = year_week, y = percent_deaths), color = "blue", size = 1)
  

