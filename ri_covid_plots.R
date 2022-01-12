library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(tsibble)
library(plotly)
library(tidyr)
library(hrbrthemes)

ri_pop <- 1097379
ct_pop <- 3605944
ma_pop <- 7029917

us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
sne <- us %>%
  filter(state %in% c("Rhode Island", "Connecticut", "Massachusetts")) %>%
  mutate(pop = case_when(state == "Rhode Island" ~
                           ri_pop,
                         state == "Connecticut" ~
                           ct_pop,
                         state == "Massachusetts" ~
                           ma_pop)) %>%
  group_by(state) %>%
  mutate(daily_cases = zoo::rollapply(.data$cases, 2, 
                                      function(x) x[2] - x[1], fill = NA),
         daily_deaths = zoo::rollapply(.data$deaths, 2, 
                                       function(x) x[2] - x[1], fill = NA),
         case_fatality_rate = (daily_deaths/daily_cases) * 100,
         daily_cases_per100k = (daily_cases/pop) * 100000,
         daily_deaths_per100k = (daily_deaths/pop) * 100000) %>%
  filter(daily_cases > 0, daily_deaths > 0) %>%
  mutate(three_day_avg_cases = zoo::rollapply(.data$daily_cases, 7, mean, fill = NA),
         three_day_avg_deaths = zoo::rollapply(.data$daily_deaths, 7, mean, fill = NA),
         three_day_avg_cfr = zoo::rollapply(.data$case_fatality_rate, 7, mean, fill = NA),
         percent_daily_cases = daily_cases/max(daily_cases, na.rm = TRUE),
         percent_daily_deaths = daily_deaths/max(daily_deaths, na.rm = TRUE),
         percent_weekly_cases = zoo::rollapply(.data$percent_daily_cases, 7, 
                                               function(x) 
                                                 mean(x, na.rm = TRUE), 
                                               fill = NA),
         percent_weekly_deaths = zoo::rollapply(.data$percent_daily_deaths, 7, 
                                               function(x) 
                                                 mean(x, na.rm = TRUE), 
                                               fill = NA)) %>%
  ungroup() 
         

variable_lab <- c("three_day_avg_cases" = "3-Day Avg. Cases", 
                  "three_day_avg_deaths" = "3-Day Avg. Deaths",
                  "three_day_avg_cfr" = "3-day Avg. Fatality Rate")


sne_plot <- sne %>%
  filter(state == "Rhode Island", date > ymd("2021-1-01")) %>%
  filter(date > ymd("2021-10-01")) %>%
  select(date, state, three_day_avg_cases, three_day_avg_deaths, 
         three_day_avg_cfr) %>%
  pivot_longer(cols = three_day_avg_cases:three_day_avg_cfr, 
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(size = 1, aes(color = variable), show.legend = FALSE) +
  facet_grid(variable ~ state, scales = "free_y", 
             labeller = labeller(variable = variable_lab)) +
  labs(x = "Date", y = "", title = "MA, CT, RI COVID-19") +
  theme_ipsum_rc() +
  scale_color_manual(values = c("three_day_avg_cases" = "darkred",
                                "three_day_avg_deaths" = "darkblue")) 

sne_plot

ggsave("sne_plot.jpg", sne_plot, width = 11, height = 8.5)
ggplotly(sne_plot)
max(sne$date)

sne_plot <- sne %>%
  filter(state == "Rhode Island", date > ymd("2021-01-01")) %>%
  select(date, state, daily_cases, daily_deaths) %>%
  pivot_longer(cols = daily_cases:daily_deaths, 
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(size = 1, aes(color = variable), show.legend = FALSE) +
  facet_grid(variable ~ state, scales = "free_y", 
             labeller = labeller(variable = variable_lab)) +
  labs(x = "Date", y = "", title = "MA, CT, RI COVID-19") +
  theme_ipsum_rc() +
  scale_color_manual(values = c("daily_cases" = "darkred",
                                "daily_deaths" = "darkblue")) 

ggplotly(sne_plot)
