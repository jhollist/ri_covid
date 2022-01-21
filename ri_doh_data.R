library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidyr)
library(hrbrthemes)
library(plotly)

gs4_deauth()
ri_doh_data <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4", "Trends")

ri_covid_data <- select(ri_doh_data, date = Date, 
                        cases = 'New cases (may count people more than once)', 
                        tests = 'Daily total tests completed (may count people more than once)',
                        deaths = 'Date of death',
                        hospitalized = 'Currently hospitalized') %>%
  mutate(positivity = (cases/tests) * 100, 
         three_day_avg_cases = zoo::rollapply(.data$cases, 3, mean, fill = NA),
         three_day_avg_deaths = 
           zoo::rollapply(.data$deaths, 3, mean, fill = NA),
         three_day_avg_hospitalized = 
           zoo::rollapply(.data$hospitalized, 3, mean, fill = NA),
         three_day_avg_positivity = 
           zoo::rollapply(.data$positivity, 3, mean, fill = NA)) %>%
  select(date, cases, starts_with("three")) %>%
  pivot_longer(cols = cases:three_day_avg_positivity, 
               names_to = "variable",
               values_to = "value")

variable_lab <- c("cases" = "Daily Cases",
                  "three_day_avg_cases" = "3-Day Avg. Cases", 
                  "three_day_avg_deaths" = "3-Day Avg. Deaths",
                  "three_day_avg_hospitalized" = "3-day Avg. Hospitalized",
                  "three_day_avg_positivity" = "3-day Avg. Positivity")

ri_plots <- ri_covid_data |>
  filter(variable != "three_day_avg_hospitalized") |>
  ggplot(aes(x = date, y = value)) +
  geom_line(size = 1, aes(color = variable)) +
  facet_grid(variable ~ ., scales = "free", 
             labeller = labeller(variable = variable_lab)) +
  labs(x = "Date", y = "", title = paste0("Rhode Island COVID-19 (as of: ", 
                                          max(ri_covid_data$date), ")")) +
  theme_ipsum_rc() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("darksalmon","darkred","darkblue","grey50","black"))
ggsave("ri_plot.jpg", ri_plots, width = 11, height = 8.5)
index <- ggplotly(ri_plots) 
htmlwidgets::saveWidget(index, 
                        here::here("index.html"), 
                        selfcontained = FALSE)
