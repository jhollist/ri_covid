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
         seven_day_avg_cases = zoo::rollapply(.data$cases, 7, mean, 
                                              fill = "right", align = "right"),
         seven_day_avg_deaths = 
           zoo::rollapply(.data$deaths, 7, mean, fill = "right", 
                          align = "right"),
         seven_day_avg_hospitalized = 
           zoo::rollapply(.data$hospitalized, 7, mean, fill = "right", 
                          align = "right"),
         seven_day_avg_positivity = 
           zoo::rollapply(.data$positivity, 7, mean, fill = "right", 
                          align = "right")) %>%
  select(date, cases, deaths, positivity, starts_with("seven")) %>%
  pivot_longer(cols = cases:seven_day_avg_positivity, 
               names_to = "variable",
               values_to = "value")

variable_lab <- c("cases" = "Daily Cases",
                  "seven_day_avg_cases" = "7-Day Avg. Cases", 
                  "seven_day_avg_deaths" = "7-Day Avg. Deaths",
                  "seven_day_avg_hospitalized" = "7-day Avg. Hospitalized",
                  "seven_day_avg_positivity" = "7-day Avg. Positivity")

ri_daily <- ri_covid_data %>%
  filter(variable %in% c("cases", "deaths", "positivity")) %>%
  mutate(variable = case_when(variable == "cases" ~
                                "seven_day_avg_cases",
                              variable == "deaths" ~
                                "seven_day_avg_deaths",
                              variable == "positivity" ~
                                "seven_day_avg_positivity"
                              ))

ri_plots <- ri_covid_data |>
  filter(!variable %in% c("seven_day_avg_hospitalized", "cases", "deaths", "positivity")) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(data = ri_daily, aes(x=date, y=value), size = 1, color = "grey70") +
  geom_line(size = 1, aes(color = variable)) +
  facet_grid(variable ~ ., scales = "free", 
             labeller = labeller(variable = variable_lab)) +
  labs(x = "Date", y = "", title = paste0("Rhode Island COVID-19 (as of: ", 
                                          max(ri_covid_data$date), ")")) +
  theme_ipsum_rc() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey30","darkred","darkblue"))
ggsave("ri_plot.jpg", ri_plots, width = 11, height = 8.5)
index <- ggplotly(ri_plots) 
htmlwidgets::saveWidget(index, 
                        here::here("index.html"), 
                        selfcontained = FALSE)

system("git add -A")
system(paste0("git commit -m '", lubridate::today(), "update.'" ))
system("git push origin main")
