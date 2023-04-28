library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidyr)
#library(hrbrthemes)
library(plotly)
library(lubridate)

jwh_covid_risk_index <- function(cases, deaths, hosp){
  case_prop <- cases/max(cases, na.rm = TRUE)
  #case_prop <- case_prop/max(case_prop, na.rm = TRUE)
  death_prop <- deaths/max(deaths, na.rm = TRUE)
  #death_prop <- death_prop/max(death_prop, na.rm = TRUE)
  hosp_prop <- hosp/max(hosp, na.rm = TRUE)
  #hosp_prop <- hosp_prop/max(hosp_prop, na.rm = TRUE)
  #pos_prop <- pos/max(pos, na.rm = TRUE)
  #pos_prop <- pos_prop/max(pos_prop, na.rm = TRUE)

  idx <- (case_prop + death_prop + hosp_prop)/3
  idx
}

#wastewater: https://github.com/biobotanalytics/covid19-wastewater-data

gs4_deauth()
ri_doh_data <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4", "Trends", na = c("", "NA", "--"))

ri_covid_data <- select(ri_doh_data, date = Date,
                        cases = 'New cases reported, including non-residents (may count people more than once)',
                        #tests = 'Daily total tests completed (may count people more than once)',
                        deaths = 'Number of fatalities among Rhode Island residents \n(date of death)',
                        hospitalized = 'Number of people currently hospitalized',
                        hospitalization = 'Number of hospital admissions',
                        cases_7day_per100k =
                          'Total new Rhode Island resident cases per 100,000 population in the past 7 days',
                        ) %>%
  mutate(#positivity = (cases/tests) * 100,
         risk_index = jwh_covid_risk_index(cases, deaths, hospitalization) * 100,
         risk_index = risk_index/max(risk_index, na.rm = TRUE) * 100,
         cases_7day_per100k = unlist(cases_7day_per100k),
         seven_day_avg_cases = zoo::rollapply(.data$cases, 7,
                                              function(x) mean(x, na.rm = T),
                                              fill = "right", align = "right"),
         seven_day_avg_deaths =
           zoo::rollapply(.data$deaths, 7, mean, fill = "right",
                          align = "right"),
         seven_day_avg_hospitalization =
           zoo::rollapply(.data$hospitalization, 7, mean, fill = "right",
                          align = "right"),
         seven_day_avg_index =
           zoo::rollapply(.data$risk_index, 7, mean, fill = "right",
                          align = "right")) %>%
         #seven_day_avg_positivity =
        #   zoo::rollapply(.data$positivity, 7, mean, fill = "right",
        #                  align = "right")) %>%
  #select(date, cases, deaths, positivity, starts_with("seven")) %>%
  pivot_longer(cols = cases:seven_day_avg_index,
               names_to = "variable",
               values_to = "value") %>%
filter(date >= ymd("2022-03-01"))#today() - 730)

variable_lab <- c("cases_7day_per100k" = "7-Day Cases per 100k",
                  "cases" = "Daily Cases",
                  "seven_day_avg_cases" = "7-Day Avg. Cases",
                  "seven_day_avg_deaths" = "7-Day Avg. Deaths",
                  "seven_day_avg_hospitalization" = "7-day Avg. Hospitalizations",
                  "seven_day_avg_index" = "7-day Avg. Risk Index")
                  #"seven_day_avg_positivity" = "7-day Avg. Positivity")

ri_daily <- ri_covid_data %>%
  filter(variable %in% c("cases", "deaths", "hospitalization",
                         "risk_index")) %>%
  mutate(variable = case_when(variable == "risk_index" ~
                                "seven_day_avg_index",
                              variable == "cases" ~
                                "seven_day_avg_cases",
                              variable == "deaths" ~
                                "seven_day_avg_deaths",
                              #variable == "positivity" ~
                              #  "seven_day_avg_positivity",
                              variable == "hospitalization" ~
                                "seven_day_avg_hospitalization"
                              )) %>%
  mutate(value = round(value, 2))

cases_7day_per100k <- filter(ri_covid_data, date == max(date),
                             variable == "cases_7day_per100k") %>%
  pull(value) %>%
  max() %>%
  round(0)

index_7day <- filter(ri_covid_data, date >= max(lubridate::ymd(ri_covid_data$date)) - 6,
                     variable == "risk_index") %>%
  filter(value > 0) %>%
  pull(value) %>%
  mean(na.rm = TRUE) %>%
  round(0)


ri_plots <- ri_covid_data |>
  mutate(value = round(value, 2)) %>%
  filter(variable %in% c("cases_7day_per100k", "seven_day_avg_cases", "seven_day_avg_deaths",
                         "seven_day_avg_hospitalization", "seven_day_avg_index")) |>
  ggplot(aes(x = date, y = value)) +
  geom_point(data = ri_daily, aes(x = date, y = value), size = 1, color = "grey70") +
  geom_line(linewidth = 1, aes(color = variable)) +
  facet_grid(variable ~ ., scales = "free",
             labeller = labeller(variable = variable_lab)) +
  labs(x = "Date", y = "", title = paste0("Rhode Island COVID-19 (as of: ",
                                          max(ri_covid_data$date), ")"), subtitle = paste0("Cases per 100k in last 7 days: ", cases_7day_per100k)) +
  #theme_ipsum_rc() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("grey30","darkred","darkblue", "darkcyan", "darkorchid1"))
ggsave("ri_plot.jpg", ri_plots, width = 11, height = 8.5)
index <- ggplotly(ri_plots)  %>%
  layout(title = list(text = paste0("Rhode Island COVID-19 (as of: ",
                                    max(ri_covid_data$date), ")",
                                    '<br>',
                                    '<sup>',
                                    paste0("Cases per 100k in last 7 days: ",
                                           cases_7day_per100k),'</sup>',
                                    '<br>',
                                    '<sup>',
                                    paste0("Mean Risk Index in last 7 days: ",
                                           index_7day),'</sup>')))
htmlwidgets::saveWidget(index,
                        here::here("index.html"),
                        selfcontained = FALSE)

system("git add -A")
system(paste0("git commit -m '", lubridate::today(), "update.'" ))
system("git push origin main")
index
