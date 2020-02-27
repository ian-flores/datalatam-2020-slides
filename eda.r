library(tidyverse)
library(lubridate)
library(glue)

coronavirus_deaths_ts <- readr::read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')

coronavirus_deaths_ts

tidy_coronavirus_deaths_ts <- coronavirus_deaths_ts %>%
  gather(key = 'date', value = 'num_deaths', -`Province/State`:-Long) %>%
  rename('region' = `Province/State`,
         'country' = `Country/Region`,
         'lat' = `Lat`,
         'long' = `Long`) %>%
  mutate(date = mdy(date))

country_filter <- 'Mainland China'    

country_data <- tidy_coronavirus_deaths_ts %>%
  filter(country == country_filter)

overall_country_data <- country_data %>%
  group_by(date) %>%
  summarize(total_deaths = sum(num_deaths)) 

overall_country_last_date_data <- overall_country_data %>%
  filter(date == max(date))

overall_country_data %>%
  ggplot(aes(x = date, y = total_deaths)) +
    geom_point(colour = 'brown', alpha = 0.5, size = 3) +
    geom_line(alpha = 0.3) +
    theme_minimal() +
    labs(x = 'Date',
         y = 'Deaths Reported',
         title = glue('Number of deaths per day in {country_filter}'),
         subtitle = glue('Latest date available: {overall_country_last_date_data$date} ({overall_country_last_date_data$total_deaths} deaths)'))

library(forcats)

region_country_data <- country_data %>%
  group_by(region) %>%
  summarize(total_deaths = sum(num_deaths))

region_country_data %>%
  ggplot(aes(x = fct_reorder(region, total_deaths), y = total_deaths)) +
    geom_bar(stat = 'identity', fill = 'brown', alpha = 0.5) +
    coord_flip() +
    theme_minimal() +
    labs(x = 'Region',
         y = 'Total Deaths',
         title = glue('Total deaths by region in {country_filter}'))

country_data %>%
  mutate(month = month(date)) %>%
  group_by(region, month) %>%
  summarize(num_deaths = sum(num_deaths)) %>%
  arrange(desc(num_deaths)) %>%
  knitr::kable()
