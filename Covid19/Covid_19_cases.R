# SET UP ==================================

library(dplyr)   # v ‘0.8.0.1’ 
library(ggplot2) # v ‘2.2.1’
library(tidyr)   # v ‘0.6.0’


# GET RAW DATA ============================ 

# Confirmed coronavirus cases -------------

# Data source: https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
# accessed 21 March 2020 
raw_covid_confirmed <- 
  read.csv(
    "~/anala_lytics/Covid19/Covid_19_inputs/time_series_19-covid-Confirmed.txt", 
    stringsAsFactors = FALSE
  ) 


# Country population sizes ----------------

# Data source: https://data.worldbank.org/indicator/SP.POP.TOTL 
# accessed 21 March 2020: 
raw_pop_size <- 
  read.csv(
    "~/anala_lytics/Covid19/Covid_19_inputs/metadata_pop_tot.csv", 
    stringsAsFactors = FALSE
  )


# DATA CLEAN ============================== 

covid_confirmed <-
  raw_covid_confirmed %>% 
  select(
    Country = Country.Region,
    starts_with('X')
  ) %>% 
  rename_at(
    vars(starts_with("X")),
    funs(sub("X", "", .))
  ) %>% 
  group_by(Country) %>% 
  # Sum cases across multiple regions in a country: 
  summarise_each(sum) %>%
  ungroup() %>% 
  # Remove last day, as data not collected: 
  select(-`3.21.20`) 


pop_size <- 
  raw_pop_size %>% 
  select(
    Country = Country.Name,
    # More recent population size data unavailable: 
    pop_2018 = X2018  
  ) %>% 
  mutate(
    # Manually rename US
    Country = if_else(Country == 'United States', 'US', Country)
  )


covid_confirmed_by_pop <- 
  covid_confirmed %>% 
  inner_join(
    pop_size, 
    by = "Country"
  ) %>% 
  mutate_at(
    vars(-Country),
    # Find proportion of country with case:
    funs(. / pop_2018 * 100)
  ) %>% 
  select(
    -pop_2018,
  ) %>% 
  # Keep countries with at least n% population cases recently: 
  filter(`3.20.20` >= 0.02)


covid_confirmed_by_pop_long <- 
  gather( # data transformation for plotting
    covid_confirmed_by_pop, 
    key, 
    number, 
    -Country
  ) %>% 
  mutate(key = as.Date(key, '%m.%d.%y'))


# PLOT ==================================== 

ggplot(
  data = covid_confirmed_by_pop_long, 
  aes(x = key, 
      y = number, 
      group = Country,
      colour = Country)
) + 
  geom_line(
    size = .6, 
    alpha = .8
  ) + 
  theme_minimal()  +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle(
    "Confirmed Covid-19 cases",
    subtitle = "as proportion of country population"
  ) +
  labs(
    x = "Date",
    y = "Confirmed cases / population (%)", 
    caption = "Data current as of 21 March 2020"
  )
