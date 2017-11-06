library(tidyverse)
library(lubridate)

inflation_data <- read_csv(file = "CPIAUCSL.csv")

annual_cpi_average <- inflation_data %>%
  mutate(year = year(DATE)) %>%
  group_by(year) %>%
  summarise(annual_cpi = mean(CPIAUCSL)) %>%
  filter((year <= 2011 & year >= 1993))

base_cpi <-
  annual_cpi_average[annual_cpi_average$year == 2011,]$annual_cpi

annual_cpi_average <-
  annual_cpi_average %>%
  mutate(scaled_cpi = (annual_cpi / base_cpi) * 100)

storm_data <- storm_data %>%
  left_join(x = ., y = annual_cpi_average, by = "year") %>%
  select(-annual_cpi) %>%
  mutate(adjusted_damages_cost = (damages_cost / scaled_cpi) * 100)


