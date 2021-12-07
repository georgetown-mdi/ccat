library(tidyverse)

### read in unemployment data: https://download.bls.gov/pub/time.series/la/
### https://download.bls.gov/pub/time.series/la/la.data.64.County


national_unemployment <- read_csv("Raw/national_unemployment_rates.csv")

county_unemployment <- read_tsv("Raw/la.data.64.County.txt") %>%
  filter(year == 2020,
         period != "M13",
         grepl("LAUCN.+03$",series_id)) %>%
  transmute(GEOID = str_sub(series_id, start = 6, end = 11),
            month = paste0(str_sub(period, start = 2, end = 3), "/01/20"),
            county_unemployment_rate = value)

unemployment <- left_join(county_unemployment, national_unemployment, by = "month")

### Write transformed data to output
write_csv(unemployment, "output/unemployment_2020.csv")
