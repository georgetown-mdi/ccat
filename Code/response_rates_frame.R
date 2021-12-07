library(tidyverse)
library(janitor)

#### Download County Response Rates ------------------------------------------

### Pull 2020 county data from Census API
rr2020_raw <- jsonlite::fromJSON("https://api.census.gov/data/2020/dec/responserate?get=CRRALL&for=county:*")

### Convert data to tibble data frame
rr2020_processed <- data.frame(rr2020_raw) %>% 
  as_tibble() %>%
  
  ## Convert first row into the column names
  row_to_names(row_number = 1) %>%
  
  ## Create and rename columns
  transmute(GEOID = paste0(state, county),
            GEOID = str_pad(GEOID, 5, pad = "0"),
            `county-response-rate` = as.numeric(CRRALL),
            year = 2020)

### Pull 2010 county data from Census API
rr2010_raw <- jsonlite::fromJSON("https://api.census.gov/data/2010/dec/responserate?get=FSRR2010&for=county:*")


### Convert data to tibble data frame
rr2010_processed <- data.frame(rr2010_raw) %>% 
  as_tibble() %>%
  
  ## Convert first row into the column names
  row_to_names(row_number = 1) %>%
  
  ## Create and rename columns
  transmute(GEOID = paste0(state, county),
            GEOID = str_pad(GEOID, 5, pad = "0"),
            `county-response-rate` = as.numeric(FSRR2010),
            year = 2010)

### Bind the county response rates for 2010 and 2020 together
rr_counties <- rbind(rr2020_processed, rr2010_processed) %>%
  select(GEOID, year, `county-response-rate`)

#### Download US response rates ----------------------------------------------

### Pull 2020 national data from Census API
rr2020_us_raw <- jsonlite::fromJSON("https://api.census.gov/data/2020/dec/responserate?get=CRRALL&for=us:*")

### Convert data to tibble data frame
rr2020_us_processed <- data.frame(rr2020_us_raw) %>% 
  as_tibble() %>%
  
  ## Convert first row into the column names
  row_to_names(row_number = 1) %>%
  
  ## Create and rename columns
  transmute(`us-response-rate` = as.numeric(CRRALL),
            year = 2020)

### Pull 2010 national data from Census API
rr2010_us_raw <- jsonlite::fromJSON("https://api.census.gov/data/2010/dec/responserate?get=FSRR2010&for=us:*")


### Convert data to tibble data frame
rr2010_us_processed <- data.frame(rr2010_us_raw) %>% 
  as_tibble() %>%
  
  ## Convert first row into the column names
  row_to_names(row_number = 1) %>%
  
  ## Create and rename columns
  transmute(`us-response-rate` = as.numeric(FSRR2010),
            year = 2010)

### Bind the county and US response rates for 2010 and 2020 together
rr_us <- rbind(rr2020_us_processed, rr2010_us_processed) %>%
  select(year, `us-response-rate`)


#### Combine data ------------------------------------------------------------

rr <- full_join(rr_counties, rr_us, by = "year")


#### Fix Alaska GEOIDs -------------------------------------------------------

### combine Chugach and Copper River Census Areas in AK to create Valdez-Cordova Census Area, pre-2020 geography
rr_ak <- rr %>%
  group_by(year) %>%
  filter(GEOID == "02066" | GEOID == "02063") %>% 
  summarise_if(is.numeric, mean) 

### create the new Valdez-Cordova Census Area
rr_ak$GEOID <- "02261"


### Bind with response rates data frame
rr <- rbind(rr, rr_ak)


### write to output
write_csv(rr, "Output/response_rates.csv")
