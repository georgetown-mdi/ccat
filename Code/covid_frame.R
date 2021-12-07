library(tidyverse)
library(lubridate)


##### Read in the data --------------------------------------------------------


### Read in the 2020 census counts data
geoids <- read_csv("Raw/county_pl.csv") %>%
  
  ## turn the geoid into a string
  mutate(GEOID = str_pad(fips, 5, pad = "0")) %>% 
  
  ## select all county geoids as a linkage base
  select(GEOID) 


### Read in the national COVID-19 case data, originally from the NYT (us.csv)
### https://github.com/nytimes/covid-19-data/tree/master/rolling-averages
nyt_national <- read_csv("Raw/us-covid-cases.csv") %>%
  
  ## add weekdays
  mutate(day = wday(date,label=T)) %>% 
  
  ## only select Sundays in 2020 for the base weekly averages
  filter(date > "2020-01-01" & date < "2021-01-01" & day == "Sun") %>% 
  
  ## select variables needed
  select(date, national_cases_avg_per_100k = cases_avg_per_100k) 


### Read in the county-level COVID-19 case data, originally from the NYT (us-counties.csv)
### https://github.com/nytimes/covid-19-data/tree/master/rolling-averages
nyt_county <- read_csv("Raw/us-counties-covid-cases.csv") %>%
  
  ## clean up GEOIDs and add weekdays
  mutate(GEOID = str_remove(geoid, "USA-"), 
         day = wday(date,label=T)) %>% 
  
  ## only select Sundays in 2020 for the base weekly averages
  filter(date > "2020-01-01" & date < "2021-01-01" & day == "Sun") %>% 
  
  ## select variables needed
  select(GEOID, date, county_cases_avg_per_100k = cases_avg_per_100k) 



#### Merge the data frames ---------------------------------------------------


### Merge national COVID-19 and county GEOIDs so there are no missing counties 
nyt_dates <- left_join(nyt_national, geoids, by = character()) %>%
  
  ## Join county-level COVID-19 weekly case averages
  left_join(nyt_county, by = c("date", "GEOID")) %>%  
  
  ## select only needed vars
  select(GEOID, everything())


#### Transform GEOIDs to fix AK counties -------------------------------------

### Select Chugach and Copper River Census Areas
nyt_ak <- nyt_dates %>%
  filter(GEOID == "02066" | GEOID == "02063") %>% 
  
  ## Group by week
  group_by(date) %>%
  
  ## Summarize national and county case averages across weeks
  summarise_if(is.numeric, sum) 

### Add the Valdez-Cordova Census Area GEOID to these weeks
nyt_ak$GEOID <- "02261"


### Bind the Valdez-Cordova Census Area summary to the rest of the NYT data
nyt_dates <- rbind(nyt_dates, nyt_ak)

### write data to output
write_csv(nyt_dates, "Output/nyt_covid.csv")
