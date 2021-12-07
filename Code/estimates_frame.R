library(tidyverse)
library(tidycensus)

### Set environmental variables

options(scipen = 999) # No scientific notation

readRenviron("~/.Renviron") # Reload .Renviron file cleanly

#### Requires you to have your api key saved as CENSUS_API_KEY in your .Renviron file
census_api_key(key = Sys.getenv("CENSUS_API_KEY"))

#### Read in 2020 estimates data ------------------------------

# Read in the 2020 population estimates data
pop_est_2020 <- read_csv("Raw/CC-EST2020-ALLDATA6.csv") %>%
  filter(SUMLEV == "050", # only look at counties
         AGEGRP == 0, # only look at total population
         YEAR == 13) %>% # only look at April 1, 2020 estimates
  transmute(STNAME, 
            CTYNAME,
            GEOID = paste0(STATE, COUNTY),
            EST_POP_TOT = TOT_POP, 
            EST_NH_WA_TOT = NHWA_MALE+NHWA_FEMALE, # White Alone Non-Hispanic
            EST_NH_BA_TOT	= NHBA_MALE+NHBA_FEMALE, # Black Alone Non-Hispanic
            EST_H_TOT_TOT	= H_MALE+H_FEMALE, # Hispanic any race
            EST_NH_AI_TOT = NHIA_MALE+NHIA_FEMALE, # American Indian and Alaska Native alone Non-Hispanic
            EST_NH_AA_TOT = NHAA_MALE+NHAA_FEMALE, # Asian Alone Non-Hispanic
            EST_NH_NA_TOT = NHNA_MALE+NHNA_FEMALE, # Native Hawaiian and Other Pacific Islander alone Non-Hispanic
            EST_NH_TOM_TOT = NHTOM_MALE+NHTOM_FEMALE) # Two or more races Non-Hispanic


hu_est_2020 <- read_csv("Raw/hu-est2020.csv") %>%
  filter(SUMLEV == "050") %>% # only look at counties
  transmute(GEOID = paste0(STATE, COUNTY),
            EST_HU_TOT = HUESTIMATE042020) # housing units estimates for 4/1/2020

gq_est_2020 <- read_csv("Raw/co-est2020-alldata.csv") %>%
  filter(SUMLEV == "050") %>% # only look at counties
  transmute(GEOID = paste0(STATE, COUNTY),
            EST_GQ_TOT = round(GQESTIMATES2019 + ((GQESTIMATES2020 - GQESTIMATES2019) / 12) * 9)) # group quarters estimates set to 4/1/2020



#### Merge frames together ------------------------------
  
est_2020 <- left_join(pop_est_2020, hu_est_2020, by = "GEOID") %>%
  left_join(gq_est_2020, by = "GEOID") %>%
  mutate(GEOYEAR = 2020)

#### Add Valdez-Cordova Census Area ------------------------------


vc_area <- est_2020 %>% 
              filter(GEOID == "02063" | GEOID == "02066") %>%
              mutate_if(is.numeric, sum) %>%
              slice(1) %>%
              mutate(GEOID = "02261",
                     GEOYEAR = 2019)

#### Read in ACS data on broadband access ------------------------------


### Read in the 2016-2019 ACS data using tidycensus
acs_county <- get_acs(geography = "county",
                      variables = c(broadband = "B28002_004", # number of people with a broadband internet subscription
                                    total = "B28002_001"), # total sample frame
                      year = 2019) %>%
  select(-NAME, -moe) %>%
  
  ## pivot table so variables have their own columns
  pivot_wider(names_from = "variable", 
              values_from = "estimate") %>%
  
  ## calculate the percent of the population with a broadband internet subscription
  transmute(GEOID,
            PCT_HH_BROADBAND = (broadband/total)*100)

### Copy the Valdez-Cordova Census Area percentage to Copper River and Chugach Census Areas
ak_cr <- acs_county %>% filter(GEOID == "02261") %>%
  mutate(GEOID = "02063") # Copper River

ak_ch <- acs_county %>% filter(GEOID == "02261") %>%
  mutate(GEOID = "02066") # Chugach

### Bind together with the original frame
acs_county_final <- rbind(acs_county, ak_cr, ak_ch)


### Obtain the national broadband rate from tidycensus
acs_national <- get_acs(geography = "us",
                        variables = c(broadband = "B28002_004", # number of people with a broadband internet subscription
                                      total = "B28002_001"), # total sample frame
                        year = 2019) %>%
  select(-GEOID, -NAME, -moe) %>%
  
  ## pivot table so variables have their own columns
  pivot_wider(names_from = "variable", 
              values_from = "estimate") %>%
  
  ## calculate the percent of the population with a broadband internet subscription
  transmute(PCT_HH_BROADBAND_NATIONAL = (broadband/total)*100)


### Bind together with the county frame
acs_final <- cbind(acs_county_final, acs_national)


#### Merge all files together ------------------------------

est_2020_final <- rbind(est_2020, vc_area) %>% 
  
  ## add ACS data on broadband access
  left_join(acs_final, by = "GEOID")

write_csv(est_2020_final, "Raw/estimates.csv")
