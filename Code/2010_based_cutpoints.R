library(tidyverse)
library(tidycensus)

# Set environmental variables

options(scipen = 999) # No scientific notation

readRenviron("~/.Renviron") # Reload .Renviron file cleanly

# Requires you to have your api key saved as CENSUS_API_KEY in your .Renviron file
census_api_key(key = Sys.getenv("CENSUS_API_KEY"))

# Get variables for 2010 PL and SF1 and 2020 PL

vars_2010 <- load_variables(2010, "pl", cache = TRUE)
vars_2020 <- load_variables(2020, "pl", cache = TRUE)
vars_2010_sf1 <- load_variables(2010, "sf1", cache = TRUE)

# pull in evaluation estimates -- must download them because they are not on API
# all data downloaded from: 
# https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.2010.html
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2012/counties/asrh/

v2010_components <- read_csv("Raw/co-est2010-alldata.csv") #used for GQ only
v2010_characteristics <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010/2010-eval-estimates/cc-est2010-alldata.csv") #used for characteristics and total
v2010_housing <- read_csv("Raw/hu-est2010.csv") #used for housing units
v2012_characteristics <- read_csv("Raw/cc-est2012-alldata.csv") #used for Census 2010 characteristics
v2010_state <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010/2010-eval-estimates/sc-est2010-alldata6.csv")
mrf1_2010 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010/modified-race-data-2010/stco-mr2010_al_mo.csv")
mrf2_2010 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010/modified-race-data-2010/stco-mr2010_mt_wy.csv") %>%
  mutate(STATE = as.character(STATE))

sf1_length <- c(1:49)

sf1_char <- ifelse(sf1_length < 10, paste0("0", as.character(sf1_length)), as.character(sf1_length))

vec_sf1 <- c(paste0("P0120", sf1_char))

v2012_agesex <- get_decennial(geography = "county", 
                              variables = vec_sf1, 
                              year = 2010, 
                              sumfile = "sf1")

mrf_2010 <- mrf1_2010 %>%
  bind_rows(mrf2_2010)

##### Pull PL Data using tidycensus

# Create variable vectors for census to pull race by hispanic origin for total and 18+
# Must get GQ data from the SF1 as it is not in the PL data on the API

list_length <- c(1:73)

list_char <- ifelse(list_length < 10, paste0("0", as.character(list_length)), as.character(list_length))

vec_2010 <- c(paste0("P0020", list_char), paste0("P0040", list_char))

redist_2010 <- get_decennial(geography = "county", 
                             variables = vec_2010, 
                             year = 2010, 
                             sumfile = "pl")

sf1_2010 <- get_decennial(geography = "county",
                          variables = c(GQ_TOT = "P042001"),
                          year = 2010,
                          sumfile = "sf1")

hu_2010 <- get_decennial(geography = "county",
                         variables = c(HU_TOT = "H001001",
                                       HU_VAC = "H001003"),
                         year = 2010,
                         sumfile = "pl")


#### Trim estimates datasets to needed variables

# GQ Only -- they did not publish April, 2010 estimates so I have to do my own interpolation.
v2010_gq <- v2010_components %>%
  filter(SUMLEV == "050") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  select(GEOID, STNAME, CTYNAME, GQESTIMATES2010, GQESTIMATES2009) %>%
  mutate(GQ_TOT = round(GQESTIMATES2009 + ((GQESTIMATES2010 - GQESTIMATES2009) / 12) * 9))%>%
  select(GEOID, GQ_TOT)

# HU only
v2010_hu <- v2010_housing %>%
  filter(sumlev == "050") %>%
  mutate(GEOID = paste0(state, county)) %>%
  select(GEOID, huest_042010)

# Proportion of 18+ vs 20+ in state file

state_18p_tot <- v2010_state %>%
  select(STATE, SEX, ORIGIN, RACE, AGE, POPESTIMATE42010) %>%
  filter(SEX == 0,
         ORIGIN == 0,
         AGE > 17) %>%
  group_by(STATE) %>%
  summarize(TOT_TOT_18P = sum(POPESTIMATE42010))

state_20p_tot <- v2010_state %>%
  select(STATE, SEX, ORIGIN, RACE, AGE, POPESTIMATE42010) %>%
  filter(SEX == 0,
         ORIGIN == 0,
         AGE > 19) %>%
  group_by(STATE) %>%
  summarize(TOT_TOT_20P = sum(POPESTIMATE42010))

state_18p_hisp <- v2010_state %>%
  select(STATE, SEX, ORIGIN, RACE, AGE, POPESTIMATE42010) %>%
  filter(SEX == 0,
         ORIGIN == 2,
         AGE > 17) %>%
  group_by(STATE) %>%
  summarize(H_TOT_18P = sum(POPESTIMATE42010))

state_20p_hisp <- v2010_state %>%
  select(STATE, SEX, ORIGIN, RACE, AGE, POPESTIMATE42010) %>%
  filter(SEX == 0,
         ORIGIN == 2,
         AGE > 19) %>%
  group_by(STATE) %>%
  summarize(H_TOT_20P = sum(POPESTIMATE42010))

state_18p_nhisp <- v2010_state %>%
  select(STATE, SEX, ORIGIN, RACE, AGE, POPESTIMATE42010) %>%
  filter(SEX == 0,
         ORIGIN == 1,
         AGE > 17) %>%
  group_by(STATE, RACE) %>%
  summarize(NH = sum(POPESTIMATE42010)) %>%
  ungroup() %>%
  mutate(RACE = ifelse(RACE == 1, "WA",
                ifelse(RACE == 2, "BA",
                ifelse(RACE == 3, "AI",
                ifelse(RACE == 4, "AA",
                ifelse(RACE == 5, "NA", "TOM")))))) %>%
  pivot_wider(names_from = RACE, values_from = c(NH), names_glue = "NH_{RACE}_18P" )

state_20p_nhisp <- v2010_state %>%
  select(STATE, SEX, ORIGIN, RACE, AGE, POPESTIMATE42010) %>%
  filter(SEX == 0,
         ORIGIN == 1,
         AGE > 19) %>%
  group_by(STATE, RACE) %>%
  summarize(NH = sum(POPESTIMATE42010)) %>%
  ungroup() %>%
  mutate(RACE = ifelse(RACE == 1, "WA",
                       ifelse(RACE == 2, "BA",
                              ifelse(RACE == 3, "AI",
                                     ifelse(RACE == 4, "AA",
                                            ifelse(RACE == 5, "NA", "TOM")))))) %>%
  pivot_wider(names_from = RACE, values_from = c(NH), names_glue = "NH_{RACE}_20P" )

state_props <- state_18p_tot %>%
  left_join(state_20p_tot, by = c("STATE")) %>%
  left_join(state_18p_nhisp, by = c("STATE")) %>%
  left_join(state_20p_nhisp, by = c("STATE")) %>%
  left_join(state_18p_hisp, by = c("STATE")) %>%
  left_join(state_20p_hisp, by = c("STATE")) %>%
  pivot_longer(cols = c(2:17), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("hisp", "race", "age"), sep = "_") %>%
  pivot_wider(names_from = age, values_from = value, names_glue = "age_{age}") %>%
  mutate(age_prop = age_18P / age_20P) %>%
  select(STATE, hisp, race, age_prop)
  

# apply these rates to the 20+ in the county file

v2010_18p <- v2010_characteristics %>%
  filter(SUMLEV == 50, # only look at counties
         AGEGRP >= 5, # only look at 20+
         YEAR == 13) %>% # only look at April 1, 2020 estimates
  mutate(STATE = ifelse(STATE < 10, paste0("0", as.character(STATE)), as.character(STATE)),
         COUNTY = ifelse(COUNTY < 10, paste0("00", as.character(COUNTY)),
                         ifelse(COUNTY < 100, paste0("0", as.character(COUNTY)), as.character(COUNTY)))) %>%
  transmute(STNAME, 
            CTYNAME,
            STATE,
            GEOID = paste0(STATE, COUNTY),
            EST_POP_TOT = TOT_POP, 
            EST_NH_WA_TOT = NHWA_MALE+NHWA_FEMALE, # White Alone Non-Hispanic
            EST_NH_BA_TOT	= NHBA_MALE+NHBA_FEMALE, # Black Alone Non-Hispanic
            EST_H_TOT_TOT	= H_MALE+H_FEMALE, # Hispanic any race
            EST_NH_AI_TOT = NHIA_MALE+NHIA_FEMALE, # American Indian and Alaska Native alone Non-Hispanic
            EST_NH_AA_TOT = NHAA_MALE+NHAA_FEMALE, # Asian Alone Non-Hispanic
            EST_NH_NA_TOT = NHNA_MALE+NHNA_FEMALE, # Native Hawaiian and Other Pacific Islander alone Non-Hispanic
            EST_NH_TOM_TOT = NHTOM_MALE+NHTOM_FEMALE) %>% # Two or more races Non-Hispanic
  group_by(STATE,STNAME, CTYNAME, GEOID) %>%
  summarize(EST_TOT_TOT_20P = sum(EST_POP_TOT),
            EST_NH_WA_20P = sum(EST_NH_WA_TOT),
            EST_NH_BA_20P = sum(EST_NH_BA_TOT),
            EST_H_TOT_20P = sum(EST_H_TOT_TOT),
            EST_NH_AI_20P = sum(EST_NH_AI_TOT),
            EST_NH_AA_20P = sum(EST_NH_AA_TOT),
            EST_NH_NA_20P = sum(EST_NH_NA_TOT),
            EST_NH_TOM_20P = sum(EST_NH_TOM_TOT)) %>%
  ungroup() %>%
  pivot_longer(cols = c(5:12), names_to = "variable", values_to = "val_20P") %>%
  separate(variable, into = c("est", "hisp", "race", "age"), sep = "_") %>%
  full_join(state_props, by = c("STATE", "hisp", "race")) %>%
  mutate(`18P` = round(val_20P * age_prop)) %>%
  select(GEOID, STNAME, CTYNAME, hisp, race, `18P`) %>%
  pivot_wider(names_from = c(hisp, race), names_glue = "EST_{hisp}_{race}_18P", values_from = `18P`) %>%
  mutate(TOT_COMP = EST_NH_WA_18P + EST_NH_BA_18P + EST_H_TOT_18P + EST_NH_AI_18P +
           EST_NH_AA_18P + EST_NH_NA_18P + EST_NH_TOM_18P,
         DIFF = TOT_COMP - EST_TOT_TOT_18P,
         NH_WA_18P = EST_NH_WA_18P - (round(EST_NH_WA_18P/TOT_COMP*DIFF)),
         NH_BA_18P = EST_NH_BA_18P - (round(EST_NH_BA_18P/TOT_COMP*DIFF)),
         H_TOT_18P = EST_H_TOT_18P - (round(EST_H_TOT_18P/TOT_COMP*DIFF)),
         NH_AI_18P = EST_NH_AI_18P - (round(EST_NH_AI_18P/TOT_COMP*DIFF)),
         NH_AA_18P = EST_NH_AA_18P - (round(EST_NH_AA_18P/TOT_COMP*DIFF)),
         NH_NA_18P = EST_NH_NA_18P - (round(EST_NH_NA_18P/TOT_COMP*DIFF)),
         NH_TOM_18P = EST_NH_TOM_18P - (round(EST_NH_TOM_18P/TOT_COMP*DIFF)),
         TOT_COMP = EST_NH_WA_18P + EST_NH_BA_18P + EST_H_TOT_18P + EST_NH_AI_18P +
           EST_NH_AA_18P + EST_NH_NA_18P + EST_NH_TOM_18P,
         DIFF = TOT_COMP - EST_TOT_TOT_18P,
         NH_WA_18P = NH_WA_18P - DIFF,
         POP_18P = EST_TOT_TOT_18P,
         NH_WA_18P_PROP = NH_WA_18P / POP_18P,
         NH_BA_18P_PROP = NH_BA_18P / POP_18P,
         NH_AI_18P_PROP = NH_AI_18P / POP_18P,
         NH_AA_18P_PROP = NH_AA_18P / POP_18P,
         NH_NA_18P_PROP = NH_NA_18P / POP_18P,
         NH_TOM_18P_PROP = NH_TOM_18P / POP_18P,
         H_TOT_18P_PROP = H_TOT_18P / POP_18P) %>%
  select(GEOID, STNAME, CTYNAME, POP_18P, NH_WA_18P, NH_BA_18P, H_TOT_18P, 
         NH_AI_18P, NH_AA_18P, NH_NA_18P, NH_TOM_18P, NH_WA_18P_PROP, NH_BA_18P_PROP, 
         H_TOT_18P_PROP, NH_AI_18P_PROP, NH_AA_18P_PROP, NH_NA_18P_PROP, NH_TOM_18P_PROP) 
  

# create the final estimates file -- start by bringing in race and total pop for April, 2020
v2010_chars_tot <- v2010_characteristics %>%
  filter(YEAR == "13" & AGEGRP == "0") %>%
  mutate(POP_TOT = TOT_POP,
         NH_WA_TOT = NHWA_MALE + NHWA_FEMALE,
         NH_BA_TOT = NHBA_MALE + NHBA_FEMALE,
         NH_AI_TOT = NHIA_MALE + NHIA_FEMALE,
         NH_AA_TOT = NHAA_MALE + NHAA_FEMALE,
         NH_NA_TOT = NHNA_MALE + NHNA_FEMALE,
         NH_TOM_TOT = NHTOM_MALE + NHTOM_FEMALE,
         H_TOT_TOT = H_MALE + H_FEMALE,
         STATE = ifelse(STATE < 10, paste0("0", as.character(STATE)), as.character(STATE)),
         COUNTY = ifelse(COUNTY < 10, paste0("00", as.character(COUNTY)),
                         ifelse(COUNTY < 100, paste0("0", as.character(COUNTY)), as.character(COUNTY))),
         GEOID = paste0(STATE, COUNTY),
         NH_WA_PROP = NH_WA_TOT / POP_TOT,
         NH_BA_PROP = NH_BA_TOT / POP_TOT,
         NH_AI_PROP = NH_AI_TOT / POP_TOT,
         NH_AA_PROP = NH_AA_TOT / POP_TOT,
         NH_NA_PROP = NH_NA_TOT / POP_TOT,
         NH_TOM_PROP = NH_TOM_TOT / POP_TOT,
         H_TOT_PROP = H_TOT_TOT / POP_TOT) %>%
  select(-c(1, 2, 3, 6:80)) %>%
  relocate(GEOID) %>%
  
  full_join(v2010_18p, by = c("GEOID", "STNAME", "CTYNAME")) %>%
  mutate(POP_17U = POP_TOT - POP_18P,
         NH_WA_17U = NH_WA_TOT - NH_WA_18P,
         NH_BA_17U = NH_BA_TOT - NH_BA_18P,
         NH_AI_17U = NH_AI_TOT - NH_AI_18P,
         NH_AA_17U = NH_AA_TOT - NH_AA_18P,
         NH_NA_17U = NH_NA_TOT - NH_NA_18P,
         NH_TOM_17U = NH_TOM_TOT - NH_TOM_18P,
         H_TOT_17U = H_TOT_TOT - H_TOT_18P,
         NH_WA_17U_PROP = NH_WA_17U / POP_17U,
         NH_BA_17U_PROP = NH_BA_17U / POP_17U,
         NH_AI_17U_PROP = NH_AI_17U / POP_17U,
         NH_AA_17U_PROP = NH_AA_17U / POP_17U,
         NH_NA_17U_PROP = NH_NA_17U / POP_17U,
         NH_TOM_17U_PROP = NH_TOM_17U / POP_17U,
         H_TOT_17U_PROP = H_TOT_17U / POP_17U) %>%
  
  #Join in HU data and rename
  full_join(v2010_hu, by = c("GEOID")) %>%
  rename(HU_TOT = huest_042010) %>%
  
  # Join in GQ data
  full_join(v2010_gq, by = c("GEOID")) %>%
  
  # Create a Household pop number so that we can create a proper persons per household
  mutate(HH_TOT = POP_TOT - GQ_TOT,
         PPH_RAT = HH_TOT / HU_TOT) %>%
  select(-HH_TOT) %>%
  
  # Create a long file for easier calculations later
  pivot_longer(cols = c(4:51), names_to = "variable", values_to = "est_2010")

### Trim census datasets to needed variables

# GQ only. Must come from the SF1 because GQ was not on the PL in 2010
gq_2010 <- sf1_2010 %>%
  mutate(st = str_sub(GEOID, 1, 2)) %>%
  filter(!(st == "72")) %>%
  select(-st, -NAME, -variable) %>%
  rename(GQ_TOT = value)

# Total population and 18 plus totals
totpop_2010 <- redist_2010 %>%
  filter(variable == "P002001" | variable == "P004001") %>%
  mutate(st = str_sub(GEOID, 1, 2),
         variable = ifelse(variable == "P002001", "POP_TOT", "POP_18P")) %>%
  filter(!(st == "72")) %>%
  select(-st, -NAME) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(POP_17U = POP_TOT - POP_18P)

# Housing units (total and vacant)
hu_cen2010 <- hu_2010 %>%
  mutate(st = str_sub(GEOID, 1, 2)) %>%
  filter(!(st == "72")) %>%
  select(-st, -NAME) %>%
  pivot_wider(names_from = variable, values_from = value)

# Create 18+ vs 20+ proportions from V2012 Age/Sex for Census 2010
cen2010_18P_prop <- v2012_characteristics %>%
  
  filter(YEAR == 1 & AGEGRP >= 5) %>%
  
  transmute(STNAME, 
            CTYNAME,
            STATE,
            GEOID = paste0(STATE, COUNTY),
            EST_POP_TOT = TOT_POP) %>% 
  group_by(GEOID) %>%
  summarize(POP_20P = sum(EST_POP_TOT)) %>%
  full_join(totpop_2010, by = c("GEOID")) %>%
  mutate(tot_prop = POP_18P / POP_20P) %>%
  select(GEOID, tot_prop)

cen2010_18p <- v2012_characteristics %>%
  filter(SUMLEV == "050", # only look at counties
         AGEGRP >= 5, # only look at 20+
         YEAR == 1) %>% # only look at April 1, 2020 estimates
  transmute(STNAME, 
            CTYNAME,
            STATE,
            GEOID = paste0(STATE, COUNTY),
            EST_POP_TOT = TOT_POP, 
            EST_NH_WA_TOT = NHWA_MALE+NHWA_FEMALE, # White Alone Non-Hispanic
            EST_NH_BA_TOT	= NHBA_MALE+NHBA_FEMALE, # Black Alone Non-Hispanic
            EST_H_TOT_TOT	= H_MALE+H_FEMALE, # Hispanic any race
            EST_NH_AI_TOT = NHIA_MALE+NHIA_FEMALE, # American Indian and Alaska Native alone Non-Hispanic
            EST_NH_AA_TOT = NHAA_MALE+NHAA_FEMALE, # Asian Alone Non-Hispanic
            EST_NH_NA_TOT = NHNA_MALE+NHNA_FEMALE, # Native Hawaiian and Other Pacific Islander alone Non-Hispanic
            EST_NH_TOM_TOT = NHTOM_MALE+NHTOM_FEMALE) %>% # Two or more races Non-Hispanic
  group_by(STATE,STNAME, CTYNAME, GEOID) %>%
  summarize(EST_TOT_TOT_20P = sum(EST_POP_TOT),
            EST_NH_WA_20P = sum(EST_NH_WA_TOT),
            EST_NH_BA_20P = sum(EST_NH_BA_TOT),
            EST_H_TOT_20P = sum(EST_H_TOT_TOT),
            EST_NH_AI_20P = sum(EST_NH_AI_TOT),
            EST_NH_AA_20P = sum(EST_NH_AA_TOT),
            EST_NH_NA_20P = sum(EST_NH_NA_TOT),
            EST_NH_TOM_20P = sum(EST_NH_TOM_TOT)) %>%
  ungroup() %>%
  pivot_longer(cols = c(5:12), names_to = "variable", values_to = "val_20P") %>%
  separate(variable, into = c("est", "hisp", "race", "age"), sep = "_") %>%
  full_join(cen2010_18P_prop, by = c("GEOID")) %>%
  mutate(`18P` = round(val_20P * tot_prop)) %>%
  select(GEOID, STNAME, CTYNAME, hisp, race, `18P`) %>%
  pivot_wider(names_from = c(hisp, race), names_glue = "EST_{hisp}_{race}_18P", values_from = `18P`) %>%
  mutate(TOT_COMP = EST_NH_WA_18P + EST_NH_BA_18P + EST_H_TOT_18P + EST_NH_AI_18P +
           EST_NH_AA_18P + EST_NH_NA_18P + EST_NH_TOM_18P,
         DIFF = TOT_COMP - EST_TOT_TOT_18P,
         NH_WA_18P = EST_NH_WA_18P - (round(EST_NH_WA_18P/TOT_COMP*DIFF)),
         NH_BA_18P = EST_NH_BA_18P - (round(EST_NH_BA_18P/TOT_COMP*DIFF)),
         H_TOT_18P = EST_H_TOT_18P - (round(EST_H_TOT_18P/TOT_COMP*DIFF)),
         NH_AI_18P = EST_NH_AI_18P - (round(EST_NH_AI_18P/TOT_COMP*DIFF)),
         NH_AA_18P = EST_NH_AA_18P - (round(EST_NH_AA_18P/TOT_COMP*DIFF)),
         NH_NA_18P = EST_NH_NA_18P - (round(EST_NH_NA_18P/TOT_COMP*DIFF)),
         NH_TOM_18P = EST_NH_TOM_18P - (round(EST_NH_TOM_18P/TOT_COMP*DIFF)),
         TOT_COMP = EST_NH_WA_18P + EST_NH_BA_18P + EST_H_TOT_18P + EST_NH_AI_18P +
           EST_NH_AA_18P + EST_NH_NA_18P + EST_NH_TOM_18P,
         DIFF = TOT_COMP - EST_TOT_TOT_18P,
         NH_WA_18P = NH_WA_18P - DIFF,
         POP_18P = EST_TOT_TOT_18P,
         NH_WA_18P_PROP = NH_WA_18P / POP_18P,
         NH_BA_18P_PROP = NH_BA_18P / POP_18P,
         NH_AI_18P_PROP = NH_AI_18P / POP_18P,
         NH_AA_18P_PROP = NH_AA_18P / POP_18P,
         NH_NA_18P_PROP = NH_NA_18P / POP_18P,
         NH_TOM_18P_PROP = NH_TOM_18P / POP_18P,
         H_TOT_18P_PROP = H_TOT_18P / POP_18P) %>%
  select(GEOID, STNAME, CTYNAME, POP_18P, NH_WA_18P, NH_BA_18P, H_TOT_18P, 
         NH_AI_18P, NH_AA_18P, NH_NA_18P, NH_TOM_18P, NH_WA_18P_PROP, NH_BA_18P_PROP, 
         H_TOT_18P_PROP, NH_AI_18P_PROP, NH_AA_18P_PROP, NH_NA_18P_PROP, NH_TOM_18P_PROP)

# Create final Census 2010 file for comparison to estimates
cen2010_chars_tot <- v2012_characteristics %>%
  
  filter(YEAR == "1" & AGEGRP == "0") %>%
  mutate(POP_TOT = TOT_POP,
         NH_WA_TOT = NHWA_MALE + NHWA_FEMALE,
         NH_BA_TOT = NHBA_MALE + NHBA_FEMALE,
         NH_AI_TOT = NHIA_MALE + NHIA_FEMALE,
         NH_AA_TOT = NHAA_MALE + NHAA_FEMALE,
         NH_NA_TOT = NHNA_MALE + NHNA_FEMALE,
         NH_TOM_TOT = NHTOM_MALE + NHTOM_FEMALE,
         H_TOT_TOT = H_MALE + H_FEMALE,
         GEOID = paste0(STATE, COUNTY),
         NH_WA_PROP = NH_WA_TOT / POP_TOT,
         NH_BA_PROP = NH_BA_TOT / POP_TOT,
         NH_AI_PROP = NH_AI_TOT / POP_TOT,
         NH_AA_PROP = NH_AA_TOT / POP_TOT,
         NH_NA_PROP = NH_NA_TOT / POP_TOT,
         NH_TOM_PROP = NH_TOM_TOT / POP_TOT,
         H_TOT_PROP = H_TOT_TOT / POP_TOT) %>%
  select(-c(1, 2, 3, 6:80)) %>%
  relocate(GEOID) %>%
  
  full_join(cen2010_18p, by = c("GEOID", "STNAME", "CTYNAME")) %>%
  mutate(POP_17U = POP_TOT - POP_18P,
         NH_WA_17U = NH_WA_TOT - NH_WA_18P,
         NH_BA_17U = NH_BA_TOT - NH_BA_18P,
         NH_AI_17U = NH_AI_TOT - NH_AI_18P,
         NH_AA_17U = NH_AA_TOT - NH_AA_18P,
         NH_NA_17U = NH_NA_TOT - NH_NA_18P,
         NH_TOM_17U = NH_TOM_TOT - NH_TOM_18P,
         H_TOT_17U = H_TOT_TOT - H_TOT_18P,
         NH_WA_17U_PROP = NH_WA_17U / POP_17U,
         NH_BA_17U_PROP = NH_BA_17U / POP_17U,
         NH_AI_17U_PROP = NH_AI_17U / POP_17U,
         NH_AA_17U_PROP = NH_AA_17U / POP_17U,
         NH_NA_17U_PROP = NH_NA_17U / POP_17U,
         NH_TOM_17U_PROP = NH_TOM_17U / POP_17U,
         H_TOT_17U_PROP = H_TOT_17U / POP_17U) %>%
  
  # Join in total gq and hu data
  full_join(gq_2010, by = c("GEOID")) %>%
  full_join(hu_cen2010, by = c("GEOID")) %>%
  
  # calculate proportions for later use as well as coding size of county
  mutate(HH_TOT = POP_TOT - GQ_TOT,
         PPH_RAT = HH_TOT / HU_TOT,
         SIZE_CLASS = case_when(POP_TOT >= 100000 ~ "Large",
                                POP_TOT >= 40000  ~ "Medium",
                                POP_TOT <  40000  ~ "Small")) %>%
  select(-HH_TOT, -HU_VAC) %>%
  
  # Make a longer file so it is easier to calculate differences
  pivot_longer(cols = c(4:51), names_to = "variable", values_to = "cen_2010")

### Comparison file

# Create our final grades for everything but vacancy

final_output <- v2010_chars_tot %>%
  full_join(cen2010_chars_tot, by = c("GEOID", "STNAME", "CTYNAME", "variable")) %>%
  
  # Not currently grading by race/ethnicity pop totals, but instead shares
  filter(!(variable %in% c("NH_WA_TOT", "NH_BA_TOT", "NH_AI_TOT", "NH_AA_TOT", "NH_NA_TOT", "NH_TOM_TOT", "H_TOT_TOT",
                           "NH_WA_18P", "NH_BA_18P", "NH_AI_18P", "NH_AA_18P", "NH_NA_18P", "NH_TOM_18P", "H_TOT_18P",
                           "NH_WA_17U", "NH_BA_17U", "NH_AI_17U", "NH_AA_17U", "NH_NA_17U", "NH_TOM_17U", "H_TOT_17U"))) %>%
  
  # Calculate the differences we care about (numeric or percentage) depending on variable
  mutate(difference = ifelse(variable %in% c("POP_TOT", "POP_18P", "POP_17U", "HU_TOT", "GQ_TOT"), 
                             (cen_2010 - est_2010) / est_2010, 
                             cen_2010 - est_2010),
         difference = ifelse(is.na(difference) | is.infinite(difference), 0, difference)) %>%
  
  # Creating grading cells by VARIABLE and SIZE of county (based on DHC suggestions)
  group_by(variable, SIZE_CLASS) %>%
  
  # Create our measurement groups (over/under 1 and 2 standard deviations of each measure)
  summarize(count = n(),
            mean = mean(difference),
            sd = sd(difference),
            range_1_high = mean + sd,
            range_1_low = mean - sd,
            range_2_high = mean + 2*sd,
            range_2_low = mean - 2*sd)

write_csv(final_output, "Raw/cutpoints.csv")

## Calculate and output vacancy rate

final_vacancy <- hu_cen2010 %>%
  
  #deal with geography changes between 2010 and 2020
  mutate(GEOID = ifelse(GEOID == "51515", "51019",
                        ifelse(GEOID == "46113", "46102",
                               ifelse(GEOID == "02270", "02158", GEOID)))) %>%
  
  # Summarize because Bedford City, VA was subsumed by Bedford County
  group_by(GEOID) %>%
  summarize(HU_VAC = sum(HU_VAC),
            HU_TOT = sum(HU_TOT)) %>%
  
  # Calculate Vacancy percent
  mutate(cen2010_pct_vacant_hu = (HU_VAC / HU_TOT) *100) %>%
  select(GEOID, cen2010_pct_vacant_hu)

# Fixing for Alaska changes
ak_fix1 <- final_vacancy %>%
  filter(GEOID == "02261") %>%
  mutate(GEOID = "02066")

ak_fix2 <- final_vacancy %>%
  filter(GEOID == "02261") %>%
  mutate(GEOID = "02063")


# Create final file with AK changes
final_vacancy <- final_vacancy %>%
  bind_rows(ak_fix1) %>%
  bind_rows(ak_fix2)

write_csv(final_vacancy, "Raw/cen2010_vacancy.csv")
