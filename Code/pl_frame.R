######################################################################################################
### Program to create modified race from a some other race file and load all PL data into repo
###
### Description: For many use cases there is a need for creating race data without 
###              including "Some Other Race". However, according to the Census Bureau
###              we will not see a modified race file until at least Fall of 2022. This
###              program creates a modified race file for the 2020 PL data for use 
###              until the official data are released.
###
### Methodology: In order to create a modified race file we proceed in a set of logical
###              (if imperfect) steps. After we read in the necessary data and set up our
###              environment, we proceed through the following:
###                1. Calculate the pre and post modified race data for each NH race group from
###                   Census 2010. This has to be done in several steps: (a) recode all SOR
###                   in combination populations to the other races that it was in combination
###                   with, then (b) calculate the proportion of SOR alone that was recoded to 
###                   each race group.
###                2. Create a share of SOR for each NH race group from 2010.
###                3. For 2020, recode all SOR in combination to the other groups they are in
###                   combination with.
###                4. Apply 2010 proportions for SOR alone to the 2020 data.
###
###  
###
########################################################################################################

library(tidyverse)
library(tidycensus)

readRenviron("~/.Renviron")

options(scipen = 999)

census_api_key(key = Sys.getenv("CENSUS_API_KEY"))

# National level cenrace to imprace conversion factors from file here: 
# https://www.census.gov/data/datasets/2010/demo/popest/modified-race-data-2010.html

sor_to_white <- 0.5984
sor_to_black <- 0.2478
sor_to_aian  <- 0.0116
sor_to_asian <- 0.1084
sor_to_nhpi  <- 0.0059
sor_to_multi <- 0.0278


list_length <- c(1:73)

list_char <- ifelse(list_length < 10, paste0("0", as.character(list_length)), as.character(list_length))

vec_2020 <- c(paste0("P2_0", list_char, "N"), paste0("P4_0", list_char, "N"))

vec_gqhu <- c(hu_total = "H1_001N", 
              hu_occupied = "H1_002N", 
              hu_vacant = "H1_003N",
              gq_total = "P5_001N",
              gq_inst = "P5_002N",
              gq_inst_corr_adult = "P5_003N",
              gq_inst_juv = "P5_004N",
              gq_inst_nursing = "P5_005N",
              gq_inst_other = "P5_006N",
              gq_noninst = "P5_007N",
              gq_noninst_college = "P5_008N",
              gq_noninst_military = "P5_009N",
              gq_noninst_other = "P5_010N")


redist_2020 <- get_decennial(geography = "county", 
                             variables = vec_2020, 
                             year = 2020, 
                             sumfile = "pl")

hugq_2020 <- get_decennial(geography = "county",
                           variables = vec_gqhu,
                           year = 2020,
                           sumfile = "pl")

redist_wide_2020 <- redist_2020 %>%
  mutate(table = str_sub(variable, -6, -6),
         varnum = str_sub(variable, -3, -2),
         age = ifelse(table == "2", "total", "over17"),
         state = str_sub(GEOID, 1, 2)) %>%
  select(-variable, -table) %>%
  filter(age == "total" & !(state == "72")) %>%
  select(-state) %>%
  pivot_wider(names_from = varnum, values_from = value) %>%
  mutate(total_all_all = `01`,
         all_hisp_all = `02`,
         white_alone = `05` + `17`,
         black_alone = `06` + `21`,
         aian_alone  = `07` + `24`,
         asian_alone = `08` + `26`,
         nhpi_alone  = `09` + `27`,
         sor_alone   = `10`,
         multi_race  = `13`+`14`+`15`+`16`+`18`+`19`+`20`+`22`+`23`+`25`+`29`+`30`+`31`+`32`+`33`+`34`+`35`+
           `36`+`37`+`38`+`39`+`40`+`41`+`42`+`43`+`44`+`45`+`46`+`47`+`48`+`50`+`51`+`52`+`53`+`54`+`55`+`56`+
           `57`+`58`+`59`+`60`+`61`+`62`+`63`+`64`+`66`+`67`+`68`+`69`+`70`+`71`+`73`) %>%
  select(-(4:76)) %>%
  mutate(white_sor = round(sor_alone * sor_to_white),
         black_sor = round(sor_alone * sor_to_black),
         aian_sor  = round(sor_alone * sor_to_aian),
         asian_sor = round(sor_alone * sor_to_asian),
         nhpi_sor  = round(sor_alone * sor_to_nhpi),
         multi_sor = round(sor_alone * sor_to_multi),
         difference = white_sor+black_sor+aian_sor+asian_sor+nhpi_sor+multi_sor-sor_alone,
         white_sor = white_sor - difference,
         white_all_all = white_alone + white_sor,
         black_all_all = black_alone + black_sor,
         aian_all_all  = aian_alone + aian_sor,
         asian_all_all = asian_alone + asian_sor,
         nhpi_all_all = nhpi_alone + nhpi_sor,
         twoplus_all_all = multi_race + multi_sor) %>%
  select(GEOID, NAME, total_all_all, all_hisp_all, white_all_all, black_all_all, aian_all_all, 
         asian_all_all, nhpi_all_all, twoplus_all_all)

hugq_wide <- hugq_2020 %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(!(str_sub(GEOID, 1, 2) == "72"))

fips <- fips_codes %>% 
  mutate(fips = paste0(state_code, county_code)) %>%
  select(fips, stabbrev = state, countynm = county)

final_output <- redist_wide_2020 %>%
  full_join(hugq_wide, by = c("GEOID", "NAME")) %>%
  left_join(fips, by = c("GEOID" = "fips")) %>%
  select(-NAME) %>%
  relocate(GEOID, stabbrev, countynm) %>%
  rename(fips = GEOID)
  
write_csv(final_output, "Raw/county_pl.csv")
