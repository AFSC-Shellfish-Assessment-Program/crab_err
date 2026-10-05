
## Set path for FTP'd data folder on Kodiak server 
current_year <- 2025
path <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/RawData/")

## Read in all specimen tables from FTP'd data 
dat <- list.files(path, pattern = "CRAB_SPECIMEN", recursive = TRUE) %>% 
       purrr::map_df(~read.csv(paste0(path, .x))) %>%
       mutate(LEG = case_when((VESSEL == 162 & HAUL <= 70) ~ 1,
                              (VESSEL == 162 & HAUL > 70) ~ 2,
                              (VESSEL == 134 & HAUL <= 71) ~ 1,
                              (VESSEL == 134 & HAUL > 71) ~ 2,
                              TRUE ~ NA))


sex_summary <- dat %>%
               group_by(VESSEL, LEG, HAUL, STATION, SPECIES_CODE, SAMPLING_FACTOR, SEX) %>%
               summarise(N_MEASURED = n()) %>%
               arrange(-N_MEASURED)

haul_summary <- dat %>%
                group_by(VESSEL, LEG, HAUL, STATION, SPECIES_CODE, SEX) %>%
                summarise(N_MEASURED = n()) %>%
                arrange(-N_MEASURED)

catch_summary <- dat %>%
                 group_by(VESSEL, LEG, HAUL, STATION, SPECIES_CODE) %>%
                 summarise(N_MEASURED = n()) %>%
                 arrange(-N_MEASURED)


## Read in catch data
dat_catch <- list.files(path, pattern = "CRAB_CATCH", recursive = TRUE) %>% 
             purrr::map_df(~read.csv(paste0(path, .x))) %>%
             select(VESSEL, HAUL, COMMON_NAME, SPECIES_CODE, NUMBER_CRAB) %>%
             group_by(VESSEL, HAUL, COMMON_NAME, SPECIES_CODE) %>%
             summarise(NUMBER_CRAB = sum(NUMBER_CRAB)) %>%
             full_join(., catch_summary) %>%
             mutate(PERC_MEASURED = round((N_MEASURED/NUMBER_CRAB)*100, digits = 0))
             
