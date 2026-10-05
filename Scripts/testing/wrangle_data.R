
temp_check <- data.frame(FILE = list.files("c:/Users/Shannon.Hennessey/Desktop/RawData/", pattern = ".csv", recursive = TRUE)) %>%
              separate_wider_delim(FILE, "/", names = c("VESSEL", "LEG", "FOLDER", "FILE")) %>%
              mutate(TABLET = str_sub(FILE, 1, 6),
                     DATETIME = str_sub(FILE, -12, -5)) %>%
              group_by(VESSEL, LEG, TABLET, DATETIME) %>%
              summarise(N_FILES = n())

current_year <- 2026
path <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/RawData/_for_QAQC/")

# Read in all 'CATCH_db.csv' and 'SPECIMEN_db.csv' files from FTPd data

specimen <- list.files(path, pattern = "SPECIMEN_db") %>%
            map_df(~read.csv(paste0(path, .x)))

catch <- list.files(path, pattern = "CATCH_db") %>%
         map_df(~read.csv(paste0(path, .x))) %>%
         group_by(VESSEL, CRUISE, HAUL, COMMON_NAME, SPECIES_CODE) %>%
         # combine weights and catch numbers by species (if 2 tablets were used for the haul)
         summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                   NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE),
                   .groups = "drop_last") %>%
         # summarize catch numbers by species from specimen table and update catch numbers
         # if there were rounding discrepancies from using 2 tablets
         # (ie. tablet rounds to whole numbers but if the catch was split, 0.4 and 0.4 round down, but 0.8 rounds up)
         left_join(., specimen %>%
                        group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
                        summarise(CATCH = sum(SAMPLING_FACTOR)),
                   by = join_by(CRUISE, VESSEL, HAUL, SPECIES_CODE)) %>%
         mutate(NUMBER_CRAB = ifelse(CATCH > NUMBER_CRAB, round(CATCH), round(NUMBER_CRAB))) %>% 
         # will the ^^ specimen #s always be larger??
         # and really should only be off by 1, right?? hmm think about more
         select(-CATCH) %>%
         write.table(., paste0(path, "CATCH_db.csv"),
                     row.names = FALSE, na = "", sep = ",")




specimen <- read.csv("Y:/KOD_Survey/EBS Shelf/2026/RawData/AKK/Leg4/SPECIMEN_db.csv") 

write.table(specimen, "Y:/KOD_Survey/EBS Shelf/2026/RawData/_for_QAQC/AKK_4_SPECIMEN_db.csv",
            row.names = FALSE, na = "", sep = ",")


#-----------------------------
current_year <- 2026
path <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/RawData/")

haul <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/haul_atsea_mod.csv") %>%
        filter(PERFORMANCE >= 0)


specimen_akk <- read.csv("Y:/KOD_Survey/EBS Shelf/2026/RawData/AKK/Leg4/SPECIMEN_db.csv") %>%
                filter(HAUL >= 500) 
write.table(specimen_akk, "Y:/KOD_Survey/EBS Shelf/2026/RawData/_for_QAQC/AKK_4mod_SPECIMEN_db.csv", row.names = FALSE, na = "", sep = ",")

specimen_nwex <- read.csv("Y:/KOD_Survey/EBS Shelf/2026/RawData/NWEx/Leg4/SPECIMEN_db.csv") 
write.table(specimen_nwex, "Y:/KOD_Survey/EBS Shelf/2026/RawData/_for_QAQC/NWEx_4mod_SPECIMEN_db.csv", row.names = FALSE, na = "", sep = ",")

catch_akk <- read.csv("Y:/KOD_Survey/EBS Shelf/2026/RawData/AKK/Leg4/CATCH_db.csv") %>%
             filter(HAUL >= 500) %>%
             group_by(VESSEL, CRUISE, HAUL, COMMON_NAME, SPECIES_CODE) %>%
             summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                       NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE),
                       .groups = "drop_last") %>%
                       left_join(., specimen_akk %>%
                                   group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
                                   summarise(CATCH = sum(SAMPLING_FACTOR)),
                                 by = join_by(CRUISE, VESSEL, HAUL, SPECIES_CODE)) %>%
                       mutate(NUMBER_CRAB = ifelse(CATCH > NUMBER_CRAB, round(CATCH), round(NUMBER_CRAB))) %>% 
                       select(-CATCH)
write.table(catch_akk, "Y:/KOD_Survey/EBS Shelf/2026/RawData/_for_QAQC/AKK_4mod_CATCH_db.csv", row.names = FALSE, na = "", sep = ",")

catch_nwex <- read.csv("Y:/KOD_Survey/EBS Shelf/2026/RawData/NWEx/Leg4/CATCH_db.csv") %>%
              group_by(VESSEL, CRUISE, HAUL, COMMON_NAME, SPECIES_CODE) %>%
              summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                        NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE),
                        .groups = "drop_last") %>%
              left_join(., specimen_nwex %>%
                          group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
                          summarise(CATCH = sum(SAMPLING_FACTOR)),
                        by = join_by(CRUISE, VESSEL, HAUL, SPECIES_CODE)) %>%
              mutate(NUMBER_CRAB = ifelse(CATCH > NUMBER_CRAB, round(CATCH), round(NUMBER_CRAB))) %>% 
              select(-CATCH)
write.table(catch_nwex, "Y:/KOD_Survey/EBS Shelf/2026/RawData/_for_QAQC/NWEx_4mod_CATCH_db.csv", row.names = FALSE, na = "", sep = ",")






