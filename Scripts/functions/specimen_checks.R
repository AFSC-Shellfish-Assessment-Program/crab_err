
# Function: specimen_checks ----------------------------------------------------
#
# Purpose: 
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# For each haul, check specimen entries for incomplete or implausible biometrics
# Across all specimens:
# - all specimens have a length/width (and correct measurement type for correct species)
# - # specimen biometrics matches # in catch summary? (using sampling factor I think)
# - correct species, sex codes
# - nonsensical clutch codes
# - clutch/no for male/female
# - catch weight vs. estimated weights?
#   - this might be hard, because catch summary is @ species level, and we can't match to subsample for sure....(size/sex categories)
#   - maybe just continue to rely on tablet checks for this one....
# - any specimens above/below 99th %ile size? (or some way to set bounds/ID outliers....)
# - flag any species never seen at a given station before?? (have to make sure we have "-B" stations defined for modernization hauls...)
#**have an iterator for error messages so every time it throws one, those save as object and get complied/printed in final .txt file?*
#**...incorporate this into the 0-catch station check too, and maybe the files # check?*

# For each species:
# - small mature females (especially mature barren RKC/BKC)
# - RKC: shell 3:5 and EC 1


specimen_checks <- function(files_all,
                            haul_info,
                            haul_number,
                            errors){
  
  # cat("\nStarting specimen checks.\n")
  
  # Set file directory
    in_dir <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop/QAQC Queue"), winslash = "/")
  
  ## first join RAW files to get subsample descriptors onto specimens??
  # # unpack raw files...
  #   files_inventory <- files_all %>% 
  #                      right_join(., haul_info)
  #   
  #   catch <- list.files(in_dir, pattern = paste0("_CRAB_CATCH_", haul_number)) %>% 
  #            map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x))))
  #   
  #   specimen <- list.files(in_dir, pattern = paste0("_CRAB_SPECIMEN_", haul_number)) %>% 
  #               map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #               mutate(SPECIMEN_ID = as.integer(str_remove(str_sub(ID, -10, -1), "^0+")))
  #   
  #   
  #   
  #   # raw_haul <- list.files(in_dir, pattern = paste0(haul_number, "_HAUL_")) %>% 
  #   #             map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #   #             select(HAUL_ID, CRUISE_ID, HAUL, STATION, RECORDING_DEVICE, RECORDER)
  #   
  #   raw_sample <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_0")) %>% 
  #                 map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #                 select(-DEVICE_RECORD_ID, -RECORD_TIMESTAMP, -STATUS_TIMESTAMP)
  #   
  #   raw_sample_values <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_VALUES_")) %>% 
  #                        map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #                        select(-DEVICE_RECORD_ID, -RECORD_TIMESTAMP, -STATUS_TIMESTAMP)
  #                        # mutate(TOSSED = ifelse(is.na(COUNT) == FALSE, COUNT, 0)) %>%
  #                        # group_by(HAUL_ID, CATCH_SAMPLE_ID) %>%
  #                        # reframe(TOSSED = sum(TOSSED)) 
  #   
  #   # raw_specimen <- list.files(in_dir, pattern = paste0(haul_number, "_SPECIMEN_0")) %>% 
  #   #                 map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) #%>%
  #   #                 # select(HAUL_ID, SPECIMEN_ID, CATCH_SAMPLE_ID, SPECIES_CODE)
  #   
  #   raw_specimen_bio <- list.files(in_dir, pattern = paste0(haul_number, "_SPECIMEN_BIOMETRICS_")) %>% 
  #                       map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) #%>%
  #                       # right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE),
  #                       #            by = join_by(HAUL_ID, RECORDING_DEVICE))
  # 
  # 
  # 
  # # # Join raw_sample_values and raw_sample to get # tossed per haul, sex, and catch sample id
  # #   samples <- right_join(raw_sample, raw_sample_values,
  # #                         by = join_by(HAUL_ID, CATCH_SAMPLE_ID, RECORDING_DEVICE, RECORDER, DATABASE_STATUS)) %>%
  # #              right_join(., catch %>% select(HAUL, HAUL_ID, RECORDING_DEVICE),
  # #                         by = join_by(HAUL_ID, RECORDING_DEVICE)) #%>%
  # #              # select(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, SPECIES_NAME, SEX, RECORDING_DEVICE)
  # 
  # # Expand specimen biometric table, join to raw_specimen table to get catch sample ID, join with samples file to get 
  # # number tossed
  # specimen_sum <- raw_specimen_bio %>%
  #                 select(HAUL_ID, SPECIMEN_ID, BIOMETRIC_NAME, VALUE, RECORDING_DEVICE) %>%
  #                 pivot_wider(., id_cols = c(HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE), 
  #                             names_from = "BIOMETRIC_NAME", values_from = "VALUE") %>%
  #                 rename(SHELL_CONDITION = CRAB_SHELL_CONDITION, EGG_COLOR = CRAB_EGG_COLOR,
  #                        EGG_CONDITION = CRAB_EGG_CONDITION, CLUTCH_SIZE = CRAB_EGG_CLUTCH_SIZE) %>% 
  #                 select(HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE, SEX, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION) %>%
  #                 right_join(., raw_specimen, by = join_by(HAUL_ID, SPECIMEN_ID)) %>%
  #                 left_join(., raw_sample, by = join_by(HAUL_ID, RECORDING_DEVICE, SEX, CATCH_SAMPLE_ID, SPECIES_CODE))
  #                 # left_join(., raw_sample_values %>% select(-WEIGHT, -WEIGHT_UNITS),
  #                 #           by = join_by(HAUL_ID, RECORDING_DEVICE, CATCH_SAMPLE_ID, RECORDER, DATABASE_STATUS))
  #                 # right_join(samples, ., by = c("HAUL", "HAUL_ID", "SEX", "RECORDING_DEVICE"), 
  #                 #            relationship = "many-to-many")
  # 
  # specimen_dat <- left_join(raw_specimen, raw_sample) %>%
  #                 right_join(specimen, .)
  # 
  # specimen_dat <- left_join(specimen, specimen_sum)
  # 
  # # Calculate sampling factor from specimen summary table, join back with specimen_sum file to 
  # # get specimen information, join with catch file to get vessel and pot #s, join with potlifts
  # # file to get lat/lon, set/haul date and time for each pot (with positive catch)
  # specimen_table <- specimen_sum %>%
  #                   group_by(HAUL, HAUL_ID, CATCH_SAMPLE_ID, SEX, RECORDING_DEVICE) %>%
  #                   reframe(KEPT = n(),
  #                           TOSSED = TOSSED,
  #                           SAMPLING_FACTOR = (KEPT + TOSSED)/KEPT) %>%
  #                   distinct() %>%
  #                   right_join(specimen_sum, by = c("HAUL", "HAUL_ID", "CATCH_SAMPLE_ID", "SEX", "TOSSED",
  #                                                   "RECORDING_DEVICE"),
  #                              multiple = "all") %>%
  #                   right_join(catch,., by = c("HAUL", "HAUL_ID", "SPECIES_CODE", "RECORDING_DEVICE"),
  #                              multiple = "all") %>%
  #                   left_join(specimen) %>%
  #                   distinct() %>%
  #                   rename(SPN = HAUL) %>%
  #                   right_join(potlifts, ., by = c("VESSEL", "SPN"), relationship = "many-to-many") %>%
  #                   filter(c(is.na(LAT_DD) & is.na(LON_DD) & is.na(SPN)) == FALSE) %>% # bad/gear testing hauls will have NA
  #                   select(CRUISE, VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD, DATE_HAUL, TIME_HAUL, SOAK_TIME, DEPTH_F,
  #                          SPECIES_CODE, SEX, LENGTH, WIDTH, SAMPLING_FACTOR, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, 
  #                          CLUTCH_SIZE, WEIGHT, DISEASE_CODE, DISEASE_DORSAL, DISEASE_VENTRAL, DISEASE_LEGS,  
  #                          CHELA_HEIGHT, MERUS_LENGTH, COMMENTS, NOTES.x)
  
  
  
  
  
  # unpack raw files...
    files_inventory <- files_all %>% 
                       right_join(., haul_info)
  
  # catch <- list.files(in_dir, pattern = paste0("_CRAB_CATCH_", haul_number)) %>% 
  #          map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x))))
  # 
  # raw_sample <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_0")) %>% 
  #               map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #               select(-DEVICE_RECORD_ID, -RECORD_TIMESTAMP, -STATUS_TIMESTAMP)
  # 
  # raw_sample_values <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_VALUES_")) %>% 
  #                      map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #                      select(-DEVICE_RECORD_ID, -RECORD_TIMESTAMP, -STATUS_TIMESTAMP)
  
  
  
  
  specimen <- list.files(in_dir, pattern = paste0("_CRAB_SPECIMEN_", haul_number)) %>% 
              map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
              mutate(SPECIMEN_ID = as.integer(str_remove(str_sub(ID, -10, -1), "^0+")))
  
  raw_specimen <- list.files(in_dir, pattern = paste0(haul_number, "_SPECIMEN_0")) %>%
                  map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x))))
  
  # raw_specimen_bio <- list.files(in_dir, pattern = paste0(haul_number, "_SPECIMEN_BIO")) %>%
  #                     map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x))))
  
  raw_sample <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_0")) %>%
                map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
                select(-DEVICE_RECORD_ID, -STATUS_TIMESTAMP, -RECORD_TIMESTAMP)
  
  
  # specimen_sum <- raw_specimen_bio %>%
  #   select(HAUL_ID, SPECIMEN_ID, BIOMETRIC_NAME, VALUE, RECORDING_DEVICE, RECORD_TIMESTAMP) %>%
  #   pivot_wider(., id_cols = c(HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE, RECORD_TIMESTAMP), 
  #               names_from = "BIOMETRIC_NAME", values_from = "VALUE") %>%
  #   rename(SHELL_CONDITION = CRAB_SHELL_CONDITION, EGG_COLOR = CRAB_EGG_COLOR,
  #          EGG_CONDITION = CRAB_EGG_CONDITION, CLUTCH_SIZE = CRAB_EGG_CLUTCH_SIZE) %>% 
  #   select(HAUL_ID, SPECIMEN_ID, RECORDING_DEVICE, RECORD_TIMESTAMP, SEX, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION) %>%
  #   right_join(., raw_specimen, by = join_by(HAUL_ID, SPECIMEN_ID)) %>%
  #   left_join(., raw_sample, by = join_by(HAUL_ID, RECORDING_DEVICE, SEX, CATCH_SAMPLE_ID, SPECIES_CODE))
  
  
  # Join specimen file with raw_specimen to get SPECIMEN_ID, SAMPLE_MODIFIER, RECORD_TIMESTAMP
    specimen_table <- left_join(raw_specimen, raw_sample, 
                                by = join_by(HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, RECORDING_DEVICE, RECORDER, DATABASE_STATUS)) %>%
                      right_join(specimen, ., by = join_by(SPECIES_CODE, SEX, RECORDING_DEVICE, SPECIMEN_ID))
  
  
  # Once dataframes are joined, run specimen checks
  
  
  
  
  
  
  cat("\nSpecimen checks have been completed.\n")
  
  return(list(errors = errors))
  
}

#**Join RAW files in a way that allows for better pinpointing which specimens throw the error!!*
#* (include this info in the error message/report)
# - subsample descriptor
# - specimen biometrics (sex, etc.)
# - specimen number/entry timestamp
#
# - flag for rhizocephalans - assuming this was a rotting clutch, was it actually a rhizocephalan?
# - same for dead eggs...assuming they were all dead, but if not, please fix and put in the clutch code for the live portion of eggs
#
# - Add common name to “_db” files? Would need to have lookup table defined in script or on computers 
# - Maybe also check the sizes against the sample descriptor? Eg. 100 CW in a “small” category…





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# SPECIMEN CHECK FUNCTION ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
specimen_chk <- function(specimen, catch){
  
  
  
  
  
  print("CHECKING SPECIES CODES AND SEX...")
  
  ##**need to incorporate crab species into this!!*
  # 3) Do species codes match RKC code 69322?"
  if(FALSE %in% unique(specimen_table$SPECIES_CODE == 69322)){
    print("ERROR: wrong code entered for RKC")
  }
  
  
  # 4) Are sex codes assigned to either 1 or 2?"
  if(unique(specimen_table$SEX %in% c(1:2)) == FALSE){
    print("ERROR: sex code not 1 or 2")
  }
  
  print("Inventory of catch by cruise, vessel, species code, and sex")
  invent2 <- specimen_table %>%
    dplyr::group_by(VESSEL, CRUISE, SPECIES_CODE, SEX) %>%
    dplyr::reframe(COUNT = sum(SAMPLING_FACTOR), 
                   N = n()) %>%
    as.data.frame()
  print(invent2)
  
  
  print("CHECKING SHELL CONDITION, EGG COLOR, EGG CONDITION, CLUTCH_SIZE...")
  
  # 5) Are egg color codes valid for females?"
  if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$EGG_COLOR %in% c(0, 2:6))) == TRUE){
    print("ERROR: invalid female egg color code (not 0 or 2:6)")
  }
  
  
  # 6) Are egg condition codes valid for females?"
  if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$EGG_CONDITION %in% c(0:5))) == TRUE){
    print("ERROR: invalid female egg condition code (not 0:5)")
  } 
  
  
  # 7) Are clutch size codes valid for females?
  if(FALSE %in% (unique(filter(specimen_table, SEX != 1)$CLUTCH_SIZE %in% c(0:6, 999))) == TRUE){
    print("ERROR: invalid female clutch size code (not 0:6)")
  }
  
  
  # 8) Any egg, egg condition, or clutch size codes assigned to males?
  if(TRUE %in% (unique((specimen_table$SEX == 1 & is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION | 
                                                        specimen_table$CLUTCH_SIZE) == FALSE))) == TRUE){
    print("ERROR: egg, egg condition, or clutch size code assigned to male")
  } 
  
  
  # 9) Any questionable egg condition x shell condition combinations for females?"
  # Checking shell condition = 0 and egg condition = 1
  if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 0 & 
                       filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
    print("ERROR: female with shell condition = 0 and egg condition = 1")
  } 
  
  # Checking shell condition = 1 and egg condition >1
  if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 & 
                       filter(specimen_table, SEX != 1)$EGG_CONDITION > 1)) == TRUE){
    print("ERROR: female with shell condition = 1 and egg condition >1")
  }
  
  # Checking shell condition = 3, 4, or 5 and egg condition = 1
  if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION %in% c(3:5) & 
                       filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
    print("ERROR: female with shell condition 3:5 and egg condition = 1")
  }
  
  # Checking shell condition = 1 and egg condition >=2
  if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 & 
                       filter(specimen_table, SEX != 1)$EGG_CONDITION >= 2)) == TRUE){
    print("ERROR: female with shell condition = 1 and egg condition >=2")
  }
  
  
  # 10) Any females without egg color, egg condition, or clutch codes?
  if(TRUE %in% (unique(specimen_table$SEX == 2 & (is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION 
                                                        | specimen_table$CLUTCH_SIZE) == TRUE))) == TRUE){
    print("ERROR: female missing egg color, egg condition, or clutch code")
  }
  
  
  print("Inventory of shell condition")
  invent3 <- specimen_table %>%
    dplyr::group_by(SPECIES_CODE, SEX, SHELL_CONDITION) %>%
    dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                   N = n()) %>%
    as.data.frame()
  print(invent3)
  
  print("Inventory of female shell condition and egg codes")
  invent4 <- specimen_table %>%
    dplyr::filter(SEX == 2) %>%
    dplyr::group_by(SPECIES_CODE, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, CLUTCH_SIZE) %>%
    dplyr::reframe(COUNT = sum(SAMPLING_FACTOR),
                   N = n()) %>%
    as.data.frame()
  print(invent4)
  
  
  print("CHECKING CRAB SIZES...")
  
  
  ##**need to incorporate crab species into this!!*
  # 11) Any missing lengths for RKC?"
  if(unique(is.na(specimen_table$LENGTH)) == TRUE){
    print("ERROR: missing length for RKC")
  } 
  
  
  # 12) Any small female crab with a clutch size?"
  if(unique(filter(specimen_table, SEX != 1)$LENGTH < 65 & 
            filter(specimen_table, SEX != 1)$CLUTCH_SIZE > 0) == TRUE){
    print("ERROR: female <65 with clutch size >0")
  } 
  
  
  # 13) Any small crab with old shell condition?"
  if(TRUE %in% unique(specimen_table$LENGTH < 60 & specimen_table$SHELL_CONDITION > 2)){
    print("ERROR: crab < 60 with shell condition >2")
  } 
  
  ## PLOT SIZE/WEIGHT FOR OUTLIERS?? 0 weights?
  ## flag entries of sizes +_ 2 sd LW regression estimate?
  
  
  ##**need to incorporate crab species into this!!*
  # 14) Any widths entered for RKC? 
  if(unique(is.na(specimen_table$WIDTH)) == FALSE) {
    print("ERROR: width entered for RKC when length is needed")
  }
  
  
  # 15) Minimum and maximum lengths by sex?
  print("What are the minimum and maximum lengths reported by sex?")
  A15 <- specimen_table %>%
    dplyr::group_by(SPECIES_CODE, SEX) %>%
    dplyr::reframe(MIN_LENGTH = min(LENGTH),
                   MAX_LENGTH = max(LENGTH)) %>%
    as.data.frame()
  print(A15)
  
  
  print("CHECKING DISEASE CODES...")
  
  # 16) Any black mat recorded but without % coverage entry?
  if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE &
            specimen_table$DISEASE_CODE == 1 & (is.na(specimen_table$DISEASE_DORSAL) == TRUE &
                                                is.na(specimen_table$DISEASE_LEGS) == TRUE &
                                                is.na(specimen_table$DISEASE_VENTRAL) == TRUE)) == TRUE){
    print("ERROR: black mat recorded without % coverage")
  } 
  
  
  # 17) Any bitter crab recorded for RKC and/or any bitter crab recorded with entries in % coverage?
  if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 |
            is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 & 
            (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
             is.na(specimen_table$DISEASE_LEGS) == FALSE &
             is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
    print("ERROR: bitter crab recorded for RKC and/or bitter crab recorded with % coverage")
  } 
  
  
  # 18) Any disease code not recorded but entries in % coverage?
  if(unique(is.na(specimen_table$DISEASE_CODE) == TRUE & (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
                                                          is.na(specimen_table$DISEASE_LEGS) == FALSE &
                                                          is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
    print("ERROR: disease code not recorded but % cover entered")
  } 
  
  
  # 19) Any disease codes >9? 
  if(unique(is.na(specimen_table$DISEASE_CODE) == "FALSE" & specimen_table$DISEASE_CODE > 9) == TRUE){
    print("ERROR: disease code >9")
  }
  
  
  print("CHECKING SAMPLING FACTOR...")
  
  # 20) What is the maximum sampling factor by by sex?
  print("What is the maximum sampling factor by sex?")
  A20 <- specimen_table %>%
    dplyr::group_by(SPECIES_CODE, SEX) %>%
    dplyr::reframe(MIN_SAMPLING_FACTOR = min(SAMPLING_FACTOR),
                   MAX_SAMPLING_FACTOR= max(SAMPLING_FACTOR)) %>%
    as.data.frame() 
  print(A20)
  
  
  # 21) Any sampling factors < 1?"
  if(unique(specimen_table$SAMPLING_FACTOR < 1) == TRUE){
    print("ERROR: minimum sampling factor < 1")
  } 
  
  
  print("CHECKING CRUISE, HAUL, AND STATION IDs...")
  xx <- cpue %>%
    dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD) %>%
    distinct() %>%
    as.data.frame()
  
  yy <- potlifts %>%
    dplyr::select(VESSEL, SPN, POT_ID, BUOY, LAT_DD, LON_DD) %>%
    distinct()
  
  if(TRUE %in% is.na(suppressMessages(right_join(xx, yy, keep = TRUE))) == TRUE){
    print("ERROR: POT: pot, station, and/or buoy IDs do not match between potlifts table and pot cpue table")
  }
  
  
  
  
  print("COMPARING SPECIMEN TABLE WITH CATCH SUMMARY...")
  
  # 22) Does the number of crab and number of entries match between the specimen table and catch summary?
  # if(method == "POT"){
  #   spec_sum <- specimen_table %>%
  #               dplyr::group_by(CRUISE, VESSEL, POT_ID, SPECIES_CODE) %>%
  #               dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
  #                              N_ENTRIES = n())
  #   
  #   catch_sum <- catch_summary %>%
  #                dplyr::select(CRUISE, VESSEL, POT_ID, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
  #   
  #   if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
  #     print("ERROR: POT: number of crab and number of entries do not 
  #              match between summarized specimen table and catch summary")
  #   }
  # } 
  
  # if(method == "TRAWL"){
  spec_sum <- specimen_table %>%
    dplyr::group_by(CRUISE, VESSEL, HAUL, SPECIES_CODE) %>%
    dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
                   N_ENTRIES = n())
  
  catch_sum <- catch_summary %>%
    dplyr::select(CRUISE, VESSEL, HAUL, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
  
  if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
    print("ERROR: TRAWL: number of crab and number of entries do not 
                     match between summarized specimen table and catch summary")
  } 
  # }
}    













