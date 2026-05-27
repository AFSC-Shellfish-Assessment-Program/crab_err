
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
#   - Need to compile raws to match to subsample (size/sex categories)!!
# - any specimens above/below 99th %ile size? (or some way to set bounds/ID outliers....)
# - flag any species never seen at a given station before?? (have to make sure we have "-B" stations defined for modernization hauls...)

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
  
  # ## first join RAW files to get subsample descriptors onto specimens??
  # # unpack raw files...
  #   files_inventory <- files_all %>%
  #                      right_join(., haul_info)
  # 
  #   catch <- list.files(in_dir, pattern = paste0("_CRAB_CATCH_", haul_number)) %>%
  #            map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x))))
  # 
  #   raw_sample <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_0")) %>%
  #                 map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #                 select(-DEVICE_RECORD_ID, -RECORD_TIMESTAMP, -STATUS_TIMESTAMP)
  # 
  #   raw_sample_values <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_VALUES_")) %>%
  #                        map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
  #                        select(-DEVICE_RECORD_ID, -RECORD_TIMESTAMP, -STATUS_TIMESTAMP)

    specimen <- list.files(in_dir, pattern = paste0("_CRAB_SPECIMEN_", haul_number), full.names = TRUE) %>% 
                # funky thing here to extract/attach TABLET from file name
                map_df(.f = function(FILE){
                  data <- read.csv(FILE) %>%
                          mutate(TABLET = str_split(str_split(FILE, pattern = "/")[[1]][6], pattern = "_CRAB_SPECIMEN_")[[1]][1])
                  return(data)}) %>%
                mutate(SPECIMEN_ID = as.integer(str_remove(str_sub(ID, -10, -1), "^0+")))

    
    raw_specimen <- list.files(in_dir, pattern = paste0(haul_number, "_SPECIMEN_0")) %>%
                    map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x))))
    
    raw_sample <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_0")) %>%
                  map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
                  select(-DEVICE_RECORD_ID, -STATUS_TIMESTAMP, -RECORD_TIMESTAMP)
  
  
  # Join specimen file with raw_specimen to get SPECIMEN_ID, SAMPLE_MODIFIER, RECORD_TIMESTAMP
    specimen_table <- left_join(raw_specimen, raw_sample, 
                                by = join_by(HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, RECORDING_DEVICE, RECORDER, DATABASE_STATUS)) %>%
                      right_join(specimen, ., by = join_by(SPECIES_CODE, SEX, RECORDING_DEVICE, SPECIMEN_ID))
  
  
  # Once dataframes are joined, run specimen checks
  
 
  # SPECIES_CODE ----
    print("CHECKING SPECIES CODES AND SEX...")
    
    ##**need to incorporate crab species into this!!*
    # 3) Do species codes match RKC code 69322?"
    if(FALSE %in% unique(specimen_table$SPECIES_CODE == 69322)){
      print("ERROR: wrong code entered for RKC")
    }
  
  # SEX ----
    
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
    
    
  # SHELL_CONDITION ----
    
    
  # CLUTCH CODES ----
  # - flag dead eggs...assuming they were all dead, but if not, please fix and put in the clutch code for the live portion of eggs
    
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
    
    
    
  # SIZE ----
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
    
    
    
  # DISEASE_CODE ----
  # - flag for rhizocephalans - assuming this was a rotting clutch, was it actually a rhizocephalan?
    
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
    
    
    # # 18) Any disease code not recorded but entries in % coverage?
    # if(unique(is.na(specimen_table$DISEASE_CODE) == TRUE & (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
    #                                                         is.na(specimen_table$DISEASE_LEGS) == FALSE &
    #                                                         is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
    #   print("ERROR: disease code not recorded but % cover entered")
    # } 
    
    
    # 19) Any disease codes >9? 
    if(unique(is.na(specimen_table$DISEASE_CODE) == "FALSE" & specimen_table$DISEASE_CODE > 9) == TRUE){
      print("ERROR: disease code >9")
    }
    
  
  # SAMPLING_FACTOR ----
  # Sampling factor < 1
    if(nrow(specimen_table %>% filter(SAMPLING_FACTOR == 1)) > 0){
      
        temp <- specimen_table %>% 
                filter(SAMPLING_FACTOR == 1) %>%
                select(SPECIES_NAME, SAMPLE_MODIFIER, SEX, CATCH_SAMPLE_ID, SAMPLING_FACTOR, TABLET) %>%
                distinct()
        
        cat(col_red(pluralize("There {?was/were} {length(unique(temp$CATCH_SAMPLE_ID))} sample{?s} with a SAMPLING_FACTOR < 1:")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
            temp_tablet <- temp[i,]$TABLET
            temp_sample <- temp[i,]$CATCH_SAMPLE_ID
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male", 
                                  temp[i,]$SEX == 2 ~ "Female",
                                  temp[i,]$SEX == 3 ~ "Unsexed",
                                  temp[i,]$SEX == 4 ~ "Hermaphrodite")
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- paste0("A SAMPLING_FACTOR < 1 was detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample with the CATCH_SAMPLE_ID = ", temp_sample, " on Tablet '", temp_tablet, "'.")
        
          # Print error message
            cat(col_red(paste0("- '", temp_species, ", ", temp_sex, ", ", temp_description, "' on Tablet '", temp_tablet, "'\n")), sep = "")
        }
    }
    
  
    
  # Catch/specimen comparison? ----
  # specimen biometrics matches # in catch summary? (using sampling factor)
    
    # 22) Does the number of crab and number of entries match between the specimen table and catch summary?
    spec_sum <- specimen_table %>%
                group_by(CRUISE, VESSEL, HAUL, SPECIES_CODE) %>%
                reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
                               N_ENTRIES = n())
    
    catch_sum <- catch_summary %>%
                 select(CRUISE, VESSEL, HAUL, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
    
    if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
      print("ERROR: number of crab and number of entries do not match between summarized specimen table and catch summary")
    }
  
  
  
    
  # END ----
    cat("\nSpecimen checks have been completed.\n")
    
    return(list(errors = errors))
  
}


# - Add common name to “_db” files? Would need to have lookup table defined in script or on computers 
# - Maybe also check the sizes against the sample descriptor? Eg. 100 CW in a “small” category...or at least check for outliers...



