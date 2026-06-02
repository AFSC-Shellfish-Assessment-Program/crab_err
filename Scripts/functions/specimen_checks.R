
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
  
  # CRUISE ----
  #**Will probably need to update this at some point to make more adaptive...*
  #* Maybe something to do with STATION_ID lookup to tell whether it should be EBS or NBS??
  #* Nonstandard stations might make this more tricky though...could add to lookup?

    if(!unique(specimen_table$CRUISE) == "202601"){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                select(CRUISE, TABLET) %>%
                distinct()
        
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} incorrect 'CRUISE' entr{?y/ies} detected:\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_tablet <- temp[i,]$TABLET
            temp_cruise <- temp[i,]$CRUISE
            year <- substr(Sys.Date(), 1, 4)
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Cruise"
            errors[error_iter, 2] <- paste0("A 'CRUISE' = ", temp_cruise, " was detected on Tablet '", temp_tablet, "'. 'CRUISE' for the EBS this year should be '", year, "01'.")

          # Print error message
            cat(col_red(paste0("\n- 'CRUISE' = ", temp_cruise, " was detected on Tablet '", temp_tablet, "'. 'CRUISE' for the EBS this year should be '", year, "01'.\n\n")), sep = "")
        }
    }
    
 
  # SPECIES_CODE ----
  # Check regular species
    if(TRUE %in% (!unique(specimen_table$SPECIES_CODE) %in% species_lookup$SPECIES_CODE)){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(!SPECIES_CODE %in% species_lookup$SPECIES_CODE) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} 'SPECIES_CODE' entr{?y/ies} that {?is/are} not in the standard list:\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_CODE
            temp_n <- temp[i,]$N
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen_ID"
            errors[error_iter, 2] <- pluralize("{temp_n} entr{?y/ies}: 'SPECIES_CODE' = ", temp_species, " was detected on Tablet '", temp_tablet, "', and is not part of the standard list. Please verify that this species was observed in the haul.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SPECIES_CODE' = ", temp_species, " {?is/are} detected on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    
    
    # Check special species
      if(TRUE %in% (!unique(specimen_table$SPECIES_CODE) %in% species_lookup$SPECIES_CODE[1:6])){
        
        # Create temporary dataframe
          temp <- specimen_table %>% 
                  filter(!SPECIES_CODE %in% species_lookup$SPECIES_CODE) %>%
                  group_by(SPECIES_CODE, SPECIES_NAME, TABLET) %>%
                  summarise(N = n(), .groups = "drop_last")
        
        # Print message  
          cat(col_red(pluralize("There {?is/are} {nrow(temp)} 'SPECIES_CODE' entr{?y/ies} that {?is/are} more uncommon species:\n")))
        
        # Loop over combos to ID which catch samples have the issue
          for(i in 1:nrow(temp)){
            
            # Set temporary identifiers
              temp_tablet <- temp[i,]$TABLET
              temp_species <- temp[i,]$SPECIES_NAME
              temp_n <- temp[i,]$N
              
            # Add note to Error Report
              error_iter <- nrow(errors) + 1
              errors[error_iter, 1] <- "Specimen_ID"
              errors[error_iter, 2] <- pluralize("{temp_n} entr{?y/ies}: 'SPECIES_NAME' = ", temp_species, " was detected on Tablet '", temp_tablet, "'. Please verify that this species was observed in the haul.")
      
            # Print error message
              cat("\n")
              cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SPECIES_NAME' = ", temp_species, " {?is/are} detected on Tablet '", temp_tablet, "'.\n")), sep = "")
          }
          cat("\n")
      }
      



  # SEX ----
  #**DO we want a flag for herm/unsexed just to verify??*
  #*crabs without SEX?

    if(TRUE %in% (!unique(specimen_table$SEX) %in% c(1:4))){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(!SEX %in% c(1:4)) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} invalid entr{?y/ies} for 'SEX':\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_n <- temp[i,]$N
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- temp[i,]$SEX
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("{temp_n} invalid 'SEX' = ", temp_sex, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SEX' = ", temp_sex, " {?is/are} detected for a '", temp_species, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
        
    


  # SHELL_CONDITION ----
     
  # Check for invalid shell conditions
    if(TRUE %in% (!unique(specimen_table$SHELL_CONDITION) %in% c(0:5))){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(!SHELL_CONDITION %in% c(0:5)) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, SHELL_CONDITION) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} invalid entr{?y/ies} for 'SHELL_CONDITION':\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_n <- temp[i,]$N
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male", 
                                  temp[i,]$SEX == 2 ~ "Female",
                                  temp[i,]$SEX == 3 ~ "Unsexed",
                                  temp[i,]$SEX == 4 ~ "Hermaphrodite")
            temp_sc <- temp[i,]$SHELL_CONDITION
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("{temp_n} invalid 'SHELL_CONDITION' = ", temp_sc, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SHELL_CONDITION' = ", temp_sc, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }



  # EGG_CONDITION ----
  # Check for invalid egg condition
    if(TRUE %in% (!unique(specimen_table %>% filter(SEX == 2) %>% pull(EGG_CONDITION)) %in% c(0:5))){
      
      # Create temporary dataframe
      temp <- specimen_table %>% 
              filter(SEX == 2,
                     !EGG_CONDITION %in% c(0:5)) %>%
              group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, EGG_CONDITION) %>%
              summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} invalid entr{?y/ies} for 'EGG_CONDITION':\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_n <- temp[i,]$N
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male", 
                                  temp[i,]$SEX == 2 ~ "Female",
                                  temp[i,]$SEX == 3 ~ "Unsexed",
                                  temp[i,]$SEX == 4 ~ "Hermaphrodite")
            temp_ec <- temp[i,]$EGG_CONDITION
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("{temp_n} invalid 'EGG_CONDITION' = ", temp_ec, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'EGG_CONDITION' = ", temp_ec, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }




  # CLUTCH_SIZE ----
  # Check for invalid clutch sizes
    if(TRUE %in% (!unique(specimen_table %>% filter(SEX == 2) %>% pull(CLUTCH_SIZE)) %in% c(0:6))){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(SEX == 2,
                       !CLUTCH_SIZE %in% c(0:6)) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, CLUTCH_SIZE) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} invalid entr{?y/ies} for 'CLUTCH_SIZE':\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_n <- temp[i,]$N
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male", 
                                  temp[i,]$SEX == 2 ~ "Female",
                                  temp[i,]$SEX == 3 ~ "Unsexed",
                                  temp[i,]$SEX == 4 ~ "Hermaphrodite")
            temp_clutch <- temp[i,]$CLUTCH_SIZE
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("{temp_n} invalid 'CLUTCH_SIZE' = ", temp_clutch, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'CLUTCH_SIZE' = ", temp_clutch, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    
  
  # EGG_COLOR ----
  # Check for invalid egg color
    if(TRUE %in% (!unique(specimen_table %>% filter(SEX == 2) %>% pull(EGG_COLOR)) %in% c(0, 2:6))){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(SEX == 2,
                       !EGG_COLOR %in% c(0, 2:6)) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, EGG_COLOR) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} invalid entr{?y/ies} for 'EGG_COLOR':\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_n <- temp[i,]$N
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male", 
                                  temp[i,]$SEX == 2 ~ "Female",
                                  temp[i,]$SEX == 3 ~ "Unsexed",
                                  temp[i,]$SEX == 4 ~ "Hermaphrodite")
            temp_ec <- temp[i,]$EGG_COLOR
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("{temp_n} invalid 'EGG_COLOR' = ", temp_ec, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'EGG_COLOR' = ", temp_ec, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    
    
  # CLUTCH CODES (combos) ----
  # - flag dead eggs...assuming they were all dead, but if not, please fix and put in the clutch code for the live portion of eggs
    
    
    # # 8) Any egg, egg condition, or clutch size codes assigned to males? or herm??
    # if(TRUE %in% (unique((specimen_table$SEX == 1 & is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION |
    #                                                       specimen_table$CLUTCH_SIZE) == FALSE))) == TRUE){
    #   print("ERROR: egg, egg condition, or clutch size code assigned to male")
    # }
    # 
    # 
    # # 9) Any questionable egg condition x shell condition combinations for females?"
    # # Checking shell condition = 0 and egg condition = 1
    # if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 0 &
    #                      filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
    #   print("ERROR: female with shell condition = 0 and egg condition = 1")
    # }
    # 
    # # Checking shell condition = 1 and egg condition >1
    # if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 &
    #                      filter(specimen_table, SEX != 1)$EGG_CONDITION > 1)) == TRUE){
    #   print("ERROR: female with shell condition = 1 and egg condition >1")
    # }
    # 
    # # Checking shell condition = 3, 4, or 5 and egg condition = 1
    # if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION %in% c(3:5) &
    #                      filter(specimen_table, SEX != 1)$EGG_CONDITION == 1)) == TRUE){
    #   print("ERROR: female with shell condition 3:5 and egg condition = 1")
    # }
    # 
    # # Checking shell condition = 1 and egg condition >=2
    # if(TRUE %in% (unique(filter(specimen_table, SEX != 1)$SHELL_CONDITION == 1 &
    #                      filter(specimen_table, SEX != 1)$EGG_CONDITION >= 2)) == TRUE){
    #   print("ERROR: female with shell condition = 1 and egg condition >=2")
    # }
    # 
    # 
    # # 10) Any females without egg color, egg condition, or clutch codes?
    # if(TRUE %in% (unique(specimen_table$SEX == 2 & (is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION
    #                                                       | specimen_table$CLUTCH_SIZE) == TRUE))) == TRUE){
    #   print("ERROR: female missing egg color, egg condition, or clutch code")
    # }
    
    
    
    
  # SIZE ----

    # ##**need to incorporate crab species into this!!*
    # # 11) Any missing lengths for RKC?"
    # if(unique(is.na(specimen_table$LENGTH)) == TRUE){
    #   print("ERROR: missing length for RKC")
    # }
    # 
    # ##**need to incorporate crab species into this!!*
    # # 14) Any widths entered for RKC?
    # if(unique(is.na(specimen_table$WIDTH)) == FALSE) {
    #   print("ERROR: width entered for RKC when length is needed")
    # }
    # 
    # 
    # # 12) Any small female crab with a clutch size?"
    # if(unique(filter(specimen_table, SEX != 1)$LENGTH < 65 &
    #           filter(specimen_table, SEX != 1)$CLUTCH_SIZE > 0) == TRUE){
    #   print("ERROR: female <65 with clutch size >0")
    # }
    # 
    # 
    # # 13) Any small crab with old shell condition?"
    # if(TRUE %in% unique(specimen_table$LENGTH < 60 & specimen_table$SHELL_CONDITION > 2)){
    #   print("ERROR: crab < 60 with shell condition >2")
    # }



  ## PLOT SIZE/WEIGHT FOR OUTLIERS?? 0 weights?
  ## flag entries of sizes +_ 2 sd LW regression estimate?




  # DISEASE_CODE ----
  # - flag for rhizocephalans - assuming this was a rotting clutch, was it actually a rhizocephalan?

  # Check for invalid disease code
    if(TRUE %in% (!unique(specimen_table$DISEASE_CODE) %in% c(NA, 1:8))){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(!DISEASE_CODE %in% c(NA, 1:8)) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, DISEASE_CODE) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} invalid entr{?y/ies} for 'DISEASE_CODE':\n")))
      
      # Loop over combos to ID which catch samples have the issue
        for(i in 1:nrow(temp)){
          
          # Set temporary identifiers
            temp_n <- temp[i,]$N
            temp_tablet <- temp[i,]$TABLET
            temp_species <- temp[i,]$SPECIES_NAME
            temp_description <- temp[i,]$SAMPLE_MODIFIER
            temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male", 
                                  temp[i,]$SEX == 2 ~ "Female",
                                  temp[i,]$SEX == 3 ~ "Unsexed",
                                  temp[i,]$SEX == 4 ~ "Hermaphrodite")
            temp_disease <- temp[i,]$DISEASE_CODE
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("{temp_n} invalid 'DISEASE_CODE' = ", temp_disease, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'DISEASE_CODE' = ", temp_disease, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    


    #**Any bitter crab recorded for RKC?*
    # if(unique(is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 |
    #           is.na(specimen_table$DISEASE_CODE) == FALSE & specimen_table$DISEASE_CODE == 2 &
    #           (is.na(specimen_table$DISEASE_DORSAL) == FALSE &
    #            is.na(specimen_table$DISEASE_LEGS) == FALSE &
    #            is.na(specimen_table$DISEASE_VENTRAL) == FALSE)) == TRUE){
    #   print("ERROR: bitter crab recorded for RKC and/or bitter crab recorded with % coverage")
    # }



  
  # SAMPLING_FACTOR ----
  # Sampling factor < 1
    if(nrow(specimen_table %>% filter(SAMPLING_FACTOR < 1)) > 0){
      
      # Set temporary identifiers
        temp <- specimen_table %>% 
                filter(SAMPLING_FACTOR == 1) %>%
                select(SPECIES_NAME, SAMPLE_MODIFIER, SEX, CATCH_SAMPLE_ID, SAMPLING_FACTOR, TABLET) %>%
                distinct()
      
      # Print message 
        cat(col_red(pluralize("There {?is/are} {length(unique(temp$CATCH_SAMPLE_ID))} sample{?s} with a SAMPLING_FACTOR < 1:")))
      
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
            errors[error_iter, 2] <- paste0("A 'SAMPLING_FACTOR' < 1 is detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample with the 'CATCH_SAMPLE_ID' = ", temp_sample, " on Tablet '", temp_tablet, "'.")
        
          # Print error message
            cat(col_red(paste0("\n- '", temp_species, ", ", temp_sex, ", ", temp_description, "' on Tablet '", temp_tablet, "'.")), sep = "")
        }
        cat("\n\n")
    }
    
  
    
  # # Catch/specimen comparison? ----
  # # specimen biometrics matches # in catch summary? (using sampling factor)
  #   
  #   # 22) Does the number of crab and number of entries match between the specimen table and catch summary?
  #   spec_sum <- specimen_table %>%
  #               group_by(CRUISE, VESSEL, HAUL, SPECIES_CODE) %>%
  #               reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR),
  #                              N_ENTRIES = n())
  #   
  #   catch_sum <- catch_summary %>%
  #                select(CRUISE, VESSEL, HAUL, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES)
  #   
  #   if(TRUE %in% is.na(suppressMessages(right_join(spec_sum, catch_sum))) == TRUE){
  #     print("ERROR: number of crab and number of entries do not match between summarized specimen table and catch summary")
  #   }
  # 
  # 
  # 
    
  # END ----
    # cat("\nSpecimen checks have been completed.\n")
    
    return(list(errors = errors))
  
}


# - Add common name to “_db” files? Would need to have lookup table defined in script or on computers 
# - Maybe also check the sizes against the sample descriptor? Eg. 100 CW in a “small” category...or at least check for outliers...



