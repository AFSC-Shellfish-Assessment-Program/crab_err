
# Function: specimen_checks ----------------------------------------------------
#
# Purpose: 
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

#**Is there some way I could streamline this??*
#*Make a list of criteria, metadata, messages and then just loop through for most??



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
# - flag any species never seen at a given station before?? (have to make sure we have "-B" stations defined/corrected for modernization hauls...)

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
                    map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
                    select(-RECORDER)
    
    raw_sample <- list.files(in_dir, pattern = paste0(haul_number, "_SAMPLE_0")) %>%
                  map_df(~suppressWarnings(read.csv(paste0(in_dir, "/", .x)))) %>%
                  select(-DEVICE_RECORD_ID, -STATUS_TIMESTAMP, -RECORD_TIMESTAMP)
  
  
  # Join specimen file with raw_specimen to get SPECIMEN_ID, SAMPLE_MODIFIER, RECORD_TIMESTAMP
    specimen_table <- left_join(raw_specimen, raw_sample, 
                                by = join_by(HAUL_ID, CATCH_SAMPLE_ID, SPECIES_CODE, RECORDING_DEVICE, DATABASE_STATUS)) %>%
                      right_join(specimen, ., by = join_by(SPECIES_CODE, SEX, RECORDING_DEVICE, SPECIMEN_ID))
  
  
  # Once dataframes are joined, run specimen checks
  
    
    
  # STATION_ID ----
    
    # station_check <- specimen_table %>%
    #                  # remove any corner stations
    #                  filter(str_detect(STATION, "-")) %>% 
    #                  select(CRUISE, VESSEL, HAUL, TABLET, STATION) %>%
    #                  # # remove any 15 minute tows (i.e. X-X-B notation)  
    #                  # mutate(standard_station = ifelse(str_count(STATION, "-") == 2, F, T)) %>%
    #                  # filter(standard_station == TRUE) %>%
    #                  # make sure all stations have leading zeros
    #                  mutate(STATION_ID = STATION) %>% # make duplicate column to match against
    #                  (separate(STATION_ID, sep = "-", into = c("col", "row", "mod"))) %>%
    #                  mutate(row = str_pad(row, width = 2, pad = "0")) %>% # make sure all names have leading zeros
    #                  unite("STATION_ID", col:row, sep = "-") %>%
    #                  distinct()
    
    
  # CRUISE ----
  #**Will probably need to update this at some point to make more adaptive...*
  #* Maybe something to do with STATION_ID lookup to tell whether it should be EBS or NBS??
  #* Nonstandard stations might make this more tricky though...could add to lookup?

    if(nrow(specimen_table %>% 
            filter(!CRUISE == "202601")) > 0){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(!CRUISE == "202601") %>%
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
    if(nrow(specimen_table %>% 
            filter(!SPECIES_CODE %in% species_lookup$SPECIES_CODE)) > 0){
      
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
    
    
    # # Check special species
    #   if(nrow(specimen_table %>% 
    #           filter(!SPECIES_CODE %in% species_lookup$SPECIES_CODE[1:6])) > 0){
    #     
    #     # Create temporary dataframe
    #       temp <- specimen_table %>% 
    #               filter(!SPECIES_CODE %in% species_lookup$SPECIES_CODE[1:6]) %>%
    #               group_by(SPECIES_CODE, SPECIES_NAME, TABLET) %>%
    #               summarise(N = n(), .groups = "drop_last")
    #     
    #     # Print message  
    #       cat(col_red(pluralize("There {?is/are} {nrow(temp)} 'SPECIES_CODE' entr{?y/ies} that {?is/are} more uncommon species:\n")))
    #     
    #     # Loop over combos to ID which catch samples have the issue
    #       for(i in 1:nrow(temp)){
    #         
    #         # Set temporary identifiers
    #           temp_tablet <- temp[i,]$TABLET
    #           temp_species <- temp[i,]$SPECIES_NAME
    #           temp_n <- temp[i,]$N
    #           
    #         # Add note to Error Report
    #           error_iter <- nrow(errors) + 1
    #           errors[error_iter, 1] <- "Specimen_ID"
    #           errors[error_iter, 2] <- pluralize("{temp_n} entr{?y/ies}: 'SPECIES_NAME' = ", temp_species, " was detected on Tablet '", temp_tablet, "'. Please verify that this species was observed in the haul.")
    #   
    #         # Print error message
    #           cat("\n")
    #           cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SPECIES_NAME' = ", temp_species, " {?is/are} detected on Tablet '", temp_tablet, "'.\n")), sep = "")
    #       }
    #       cat("\n")
    #   }
      



  # SEX ----
  #**DO we want a flag for herm/unsexed just to verify??*

  # Check for invalid SEX entries
    if(nrow(specimen_table %>% 
            filter(!SEX %in% c(1:4))) > 0){
      
      # Create temporary dataframe
      temp_spec <- specimen_table %>% 
                   filter(!SEX %in% c(1:4))
      
      temp <- temp_spec %>% 
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
          
          # Filter specimens
            specimen_flags <- temp_spec %>% 
                              filter(TABLET == temp_tablet,
                                     SPECIES_NAME == temp_species,
                                     SAMPLE_MODIFIER == temp_description,
                                     SEX == temp_sex) %>%
                              arrange(SPECIMEN_ID)
            
            for(s in 1:nrow(specimen_flags)){
              
              # Set SPECIMEN_ID
                temp_specimenID <- specimen_flags[s,]$SPECIMEN_ID
              
              # Add note to Error Report
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "Specimen"
                errors[error_iter, 2] <- paste0("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has an invalid 'SEX' = ", temp_sex, ".")
            }
            
            
          # # Add note to Error Report
          #   error_iter <- nrow(errors) + 1
          #   errors[error_iter, 1] <- "Specimen"
          #   errors[error_iter, 2] <- pluralize("{temp_n} invalid 'SEX' = ", temp_sex, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SEX' = ", temp_sex, " {?is/are} detected for a '", temp_species, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
        
    


  # SHELL_CONDITION ----
     
  # Check for invalid shell conditions
    if(nrow(specimen_table %>% 
            filter(!SHELL_CONDITION %in% c(0:5))) > 0){
      
      # Create temporary dataframe
        temp_spec <- specimen_table %>% 
                     filter(!SHELL_CONDITION %in% c(0:5)) 
        
        temp <- temp_spec %>% 
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
            
          # Filter specimens
            specimen_flags <- temp_spec %>% 
                              mutate(SEX_TEXT = case_when(SEX == 1 ~ "Male", 
                                                          SEX == 2 ~ "Female",
                                                          SEX == 3 ~ "Unsexed",
                                                          SEX == 4 ~ "Hermaphrodite")) %>%
                              filter(TABLET == temp_tablet,
                                     SPECIES_NAME == temp_species,
                                     SAMPLE_MODIFIER == temp_description,
                                     SHELL_CONDITION == temp_sc,
                                     SEX_TEXT == temp_sex) %>%
                              arrange(SPECIMEN_ID)
            
            for(s in 1:nrow(specimen_flags)){
              
              # Set SPECIMEN_ID
                temp_specimenID <- specimen_flags[s,]$SPECIMEN_ID
              
              # Add note to Error Report
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "Specimen"
                errors[error_iter, 2] <- paste0("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has an invalid 'SHELL_CONDITION' = ", temp_sc, ".")
            }
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'SHELL_CONDITION' = ", temp_sc, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }

    
    # Check for tiny crab with old shell for Chionoecetes crabs
    # width < 30 and shell_condition > 2
    #
    # # 13) Any small crab with old shell condition?"
    # if(TRUE %in% unique(specimen_table$LENGTH < 60 & specimen_table$SHELL_CONDITION > 2)){
    #   print("ERROR: crab < 60 with shell condition >2")
    # }
    
    

  # EGG_CONDITION ----
  # Check for invalid egg condition
    if(nrow(specimen_table %>% 
            filter(SEX == 2,
                   !EGG_CONDITION %in% c(0:5))) > 0){
      
      # Create temporary dataframe
        temp_spec <- specimen_table %>% 
                     filter(SEX == 2,
                            !EGG_CONDITION %in% c(0:5))
        
        temp <- temp_spec %>% 
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
            
          # Filter specimens
            specimen_flags <- temp_spec %>% 
                              mutate(SEX_TEXT = case_when(SEX == 1 ~ "Male", 
                                                          SEX == 2 ~ "Female",
                                                          SEX == 3 ~ "Unsexed",
                                                          SEX == 4 ~ "Hermaphrodite")) %>%
                              filter(TABLET == temp_tablet,
                                     SPECIES_NAME == temp_species,
                                     SAMPLE_MODIFIER == temp_description,
                                     EGG_CONDITION == temp_ec,
                                     SEX_TEXT == temp_sex) %>%
                              arrange(SPECIMEN_ID)
            
            for(s in 1:nrow(specimen_flags)){
              
              # Set SPECIMEN_ID
                temp_specimenID <- specimen_flags[s,]$SPECIMEN_ID
              
              # Add note to Error Report
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "Specimen"
                errors[error_iter, 2] <- paste0("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has an invalid 'EGG_CONDITION' = ", temp_ec, ".")
            }
            
            
          # # Add note to Error Report
          #   error_iter <- nrow(errors) + 1
          #   errors[error_iter, 1] <- "Specimen"
          #   errors[error_iter, 2] <- pluralize("{temp_n} invalid 'EGG_CONDITION' = ", temp_ec, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
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
        temp_spec <- specimen_table %>% 
                     filter(SEX == 2,
                            !CLUTCH_SIZE %in% c(0:6))
        
        temp <- temp_spec %>% 
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
          
          # Filter specimens
            specimen_flags <- temp_spec %>% 
                              mutate(SEX_TEXT = case_when(SEX == 1 ~ "Male", 
                                                          SEX == 2 ~ "Female",
                                                          SEX == 3 ~ "Unsexed",
                                                          SEX == 4 ~ "Hermaphrodite")) %>%
                              filter(TABLET == temp_tablet,
                                     SPECIES_NAME == temp_species,
                                     SAMPLE_MODIFIER == temp_description,
                                     CLUTCH_SIZE == temp_clutch,
                                     SEX_TEXT == temp_sex) %>%
                              arrange(SPECIMEN_ID)
            
            for(s in 1:nrow(specimen_flags)){
              
              # Set SPECIMEN_ID
                temp_specimenID <- specimen_flags[s,]$SPECIMEN_ID
              
              # Add note to Error Report
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "Specimen"
                errors[error_iter, 2] <- paste0("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has an invalid 'CLUTCH_SIZE' = ", temp_clutch, ".")
            }
            
          # # Add note to Error Report
          #   error_iter <- nrow(errors) + 1
          #   errors[error_iter, 1] <- "Specimen"
          #   errors[error_iter, 2] <- pluralize("{temp_n} invalid 'CLUTCH_SIZE' = ", temp_clutch, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
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
        temp_spec <- specimen_table %>% 
                     filter(SEX == 2,
                            !EGG_COLOR %in% c(0, 2:6))  
        
        temp <- temp_spec %>% 
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
          
          # Filter specimens
            specimen_flags <- temp_spec %>% 
                              mutate(SEX_TEXT = case_when(SEX == 1 ~ "Male", 
                                                          SEX == 2 ~ "Female",
                                                          SEX == 3 ~ "Unsexed",
                                                          SEX == 4 ~ "Hermaphrodite")) %>%
                              filter(TABLET == temp_tablet,
                                     SPECIES_NAME == temp_species,
                                     SAMPLE_MODIFIER == temp_description,
                                     EGG_COLOR == temp_ec,
                                     SEX_TEXT == temp_sex) %>%
                              arrange(SPECIMEN_ID)
            
            for(s in 1:nrow(specimen_flags)){
              
              # Set SPECIMEN_ID
                temp_specimenID <- specimen_flags[s,]$SPECIMEN_ID
              
              # Add note to Error Report
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "Specimen"
                errors[error_iter, 2] <- paste0("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has an invalid 'EGG_COLOR' = ", temp_ec, ".")
            }
            
          # # Add note to Error Report
          #   error_iter <- nrow(errors) + 1
          #   errors[error_iter, 1] <- "Specimen"
          #   errors[error_iter, 2] <- pluralize("{temp_n} invalid 'EGG_COLOR' = ", temp_ec, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'EGG_COLOR' = ", temp_ec, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    
    
    
    
  # CLUTCH CODES (combos) ----
    #**GIVE THIS SPECIMEN_ID*
  # - flag dead eggs...assuming they were all dead, but if not, please fix and put in the clutch code for the live portion of eggs
    
  
     
    # Check for questionable egg codes for female king crab
    # shell_condition = 0 and egg_condition = 1
    # or shell_condition = 1 and egg_condition > 1
    # or shell_condition in (3,4,5) and egg_condition = 1
    # or shell_condition = 1 and egg_condition >= 2
    #
    # Check for questionable egg codes for Tanner and snow crab
    # shell_condition = 1 and egg_condition >= 2
    # or shell_condition = 0 and egg_condition = 1 
    
    
    
    # # 8) Any egg, egg condition, or clutch size codes assigned to males? or herm??
    # if(TRUE %in% (unique((specimen_table$SEX == 1 & is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION |
    #                                                       specimen_table$CLUTCH_SIZE) == FALSE))) == TRUE){
    #   print("ERROR: egg, egg condition, or clutch size code assigned to male")
    # }


    # # 10) Any females without egg color, egg condition, or clutch codes?
    # if(TRUE %in% (unique(specimen_table$SEX == 2 & (is.na(specimen_table$EGG_COLOR | specimen_table$EGG_CONDITION
    #                                                       | specimen_table$CLUTCH_SIZE) == TRUE))) == TRUE){
    #   print("ERROR: female missing egg color, egg condition, or clutch code")
    # }
    
    
  
      
    
  # SIZE ----
    #**GIVE THIS SPECIMEN_ID*
    # ##**need to incorporate crab species into this!!*
    # # 11) Any missing sizes?
    # if(unique(is.na(specimen_table$LENGTH)) == TRUE){
    #   print("ERROR: missing length for RKC")
    # }

    # # 14) Any widths entered for KC/lengths for Chiono?
    # if(unique(is.na(specimen_table$WIDTH)) == FALSE) {
    #   print("ERROR: width entered for RKC when length is needed")
    # }

    # # 12) Any small female crab with a clutch size?"
    # if(unique(filter(specimen_table, SEX != 1)$LENGTH < 65 &
    #           filter(specimen_table, SEX != 1)$CLUTCH_SIZE > 0) == TRUE){
    #   print("ERROR: female <65 with clutch size >0")
    # }


    # Check the sizes against the sample descriptor? Eg. 100 CW in a “small” category...or at least check for outliers...
    
    
    # Check for small female crab with a clutch size
    #**What are the cutoffs for this??*
    # # RKC < 65, BKC < 65?, Snow/Hybrid..., Tanner...
    # #**GIVE THIS SPECIMEN_ID*
    #   if(nrow(specimen_table %>% 
    #           filter(SPECIES_NAME == "red king crab", 
    #                  SEX == 2, 
    #                  EGG_COLOR == 0,
    #                  EGG_CONDITION == 0,
    #                  CLUTCH_SIZE == 1,
    #                  LENGTH < 90))){
    #     
    #     # Create temporary dataframe
    #     temp <- specimen_table %>% 
    #       filter(SPECIES_NAME == "red king crab", 
    #              SEX == 2, 
    #              EGG_COLOR == 0,
    #              EGG_CONDITION == 0,
    #              CLUTCH_SIZE == 1,
    #              LENGTH < 90) 
    #     
    #     # Print message  
    #     cat(col_red(pluralize("There {?is/are} {nrow(temp)} 'mature barren' RKC female{?s} with a 'LENGTH' < 90mm:\n")))
    #     
    #     # Loop over combos to ID which specimens have the issue
    #     for(i in 1:nrow(temp)){
    #       
    #       # Set temporary identifiers
    #       temp_tablet <- temp[i,]$TABLET
    #       temp_species <- temp[i,]$SPECIES_NAME
    #       temp_description <- temp[i,]$SAMPLE_MODIFIER
    #       temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male",
    #                             temp[i,]$SEX == 2 ~ "Female",
    #                             temp[i,]$SEX == 3 ~ "Unsexed",
    #                             temp[i,]$SEX == 4 ~ "Hermaphrodite")
    #       temp_length <- temp[i,]$LENGTH
    #       temp_specimenID <- temp[i,]$SPECIMEN_ID
    #       
    #       # Add note to Error Report
    #       error_iter <- nrow(errors) + 1
    #       errors[error_iter, 1] <- "Specimen"
    #       errors[error_iter, 2] <- pluralize("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' was identified as 'mature, no eggs', but has a 'LENGTH' < 90mm. Please verify this entry.")
    #       
    #       # Print error message
    #       cat("\n")
    #       cat(col_red(pluralize("- 'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
    #     }
    #     cat("\n")
    #   }
    # 
    # 


    # Check for RKC small "mature barren" females (< 90mm)
      if(nrow(specimen_table %>% 
              filter(SPECIES_NAME == "red king crab", 
                     SEX == 2, 
                     EGG_COLOR == 0,
                     EGG_CONDITION == 0,
                     CLUTCH_SIZE == 1,
                     LENGTH < 90)) > 0){
        
        # Create temporary dataframe
          temp <- specimen_table %>% 
                  filter(SPECIES_NAME == "red king crab", 
                         SEX == 2, 
                         EGG_COLOR == 0,
                         EGG_CONDITION == 0,
                         CLUTCH_SIZE == 1,
                         LENGTH < 90) 
        
        # Print message  
          cat(col_red(pluralize("There {?is/are} {nrow(temp)} 'mature barren' RKC female{?s} with a 'LENGTH' < 90mm:\n")))
        
        # Loop over combos to ID which specimens have the issue
          for(i in 1:nrow(temp)){
            
            # Set temporary identifiers
              temp_tablet <- temp[i,]$TABLET
              temp_species <- temp[i,]$SPECIES_NAME
              temp_description <- temp[i,]$SAMPLE_MODIFIER
              temp_sex <- case_when(temp[i,]$SEX == 1 ~ "Male",
                                    temp[i,]$SEX == 2 ~ "Female",
                                    temp[i,]$SEX == 3 ~ "Unsexed",
                                    temp[i,]$SEX == 4 ~ "Hermaphrodite")
              temp_length <- temp[i,]$LENGTH
              temp_specimenID <- temp[i,]$SPECIMEN_ID
            
            # Add note to Error Report
              error_iter <- nrow(errors) + 1
              errors[error_iter, 1] <- "Specimen"
              errors[error_iter, 2] <- pluralize("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' was identified as 'mature, no eggs', but has a 'LENGTH' < 90mm. Please verify this entry.")
            
            # Print error message
              cat("\n")
              cat(col_red(pluralize("- 'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
          }
          cat("\n")
      }
    
    
    

  ## PLOT SIZE/WEIGHT FOR OUTLIERS?? 0 weights?
  ## flag entries of sizes +- 2 sd LW regression estimate?

    

  # CHELA_HEIGHT ----
    
    # Check the minimum and maximum chela_height height by species_code
    # SQL> select species_code, length, width, min(chela_height),max(chela_height)
    # from &xcrab
    # where cruise like '&xyear%' and chela_height is not null
    # group by species_code,length,width
    # order by species_code,length,width;

    
    
  # Check CHELA_HEIGHT entries for females
    if(nrow(specimen_table %>% 
            filter(!is.na(CHELA_HEIGHT),
                   !SEX == 1)) > 0){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(!is.na(CHELA_HEIGHT),
                       !SEX == 1) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, DISEASE_CODE) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} non-male specimen{?s} with 'CHELA_HEIGHT' recorded:\n")))
      
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
            temp_specimenID <- temp[i,]$SPECIMEN_ID
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has a 'CHELA_HEIGHT' entry.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- 'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    
    
    
  # DISEASE_CODE ----
  # - flag for rhizocephalans - assuming this was a rotting clutch, was it actually a rhizocephalan?
    if(4 %in% unique(specimen_table$DISEASE_CODE)){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(DISEASE_CODE == 4) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, DISEASE_CODE) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} specimen{?s} with Rhizocephalan barnacles ('DISEASE_CODE' = 4):\n")))
      
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
            temp_specimenID <- temp[i,]$SPECIMEN_ID
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' was identified as having Rhizocephalan barnacles. Was this a flag for a rotting clutch?")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- 'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    
    
  # Check for invalid disease code
    if(TRUE %in% (!unique(specimen_table$DISEASE_CODE) %in% c(NA, 1:8))){
      
      # Create temporary dataframe
        temp_spec <- specimen_table %>% 
                     filter(!DISEASE_CODE %in% c(NA, 1:8))
        
        temp <- temp_spec %>% 
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
          
          # Filter specimens
            specimen_flags <- temp_spec %>% 
                              mutate(SEX_TEXT = case_when(SEX == 1 ~ "Male", 
                                                          SEX == 2 ~ "Female",
                                                          SEX == 3 ~ "Unsexed",
                                                          SEX == 4 ~ "Hermaphrodite")) %>%
                              filter(TABLET == temp_tablet,
                                     SPECIES_NAME == temp_species,
                                     SAMPLE_MODIFIER == temp_description,
                                     DISEASE_CODE == temp_disease,
                                     SEX_TEXT == temp_sex) %>%
                              arrange(SPECIMEN_ID)
            
            for(s in 1:nrow(specimen_flags)){
              
              # Set SPECIMEN_ID
                temp_specimenID <- specimen_flags[s,]$SPECIMEN_ID
              
              # Add note to Error Report
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "Specimen"
                errors[error_iter, 2] <- paste0("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' has an invalid 'DISEASE_CODE' = ", temp_disease, ".")
            }
            
          # # Add note to Error Report
          #   error_iter <- nrow(errors) + 1
          #   errors[error_iter, 1] <- "Specimen"
          #   errors[error_iter, 2] <- pluralize("{temp_n} invalid 'DISEASE_CODE' = ", temp_disease, " entr{?y/ies} {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- {temp_n} entr{?y/ies} of 'DISEASE_CODE' = ", temp_disease, " {?is/are} detected for a '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }
    


  # Check for bitter crab recorded for RKC
    if(nrow(specimen_table %>% 
            filter(SPECIES_NAME == "red king crab", 
                   DISEASE_CODE == 2)) > 0){
      
      # Create temporary dataframe
        temp <- specimen_table %>% 
                filter(SPECIES_NAME == "red king crab",
                       DISEASE_CODE == 2) %>%
                group_by(SPECIES_CODE, SPECIES_NAME, TABLET, SEX, SAMPLE_MODIFIER, DISEASE_CODE) %>%
                summarise(N = n(), .groups = "drop_last")
      
      # Print message  
        cat(col_red(pluralize("There {?is/are} {nrow(temp)} RKC specimen{?s} with Bitter crab disease ('DISEASE_CODE' = 2) recorded:\n")))
      
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
            temp_specimenID <- temp[i,]$SPECIMEN_ID
          
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "Specimen"
            errors[error_iter, 2] <- pluralize("'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "' was identified as having Bitter crab disease.")
          
          # Print error message
            cat("\n")
            cat(col_red(pluralize("- 'SPECIMEN_ID' ", temp_specimenID, " in the '", temp_species, ", ", temp_sex, ", ", temp_description, "' sample on Tablet '", temp_tablet, "'.\n")), sep = "")
        }
        cat("\n")
    }



  
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
    return(list(errors = errors))
  
}
