
# Function for all haul-level file inventory/management

file_checks_haul <- function(haul_info_all, 
                             haul_number,
                             path){
  
  # Set file directory
    in_dir <- paste0(path, "QAQC_queue/")
  
  
  # Create haul ID for output file naming
    haul_id <- str_remove(haul_number, "^0+")
  
  # Subset the haul information to the relevant haul
    haul_info <- haul_info_all %>% 
                 filter(HAUL_NUMBER == haul_number)
  
  
  #**PRINT SOMETHING HERE for commentary?* "starting xx checks for haul xx...."
  # -- as part of main workflow function before calling smaller functions
    cat("\nStarting file checks for Haul ", haul_id, ".\n\n", sep = "")
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## Create error report template ----  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
    error_report <- report_setup(path = path,
                                 vessel = vessel,
                                 leg = leg,
                                 haul_number = haul_number)
  
  #**CREATE ERROR VECTOR/ITERATOR FOR HAUL*
    errors <- data.frame(ERROR_TYPE = character(),
                         ERROR_MESSAGE = character(),
                         stringsAsFactors = FALSE)
  
  
  
  # Start on first haul of ones in folder
  # - read in all files for first haul....
    in_dir <- paste0(path, "QAQC_queue/")
    files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
  
  #**also read in any previous error report - see above* can use file.append() to add on to it w/ the recheck
  #*and maybe note somewhere that there's a recheck/edits made? indicate some sort of repeat section/header??
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
  ## Check station ID inputs/vessel/cruise ----
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##  
  ## - cruise should be fine for now, but if do NBS will need to think about how to incorporate that switch....
  ## - would need a .csv with all "good" inputs and similar ones to change or something?
  ##   - maybe this doesn't matter so much since I can update with real-time haul file....
  ##**- should this go farther down once we've made sure only the relevant files are in there?*
  ##
  ## - definitely need to flag CRUISE...with specific definitions/instructions on what it should be (ie. only change for NBS!! (yyyy02; EBS = yyyy01))
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## ID any note files ----
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  # ID any 'NOTES' files
    if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
      notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
               map_df(~read.csv(paste0(in_dir, .x))) %>%
               select(HAUL_ID, NOTE_TABLE, NOTES)
    
    # Add any 'NOTES' to Error Report
      for(i in 1:nrow(notes)){
        error_iter <- nrow(errors) + 1
        errors[error_iter, 1] <- notes$NOTE_TABLE[i]
        errors[error_iter, 2] <- notes$NOTES[i]
      }
  }
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## ID potential zero-catch station ----
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
  
  # - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
  #   as "error report" and moves to next haul
  #
  # - Also, do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
  #   and verify 0-catch and say please add a note??
  
  
  # # Set 'haul_id'
  #   haul_id <- str_sub(haul_number, 2, 4)
  
  
  # Check for only 2 files in the haul with a 'Note'
    if(length(files) == 2 & exists("notes")){
      
      no_catch <- id_zero_catch(files = files, 
                                errors = errors,
                                vessel = vessel,
                                leg = leg,
                                haul_number = haul_number,
                                path = in_dir)  
      
      # Jump to next haul or break, depending on the function output
        if(no_catch == "next"){
          haul_check <- "next"
        }
        
        if(no_catch == "break"){
          haul_check <- "break"
        }
      
      return(haul_check)
    }
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## Inventory files for the given haul by tablet output type ----
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  #
  # make sure all necessary files are present for the haul -- throw error if not
  # - If just RAW_HAUL is present without a note, add special message to verify 0-catch and say please add a note??
  
    file_name <- c("_CRAB_CATCH_","_CRAB_SPECIMEN_", 
                   "_HAUL_", "_SAMPLE_0", "_SAMPLE_VALUES_", 
                   "_SPECIMEN_0", "_SPECIMEN_BIOMETRICS_")
    file_type <- c("'CRAB_CATCH'", "'CRAB_SPECIMEN'", 
                   "'RAW_HAUL'", "'RAW_SAMPLE'", "'RAW_SAMPLE_VALUES'", 
                   "'RAW_SPECIMEN'", "'RAW_SPECIMEN_BIOMETRICS'")
    
    files_all <- c()
    
  # Loop over each file type
    for(i in 1:length(file_name)){
      
      if(i %in% c(1:2)){
        files_all <- rbind(files_all, 
                           data.frame(FILE = files[grepl(paste0(file_name[i], haul_number), files)]) %>%
                             separate_wider_delim(FILE, 
                                                  delim = regex("(_CRAB_CATCH_|_CRAB_SPECIMEN_)"), 
                                                  names = c("TABLET", "HAUL_NUMBER"),
                                                  too_many = "merge") %>%
                             mutate(DATETIME = str_sub(HAUL_NUMBER, -12, -5),
                                    HAUL_NUMBER = str_sub(HAUL_NUMBER, 1, 4)) %>%
                             mutate(TYPE = file_type[i])) %>%
                     arrange(HAUL_NUMBER, TABLET, DATETIME, TYPE)
      }
      
      if(i > 2){
        files_all <- rbind(files_all, 
                           data.frame(FILE = files[grepl(paste0(haul_number, file_name[i]), files)]) %>%
                             separate_wider_delim(FILE, 
                                                  delim = c("_HAUL"), 
                                                  names = c("TABLET", "HAUL_NUMBER"),
                                                  too_many = "merge") %>%
                             mutate(DATETIME = str_sub(HAUL_NUMBER, -12, -5),
                                    HAUL_NUMBER = str_sub(HAUL_NUMBER, 1, 4)) %>%
                             mutate(TYPE = file_type[i])) %>%
                     arrange(HAUL_NUMBER, TABLET, DATETIME, TYPE)
      }
    }
  
  # find all unique tablet/datetime combos
  # - verify if have each of the types
  # - and then if not, is there a complementary set?
  #   (assign group_IDs) -- what should the "minute" buffer be on this??
  # and THEN report on which ones are missing, if any...
  # and then check for duplicates from a tablet?
    tablet_combos <- files_all %>%
                     group_by(HAUL_NUMBER, TABLET, DATETIME) %>%
                     summarise(N_FILES = n(), .groups = "drop_last") %>%
                     mutate(GROUP_ID = NA) %>%
                     # ID potential files that should be grouped despite having different timestamp...
                     group_by(TABLET) %>%
                     mutate(DIFF = c(0, diff(as.numeric(DATETIME))),
                            GROUP = case_when(DIFF < 5 ~ 1,
                                              TRUE ~ NA)) %>%
                     ungroup()
    
    combo_id <- 1
    
    for(i in 1:nrow(tablet_combos)){
      # If all file types are represented for a tablet/timestamp combo, assign them a common group
        if(!FALSE %in% (file_type %in% unique(files_all %>% 
                                              right_join(., tablet_combos[i,],
                                                         by = join_by(TABLET, HAUL_NUMBER, DATETIME)) %>%
                                              pull(TYPE)))){
          
          tablet_combos <- tablet_combos %>%
                           mutate(GROUP_ID = case_when(TABLET == pull(tablet_combos[i, "TABLET"]) & 
                                                         DATETIME == pull(tablet_combos[i, "DATETIME"]) ~ combo_id,
                                                       TRUE ~ GROUP_ID))
          combo_id <- combo_id + 1
        } 
    }
    
  # For all remaining files, ID files that might be a group but with slightly different timestamps
    if(nrow(tablet_combos %>% 
            filter(is.na(GROUP_ID),
                   !is.na(GROUP))) > 0){
      
      temp_combos <- tablet_combos %>% 
                     filter(is.na(GROUP_ID),
                            !is.na(GROUP))
      
      tablet_combos <- tablet_combos %>%
                       mutate(GROUP_ID = case_when(TABLET %in% temp_combos$TABLET & 
                                                     DATETIME %in% temp_combos$DATETIME ~ combo_id,
                                                   TRUE ~ GROUP_ID))
      combo_id <- combo_id + 1
    }
  
  
  # Assign another group to any remaining files
    if(nrow(tablet_combos %>% 
            filter(is.na(GROUP_ID))) > 0){
      
      temp_combos <- tablet_combos %>% filter(is.na(GROUP_ID))
      
      for(i in 1:nrow(temp_combos)){
        tablet_combos <- tablet_combos %>%
                         mutate(GROUP_ID = case_when(TABLET == temp_combos$TABLET & 
                                                       DATETIME == temp_combos$DATETIME ~ combo_id,
                                                     TRUE ~ GROUP_ID))
        combo_id <- combo_id + 1
      }
    }
  
  
  
  # Once all tablet/timestamp combos have a group assigned, need to ID duplicate sets for a given tablet
    haul_info <- haul_info %>% 
                 right_join(., tablet_combos %>% select(-GROUP, -DIFF, -N_FILES),
                            by = join_by(TABLET, HAUL_NUMBER, DATETIME))
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###    
  ## Check for tablet duplicates ----
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  #
  #**if same tablet/haul_number combo, pick the most recent datetime and archive the "older" files that have presumably been edited*
  # NEED TO TEST IF MULTIPLE VERSIONS FROM MULTIPLE TABLETS...function and error messages
  
    if(nrow(haul_info %>% select(-DATETIME, -GROUP_ID) %>% distinct()) < nrow(haul_info %>% select(-DATETIME) %>% distinct())){
      
      # Print initial message
        cat(col_red(paste0("\nDuplicate files for Haul ", haul_id, " were detected, with different timestamps for the same Tablet ID.\n")))
      
      
      # ID tablet/timestamp that's duplicated, and which to move (regardless of YES/NO selection below)
        duplicate_files <- haul_info %>% 
                           group_by(TABLET) %>% 
                           mutate(N_TIMESTAMP = n()) %>%
                           filter(N_TIMESTAMP > 1)
        
        archive_combos <- duplicate_files %>%
                          group_by(TABLET, GROUP_ID) %>%
                          mutate(MAX_DATETIME = max(as.numeric(DATETIME))) %>%
                          ungroup() %>%
                          filter(!MAX_DATETIME == max(MAX_DATETIME))
        
        
      # Print message: Move duplicate files? (YES/NO)
        move_selection <- menu(c("Yes", "No"), title = "\nWould you like to move the files with the earlier timestamp(s) to the 'Archive' folder?")
        
        # Select 'YES' to move the duplicate files:
          if(move_selection == 1){
              cat("\nYou selected 'Yes'.\n\n")
            
            # Loop over tablet/timestamp combos to ID which files need to be archived
              to_archive <- list()
              for(i in 1:nrow(archive_combos)){
                  temp_tablet <- archive_combos[i,]$TABLET
                  temp_timestamp <- archive_combos[i,]$DATETIME
                  to_archive <- unlist(list.append(to_archive, files[grepl(paste0("^", temp_tablet, ".*.", temp_timestamp, ".csv$"), files)]))
                
                # Add note to Error Report
                  error_iter <- nrow(errors) + 1
                  errors[error_iter, 1] <- "File"
                  errors[error_iter, 2] <- paste0("Duplicate files were detected from tablet '", temp_tablet, "'. '", temp_tablet, "' files for Haul ", haul_id, " with the timestamp '", temp_timestamp, "' were moved to the ", vessel, " ", leg, " 'Archive' folder.")
              }
            
            # Copy duplicate files to the archive folder in sFTP (and USB backups)
            # see if I can combine these commands to do both at once?? 
            # also see if I can do a "cat" return message or something to confirm files have been moved/copied or whatever
              copy_sFTP <- copy_files(files = to_archive,
                                      vessel = vessel,
                                      leg = leg, 
                                      haul_number = haul_number,
                                      file_type = "archive", 
                                      path = path, 
                                      destination = "sftp")
            
              copy_USB <- copy_files(files = to_archive,
                                     vessel = vessel,
                                     leg = leg, 
                                     haul_number = haul_number,
                                     file_type = "archive", 
                                     path = path, 
                                     destination = "backup")
            
            # Move duplicate files to the archive folder  
              to_archive <- move_files(files = to_archive,
                                       vessel = vessel,
                                       leg = leg, 
                                       haul_number = haul_number,
                                       path = path, 
                                       destination = "archive")
            
            
            # Update haul_info to remove archived files from the object
              haul_info <- haul_info %>% 
                           left_join(archive_combos, by = join_by(TABLET, HAUL_NUMBER, DATETIME, GROUP_ID)) %>%
                           filter(is.na(MAX_DATETIME)) %>%
                           select(-N_TIMESTAMP, -MAX_DATETIME)
              
              haul_info_all <- haul_info_all %>% 
                               left_join(archive_combos, by = join_by(TABLET, HAUL_NUMBER, DATETIME)) %>%
                               filter(is.na(MAX_DATETIME)) %>%
                               select(-GROUP_ID, -N_TIMESTAMP, -MAX_DATETIME)
            
            # Print message
              cat("Extra files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n", sep = "")
              cat("These files have also been copied to the sFTP queue and USB backup 'Archive' folders.\n\n", sep = "")
          }
        
        
        # Select 'NO' to NOT move the duplicate files, either STOP or SKIP
          if(move_selection == 2){
              cat("\nYou selected 'No'.\n", sep = "")
              
            # ID which tablets have files that need to be archived
              archive_tablets <- archive_combos %>% 
                                 select(TABLET) %>% 
                                 distinct() %>%
                                 pull()
              
              for(i in 1:length(archive_tablets)){
                  temp_tablet <- archive_tablets[i]
                  
                # Add note to Error Report
                  error_iter <- nrow(errors) + 1
                  errors[error_iter, 1] <- "File"
                  errors[error_iter, 2] <- paste0("Duplicate files were detected from tablet '", temp_tablet, ".")
              }
            
            
            # Option: STOP or SKIP?
              next_selection <- menu(c("Yes", "No"), title = cat(col_red("\nFiles for Haul ", haul_id, " need cleaning up before the error checking for this haul can proceed."), "\n\nWould you like to move on to the next haul in the meantime?\n", sep = ""))
            
            # Select "YES" to SKIP
              if(next_selection == 1){
                  cat("\nYou selected 'Yes'.\n\n")
                  
                #**SAVE ERROR REPORT*
                  cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
                  cat(rep("-", getOption("width")), sep = "")
                  cat("\n\n")
                  
                  haul_checks <- "next"
                  return(haul_checks)
              }
            
            # Select "NO" to STOP
              if(next_selection == 2){ 
                  cat(col_red("\nYou selected 'No'.\n\n"))
                  
                #**SAVE ERROR REPORT*
                  cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n"))
                  cat(col_red(paste0("Please review the tablet files for Haul ", haul_id, " and make sure only the most current versions are in the queue.\n\n", sep = "")))
                  cat(rep("-", getOption("width")), sep = "")
                  cat("\n\n")
                  
                  haul_checks <- "break"
                  return(haul_checks)
              }
         }
    }
  
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###    
  ## Check for complete suite of files ----
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  #
  # NOW, for all remaining files/groups (just for most recent suite for each tablet, if multiple) 
  # run checks and inventory what's incomplete
  
    files_inventory <- files_all %>% 
                       right_join(., haul_info, by = join_by(TABLET, HAUL_NUMBER, DATETIME))
  
  
  
  #**Maybe don't have to iterate through file type here anymore? Can do group instead?*
  #*and can ID which one is missing by filtering/excluding file types....
  # Check if any tablet file type has fewer files than the rest (excluding NOTES files),
  # and if so, flag which files are missing from the QA/QC queue/add to error report 
    files_by_type <- files_inventory %>%
                     group_by(TYPE) %>%
                     summarise(N = n()) %>%
                     full_join(., as_tibble(file_type) %>% 
                                    rename(TYPE = value), 
                               by = join_by(TYPE)) %>%
                     mutate(N = ifelse(is.na(N), 0, N))
    
    if(any(files_by_type$N < max(files_by_type$N))){
      
      # Print message(s) flagging which files are missing for the haul
        cat(col_red("\nA complete suite of files for Haul ", haul_id, " is not present in the queue:\n", sep = ""))
      
      
      # ID missing files by tablet/timestamp combos
        missing_combos <- haul_info %>%
                          full_join(., as_tibble(file_type) %>% 
                                      rename(TYPE = value) %>% 
                                      mutate(HAUL_NUMBER = haul_number),
                                    relationship = "many-to-many", by = join_by(HAUL_NUMBER)) %>%
                          left_join(., files_by_type, by = join_by(TYPE)) %>%
                          filter(N < max(N))
      
      # Add note to Error Report??
      #**Think about this one...should we only add to error report if they want to move on to the next haul??*
      #*I guess if we just disregard and stop, all the errors would go away...which should be fine, unless some files were moved to archive?
      #*Maybe we print report regardless, but I guess only these file errors would be recorded if 
      
      # Loop over missing files to record error message
        for(m in 1:nrow(missing_combos)){
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- "File"
            errors[error_iter, 2] <- paste0("A ", file_type[i], " file is missing from Tablet '", missing_combos$TABLET[m], "' with timestamp '", missing_combos$DATETIME[m], "'")
          
          # Print error message
            cat(col_red(paste0("- A ", file_type[i], " file is missing from Tablet '", missing_combos$TABLET[m], "' with timestamp '", missing_combos$DATETIME[m], "'\n")), sep = "")
        }  
        
        cat(col_red("\nPlease make sure the listed files are in the 'Queue' folder and try again for this haul.\n\n"))
      
      #**something here that allows continuing if a CATCH and SPECIMEN file are present, even if other RAW files are missing?*
      #*maybe not, because the specimen checks will depend on the RAW files to add context to subsample??
      
      # Print menu to select whether or not to move onto the next haul or stop the error checking protocol
        next_selection <- menu(c("Yes", "No"), title = "Would you like to move on to the next haul in the meantime?")
      
      # Select "YES" to SKIP
        if(next_selection == 1){
            cat("\nYou selected 'Yes'.\n\n")
          
          #**SAVE ERROR REPORT* Think about if need 2 error report locations....
          #*one in QAQC as temp, and then a final one that gets moved once the haul is "approved"?
            cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
            
            cat(rep("-", getOption("width")), sep = "")
            
            haul_checks <- "next"
            return(haul_checks)
        }
      
      # Select "NO" to STOP
        if(next_selection == 2){ 
            cat(col_red("\nYou selected 'No'.\n\n"))
          
          #**SAVE ERROR REPORT*
            cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n"))
            cat(col_red(paste0("Please review the tablet files for Haul ", haul_id, " in the 'QA/QC Queue' folder.\n\n", sep = "")))
            
            haul_checks <- "break"
            return(haul_checks)
        }
    }
  
    
    cat("\nA complete suite of files for Haul ", haul_id, " is present in the queue. Proceeding to Catch and Specimen checks.\n\n", sep = "")
  
    return(list(errors = errors))
}


