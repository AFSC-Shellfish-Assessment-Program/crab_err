# ON-BOARD ERROR CHECKING WORKFLOW ---------------------------------------------
#
# Purpose:
#
# Outline:
#
#
### ### ### ### ### ### ###
#**UNDER DEVELOPMENT!!!* #
### ### ### ### ### ### ###
#
# NOTES: ----
# - need to make a way to detect and bind catch/specimen from 2 tablets, 
#   but also make sure they're from 2 different tablets, not the same one re-extracted
# 
# - make into functions, and for each haul, print a preliminary error report (ie.
#   "no errors" or "this is a 0-catch station, correct?") with a Y/N option, and then
#   ask if you want to move the files to the "clean" folder....that way can say N
#   for ones that you need to go back to but clear the ones that are good
#   - and then once fixed, re-error check? and have that append to the separate 
#     error report for that haul (I think we need separate ones so there can be an
#     organized followup that says good to go...can we get it to read old errors
#     and have the user automatically say yes this is ok??)

# - look into txt file interfacing w/ R....or maybe .csv??

#**- when extracting files, do they all have the *exact* same timestamp, or can they vary???
#    - need to check this when running through all hauls as a test!
#    - also need to BOLD in protocol to please re-extract ALL files if making changes....


#**Write up a step-by-step narrative in code comments!*
#*also set up/push the .Rproj so it opens the right stuff for everyone??
# and have a copy of the protocol in the repo readme


## Check 0-catch stations and correct haul-station combos needs to be done separate
## but can verify vessel/cruise #s in on-board script

#**Compare total catch vs. estimated weights by species...?*
#* would need to do something with joining the raw files to get the subsample groups/weights
#* with specimens -- can expand weights from there and ID lg in "small" etc.

# How to append .txt files??
#**What's the best method for disseminating code fixes if we need some?? GitHub - public repo?*


# Move clean files to permanent folder
# Re-generate CATCH and SPECIMEN master files

# Lines to also move to FTP folder? need to think about the mechanics of this more....

#**Send out a poll to crab team to gauge preferences for QA/QC script functionality and outputs?*
# - function (with lots of messages/informative errors and TESTED) vs. script?
# - error report by haul or combined? (I prefer individual ones/combined by repeat checks...)
# - would any other haul-level summaries be helpful?



#**MINI FUNCTIONS TO WRITE:* 
# - Output error report
# - Save error? Might help eliminate some lines...
# - Move files to clean/archive?
# - Run intro checks...(ie. all files in queue)
# - Run file checks (for given haul)
#   - ID duplicates as separate nested?
# - Run specimen checks
# ....what else...
#
#
#**Things for main function to need:*
# *make sure to incorporate error checks/messages to validate function inputs!
# - vessel
# - leg
# - recorder (object class)
# - check that paths are valid? (but will be pre-specified...unless good reason for folks to be able to change)
# 
# Loop over hauls? Or just do each haul and use map (or something) to apply to each in queue?




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# START HERE #### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# # Load libraries ----
#   library(tidyverse)
#   library(purrr)
#   library(cli) # for colored text
#   library(stringi) # read .txt files
#   library(data.table) # read .txt files??
#   library(rlist) # append to list

source("./Scripts/setup.R")

# Set recorder, vessel/leg - user needs to do ----
# (set directories - will use vessel/leg)
  vessel <- "AKK" #"NWEx"
  leg <- "Leg1" #"Leg2"
  recorder <- "Shannon Hennessey"


##**make a check within the function to make sure vessel/leg inputs are correct??* --> check_inputs()
##*would need to do this before setting the paths because they won't work if the leg/vessel is slightly off
  path <- "C:/Users/Shannon.Hennessey/Desktop/onboard error checks/"
  # in_dir <- paste0(path, "QAQC_queue/")
  # clean_dir <- paste0(path, vessel, "/", leg, "/") # clean files, archive, error reports
  # 
  # sftp_dir <- paste0(path, "to_sFTP/", vessel, "/", leg, "/") # files for sFTP - clean tablet files, error reports, archive??
  # backup_dir <- paste0("D:/", vessel, "/", leg, "/") # thumb drive backup, only clean files/error reports
  # #**add directions for how to go in and update a directory if need be??*

  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Start global file checks ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
## compile temporary/placeholder "haul" file? ----
# - ID 0-catch stations
# - where do I want to draw this from?? just the files in the queue? or all hauls so far?


##**Might need to do a special file check here first!* 
##**inventory all files in queue and make sure a timestamp doesn't change with extraction variation....*
##  - if tablet and haul number are same, and list # of files that matches isn't complete, 
##     see if there's a complementary timestamp for that haul that makes the set....
##  - might add complication to moving files, but we'll see? I think at end, can just move by haul# and don't need timestamp

  
  haul_info_all <- file_checks_global(path)
  


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Start haul-specific file checks ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
#**# Loop over hauls here....START OF ERROR CHECKING**
#**I think I want to keep this as a loop rather than using `map`...seems to give more outs w/ `break`?*
  # for(h in 1:length(hauls)){
  #   haul_number <- hauls[h] # haul_number <- unique(haul_info$HAUL_NUMBER)[h]
  # }

# Identify number of haul to be checked
  haul_number <- unique(haul_info_all$HAUL_NUMBER)[6] #**CHANGE THIS FOR FINAL*

# Create haul ID for output file naming
  haul_id <- str_remove(haul_number, "^0+")

# Subset the haul information to the relevant haul
  haul_info <- haul_info_all %>% 
               filter(HAUL_NUMBER == haul_number) #%>%
               # add_row(TABLET = "SAM 78", HAUL_NUMBER = "0143", DATETIME = "06301740") ##Added TEMPORARY/FAKE copies to detect duplicates!!

  
#**PRINT SOMETHING HERE for commentary?* "starting xx checks for haul xx...."
# -- as part of main workflow function before calling smaller functions


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


# ##**READ IN/CHECK FOR ERROR REPORT HERE* --> FUNCTION
# ##  - Make note of if one exists, then have a "RE-CHECK" indicated in the file??
# ##  - MAKE NOTE IN PROTOCOL --> please don't rename things!!!!
# ##  - Will also need to collect some other haul/time info for report output...come up with header format
# # report_file <- list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul113"))
#   report_file <- list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))
# 
# 
#   if(length(report_file) == 1){
#     # read in existing error report file
#       report <- read.delim(paste0(clean_dir, "_error_reports/", report_file)) 
#     
#     #...then what...create "recheck" header for appending...
#     
#   } else if(length(report_file) == 0){
#     
#     #**DO SOMETHING HERE...*
#     # if it doesn't exist already, create new file....
#     # need some standard things here for header etc. 
#     # How do delim line breaks, blank lines, etc? can we preserve those with output/input through R?
#     
#   } else{
#     # THROW ERROR - multiple files for same haul?? go back and look/reconcile....
#     #**do I want to provide an option to move on to the next haul in the meantime??*
#       cat(col_red("Multiple error report files were detected for Haul ", haul_id, ". \n   To correct this, please:\n"))
#       cat(col_red("      (1) Review the files in the ", vessel, " ", leg, " 'Error Reports' folder;\n"))
#       cat(col_red("      (2) Reconcile any differences and combine into one comprehensive error report file;\n"))
#       cat(col_red("      (3) Delete the extra file(s), and try again.\n\n"))
#       break
#   }

  



# Start on first haul of ones in folder
# - read in all files for first haul....
  in_dir <- paste0(path, "QAQC_queue/")
  files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
#**also read in any previous error report - see above* can use file.append() to add on to it w/ the recheck
#*and maybe note somewhere that there's a recheck/edits made? indicate some sort of repeat section/header??


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
## Check station ID inputs/vessel/cruise ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## - cruise should be fine for now, but if do NBS will need to think about how to incorporate that switch....
## - would need a .csv with all "good" inputs and similar ones to change or something?
##   - maybe this doesn't matter so much since I can update with real-time haul file....
##**- should this go farther down once we've made sure only the relevant files are in there?*
##
## - definitely need to flag CRUISE...with specific definitions/instructions on what it should be (ie. only change for NBS!! (yyyy02; EBS = yyyy01))
  
  
  # ID any 'Notes' files
    if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
      notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
               map_df(~read.csv(paste0(in_dir, .x))) %>%
               select(HAUL_ID, NOTE_TABLE, NOTES)
      
      # - add any 'NOTES' to error report....
        for(i in 1:nrow(notes)){
          # Add note to Error Report
            error_iter <- nrow(errors) + 1
            errors[error_iter, 1] <- notes$NOTE_TABLE[i]
            errors[error_iter, 2] <- notes$NOTES[i]
        }
    }
  
  
  # Set 'haul_id'
    haul_id <- str_sub(haul_number, 2, 4)
  
  
  # Check for only 2 files in the haul with a 'Note'
    if(length(files) == 2 & exists("notes")){
      
      
      # - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
      #   as "error report" and moves to next haul
      #
      # - Also, do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
      #  and verify 0-catch and say please add a note??
      
        no_catch <- id_zero_catch(files = files, 
                                  errors = errors,
                                  vessel = vessel,
                                  leg = leg,
                                  haul_number = haul_number,
                                  path = in_dir)  
      
      # Jump to next haul or break, depending on the function output
        if(no_catch == "next"){
          next
        }
        
        if(no_catch == "break"){
          break
        }
    }
  
  

  
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ## ID any note files ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#   if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
#       notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
#                map_df(~read.csv(paste0(in_dir, .x))) %>%
#                select(HAUL_ID, NOTE_TABLE, NOTES)
#   }
#   
# ##**this part might not really be necessary?* I don't think we need to print notes, only looking for 0-catches....
# # parse into specimen notes and xx notes by NOTE_TABLE for error report??
# # catch_sample_notes - bin subsample
# # specimen_notes - rotten eggs, etc. 
# # haul_notes - 0-catch station
# 
# 
# 
# 
# 

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ## ID potential zero-catch station ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# #
# #**MAKE THIS A FUNCTION TO SOURCE*
# #
# # - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
# #   as "error report" and moves to next haul
# #
# # - Also, do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
# #  and verify 0-catch and say please add a note??
#   
#   if(length(files) == 2 & exists("notes")){
#       no_catch_selection <- menu(c("Yes", "No"), title = paste0("\nWas Haul ", haul_id, " a zero-catch station?"))
#     
#       if(no_catch_selection == 1){
#           move_selection <- menu(c("Yes", "No"), title = "\nYou selected 'Yes' - would you like to move on to the next haul?")
#           
#           if(move_selection == 1){
#               cat("\nYou selected 'Yes'.\n\n", sep = "")
#               
#               ## OUTPUT ERROR REPORT HERE indicating 0-catch station -- make function for outputting error report and call here!!!!
#               error_iter <- nrow(errors) + 1
#               errors[error_iter, 1] <- "No Catch"
#               errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
#               
#               ## MOVE FILES
#               
#               cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n", sep = "") 
#               cat("Starting error checks for the next haul.\n\n", sep = "")
#               
#               next
#           }
#           
#           if(move_selection == 2){
#               cat("\nYou selected 'No'.\n\n", sep = "")
#               
#               ## OUTPUT ERROR REPORT HERE indicating 0-catch station
#               error_iter <- nrow(errors) + 1
#               errors[error_iter, 1] <- "No Catch"
#               errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
#               
#               ## MOVE FILES
#               
#               cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n", sep = "")
#               cat(col_red("Stopping the error checking protocol.\n\n", sep = ""))
#               
#               break
#           }
#       } else if(no_catch_selection == 2){
#           cat("You selected 'No'.\n\n")
#           cat(col_red("No 'CATCH' or 'SPECIMEN' files for Haul ", haul_id, " are present in the queue.\n", sep = ""))
#           cat(col_red("Please make sure those files are in the 'QAQC_queue' folder and try again for this haul.\n\n"))
#           
#          #**PRINT ERROR REPORT*
#           error_iter <- nrow(errors) + 1
#           errors[error_iter, 1] <- "File"
#           errors[error_iter, 2] <- paste0("No 'CATCH' or 'SPECIMEN' files are present in the QAQC queue")
#           
#           next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
#           
#           if(next_selection == 1){
#               cat("You selected 'Yes'.\n")
#               cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
#               next
#           }
#           
#           if(next_selection == 2){ #**PRINT ERROR REPORT?*
#               cat(col_red("You selected 'No'.\n"))
#               cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
#               break
#           }
#     } # do I need a final "else" here? Selection not recognized, please run these lines again?
#   }


  
  
  
  
## make sure all necessary files are present for the haul -- throw error if not
# (0-catch check will have moved on to next haul....so this should only need to flag incomplete hauls that aren't 0-catch)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Inventory files for the given haul by tablet output type ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  file_name <- c("_CRAB_CATCH_","_CRAB_SPECIMEN_", 
                 "_HAUL_", "_SAMPLE_0", "_SAMPLE_VALUES_", 
                 "_SPECIMEN_0", "_SPECIMEN_BIOMETRICS_")
  file_type <- c("'CRAB_CATCH'", "'CRAB_SPECIMEN'", 
                 "'RAW_HAUL'", "'RAW_SAMPLE'", "'RAW_SAMPLE_VALUES'", 
                 "'RAW_SPECIMEN'", "'RAW_SPECIMEN_BIOMETRICS'")
  
  files_all <- c()
  
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
    # if all file types are represented for a tablet/timestamp combo...
    if(!FALSE %in% (file_type %in% unique(files_all %>% 
                                            right_join(., tablet_combos[i,],
                                                       by = join_by(TABLET, HAUL_NUMBER, DATETIME)) %>%
                                            pull(TYPE)))){
      
      tablet_combos <- tablet_combos %>%
                       mutate(GROUP_ID = case_when(TABLET == pull(tablet_combos[i, "TABLET"]) & DATETIME == pull(tablet_combos[i, "DATETIME"]) ~ combo_id,
                                                   TRUE ~ GROUP_ID))
      combo_id <- combo_id + 1
    } 
  }

  # ok so now from remaining ones....  
  # tablet_combos2 <- tablet_combos %>% 
  #                   filter(is.na(GROUP_ID))
  # files_all2 <- files_all %>% 
  #               right_join(., tablet_combos2)
  
  # if there's some that might be a group....
  if(nrow(tablet_combos %>% 
          filter(is.na(GROUP_ID),
                 !is.na(GROUP))) > 0){
    
    temp_combos <- tablet_combos %>% 
                   filter(is.na(GROUP_ID),
                          !is.na(GROUP))
    
    # check to make sure there aren't any duplicate types??
    # if(!FALSE %in% (file_type %in% unique(files_all2 %>% 
    #                                       right_join(., tablet_combos2 %>% filter(is.na(GROUP)),
    #                                                  by = join_by(TABLET, HAUL_NUMBER, DATETIME)) %>%
    #                                       pull(TYPE)))){
      
      tablet_combos <- tablet_combos %>%
                       mutate(GROUP_ID = case_when(TABLET %in% temp_combos$TABLET & DATETIME %in% temp_combos$DATETIME ~ combo_id,
                                                   TRUE ~ GROUP_ID))
      combo_id <- combo_id + 1
    # } 
  }
  
  
  # give group to any that are left...
  if(nrow(tablet_combos %>% 
          filter(is.na(GROUP_ID))) > 0){
    
    temp_combos <- tablet_combos %>% filter(is.na(GROUP_ID))
    
    for(i in 1:nrow(temp_combos)){
      tablet_combos <- tablet_combos %>%
                       mutate(GROUP_ID = case_when(TABLET == temp_combos$TABLET & DATETIME == temp_combos$DATETIME ~ combo_id,
                                                   TRUE ~ GROUP_ID))
      combo_id <- combo_id + 1
    }
  }
  
  
  
## OK SO, now everyone has groups.....now ID duplicate sets....
  haul_info <- haul_info %>% 
               right_join(., tablet_combos %>% select(-GROUP, -DIFF, -N_FILES))
  

  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###    
## check tablet duplicates ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#**if same tablet/haul_number combo, pick the most recent datetime and archive the "older" files that have presumably been edited*
# NEED TO TEST IF MULTIPLE VERSIONS FROM MULTIPLE TABLETS...function and error messages
  if(nrow(haul_info %>% select(-DATETIME, -GROUP_ID) %>% distinct()) < nrow(haul_info %>% select(-DATETIME) %>% distinct())){
    
  # Print initial message
    cat(col_red(paste0("\nDuplicate files for Haul ", haul_id, " were detected, with different timestamps for the same Tablet ID.\n")))
    
    
  # ID tablet/timestamp that's duplicated, and which to move (regardless of YES/NO selection below)
    duplicate_files <- haul_info %>% 
                       # select(-DATETIME) %>%
                       # distinct() %>%
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
      copy1 <- copy_files(files = to_archive,
                          vessel = vessel,
                          leg = leg, 
                          haul_number = haul_number,
                          file_type = "archive", 
                          path = path, 
                          destination = "sftp")
      
      #**OK so might have to make each vessel/leg/archive subfolder individually as commands before it'll make the file type folders....
      copy2 <- copy_files(files = to_archive,
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
      haul_info <- haul_info %>% #filter(!(TABLET %in% archive_tablets & DATETIME %in% archive_timestamps))
                   left_join(archive_combos, by = join_by(TABLET, HAUL_NUMBER, DATETIME, GROUP_ID)) %>%
                   filter(is.na(MAX_DATETIME)) %>%
                   select(-N_TIMESTAMP, -MAX_DATETIME)
      haul_info_all <- haul_info_all %>% #filter(!(TABLET %in% archive_tablets & DATETIME %in% archive_timestamps))
                       left_join(archive_combos, by = join_by(TABLET, HAUL_NUMBER, DATETIME)) %>%
                       filter(is.na(MAX_DATETIME)) %>%
                       select(-GROUP_ID, -N_TIMESTAMP, -MAX_DATETIME)
      
    # Print message
      cat("Extra files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
    }
    
    
  # Select 'NO' to NOT move the duplicate files, either STOP or SKIP
    if(move_selection == 2){
      cat("\nYou selected 'No'.\n\n", sep = "")
      
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
      next_selection <- menu(c("Yes", "No"), title = cat(col_red("\nFiles for Haul ", haul_id, " need cleaning up before the error checking for this haul can proceed."), "\nWould you like to move on to the next haul in the meantime?", sep = ""))
      
    # Select "YES" to SKIP
      if(next_selection == 1){
        cat("You selected 'Yes'.\n")
        
      #**SAVE ERROR REPORT*
        cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
        next
      }
      
    # Select "NO" to STOP
      if(next_selection == 2){ 
        cat(col_red("You selected 'No'.\n"))
        
      #**SAVE ERROR REPORT*
        cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
        cat(col_red(paste0("Please review the tablet files for Haul ", haul_id, " and make sure only the most current versions are in the queue.\n\n", sep = "")))
        break
      }
    }
  }
  
  
  
  
  
# NOW, for all remaining files/groups (just for most recent suite for each tablet, if multiple) 
# run checks and inventory what's incomplete
  
  files_inventory <- files_all %>% 
                     right_join(., haul_info, by = join_by(TABLET, HAUL_NUMBER, DATETIME))
  
  # files_inventory <- list(files[grepl(paste0("_CRAB_CATCH_", haul_number), files)],
  #                         files[grepl(paste0("_CRAB_SPECIMEN_", haul_number), files)],
  #                         files[grepl(paste0(haul_number, "_HAUL_"), files)],
  #                         files[grepl(paste0(haul_number, "_SAMPLE_0"), files)],
  #                         files[grepl(paste0(haul_number, "_SAMPLE_VALUES_"), files)],
  #                         files[grepl(paste0(haul_number, "_SPECIMEN_0"), files)],
  #                         files[grepl(paste0(haul_number, "_SPECIMEN_BIOMETRICS_"), files)])
  # 
  #   
  # haul_info_test <- data.frame(FILE = list.files(in_dir, pattern = ".csv", recursive = TRUE)) %>%
  #                   separate_wider_delim(FILE, 
  #                                        delim = regex("(_HAUL|_CRAB_)"), 
  #                                        names = c("TABLET", "HAUL_NUMBER"),
  #                                        too_many = "merge") %>%
  #                   mutate(DATETIME = str_sub(HAUL_NUMBER, -12, -5),
  #                          HAUL_NUMBER = str_sub(HAUL_NUMBER, 1, 4)) %>%
  #                   distinct() %>%
  #                   arrange(HAUL_NUMBER, TABLET, DATETIME)
  
  
  
#**Maybe don't have to iterate through file type here anymore? Can do group instead?*
#*and can ID which one is missing by filtering/excluding file types....
# Check if any tablet file type has fewer files than the rest (excluding NOTES files),
# and if so, flag which files are missing from the QA/QC queue/add to error report 
  files_by_type <- files_inventory %>%
                   group_by(TYPE) %>%
                   summarise(N = n()) %>%
                   full_join(., as_tibble(file_type) %>% rename(TYPE = value), by = join_by(TYPE)) %>%
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
      cat("You selected 'Yes'.\n")
      
    #**SAVE ERROR REPORT* Think about if need 2 error report locations....
      #*one in QAQC as temp, and then a final one that gets moved once the haul is "approved"?
      cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
      next
    }
    
  # Select "NO" to STOP
    if(next_selection == 2){ 
      cat(col_red("You selected 'No'.\n"))
      
    #**SAVE ERROR REPORT*
      cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n"))
      cat(col_red(paste0("Please review the tablet files for Haul ", haul_id, " in the 'Queue' folder.\n\n", sep = "")))
      break
    }
  }
    
  
  
  
  
  
  
  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Start catch/specimen checks ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
# Designate "catch" and "specimen" dfs to be checked
#**re-calculate/combine catch numbers by species....sum/double check the rounding on #specimens* 
#*-- should automatically take care of 2 tablet scenarios...
  specimen <- list.files(in_dir, pattern = paste0("_CRAB_SPECIMEN_", haul_number), recursive = TRUE) %>%
              map_df(~read.csv(paste0(in_dir, .x)))
    
  catch <- list.files(in_dir, pattern = paste0("_CRAB_CATCH_", haul_number), recursive = TRUE) %>%
           map_df(~read.csv(paste0(in_dir, .x))) %>%
           group_by(VESSEL, CRUISE, HAUL, STATION, COMMON_NAME, SPECIES_CODE) %>%
           # combine weights and catch numbers by species (if 2 tablets were used for the haul)
           summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                     NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE), 
                     .groups = "drop_last")
           # Need to check specimen #s should only be off by 1, right (if 2 tablets)??




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

  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#**SOURCE SPECIMEN CHECK FUNCTION HERE* (see below)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
##**AFTER CHECKS ARE DONE:*    
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# summary of any notes associated with haul (e.g. saw rotting clutches, etc.)

# output summary, say will pop up, check it then verify if good/not
#   -- how to disregard errors so they don't come up again in recheck....might be too tricky for now?
  cat(paste0("Printing error summary for Haul ", haul_id, "...\n\n" ))
  print(errors)

# message Y/N move to clean folder? Copy to thumb drive??
# - need directories for "clean", thumb drive backup, to FTP folder?
# - also need a message/reminder to double check that the files went where you think they did!!
  
  cat(paste0("Please review the errors for Haul ", haul_id, ".\n" ))
  cat(paste0("If there is nothing that needs correcting, please select 'Yes' when prompted to output the final error report and move the tablet files to their final folders.\n" ))
  cat(paste0("If corrections for Haul ", haul_id, " are needed, please select 'No'.\n\n" ))
  
  ok_selection <- menu(c("Yes", "No"), title = paste0("Is Haul ", haul_id, " clean and ready to be moved out of the 'QAQC_queue' folder?\n"))
  
  if(ok_selection == 1){
    
    #**WRITE ERROR REPORT HERE* 
    
 
    #**MOVE FILES*
    #*need to send to "Crab CATCH Files/", "Crab SPECIMEN Files/", and "RAW Haul Files/"
    # to_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE) %>%
    #             # list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
    #             map(~file.rename(from = paste0(in_dir, .x),
    #                              to = paste0(clean_dir, "_archive/", .x)))
    files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
    to_clean <- move_files(files = files_clean,
                           vessel = vessel,
                           leg = leg, 
                           haul_number = haul_number,
                           path = path, 
                           destination = "clean")
    

    cat("\nYou selected 'Yes'.\n\n", sep = "")
    cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 

    
    #**Also provide options/call functions to copy to sFTP and backup USB*
    copy_selection <- menu(c("Yes", "No"), title = "Would you like to copy the clean tablet files and final error report to the sFTP queue and USB backup?")
      
    if(copy_selection == 1){
      
      copyFTP <- copy_files(vessel = vessel,
                            leg = leg, 
                            haul_number = haul_number,
                            file_type = "tablet", 
                            path = path, 
                            destination = "sftp")
      
      #**OK so might have to make each vessel/leg/archive subfolder individually as commands before it'll make the file type folders....
      copyUSB <- copy_files(vessel = vessel,
                            leg = leg, 
                            haul_number = haul_number,
                            file_type = "tablet", 
                            path = path, 
                            destination = "backup")
      
      cat("\nYou selected 'Yes'.\n\n", sep = "")
      cat("Copying Haul ", haul_id, " tablet files and error report to the ", vessel, " ", leg, " folders in the sFTP queue and USB backup.\n\n", sep = "")
    }

    if(copy_selection == 2){
      cat(col_red("You selected 'No'.\n"))
      cat(col_red("Tablet files and the final error report for Haul ", haul_id, " have not been backed up. Please make sure to copy these files into the sFTP queue and USB backup folders.\n\n"))
    }
    
    
    # Move on to next haul?
    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul?")
    
    if(next_selection == 1){
      cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
      next
    }
    
    if(next_selection == 2){
      cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
      break
    }
  }
  
  
  if(ok_selection == 2){
    #**WRITE ERROR REPORT*
    
    cat(col_red("You selected 'No'.\n"))
    cat(col_red("Please review the errors in the error report and make changes in tablet as needed.\n"))
    cat(col_red("When the errors have been corrected, please put the re-extracted files into the 'QAQC_queue' folder and run them through the error checking protocol again. Don't forget to annotate any changes in the existing error report for Haul ", haul_id, " as well.\n\n"))

    
    ##**MOVE FILES TO ARCHIVE??*
    archive_selection <- menu(c("Yes", "No"), title = paste0("Would you like to archive the existing tablet files for Haul ", haul_id, " ?"))
      
    if(archive_selection == 1){
      
      #**NEED something different to archive other files out of the queue...*
      #*take the 'files_clean' and feed it through the move to archive function?? 
      # files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
      
      # to_archive <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
      #               map(~file.rename(from = paste0(in_dir, .x),
      #                                to = paste0(clean_dir, "_archive/", .x)))
                
      cat("You selected 'Yes'.\n")
      cat("Tablet files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
    }
    
    if(archive_selection == 2){
      cat(col_red("You selected 'No'.\n"))
      cat(col_red("Tablet files for Haul ", haul_id, " have not been moved. Please make sure to move these files out of the 'QAQC_queue' folder and into the ", vessel, " ", leg, " 'Archive' folder before running error checks on this haul again.\n\n"))
    }

    
    # Move on to next haul?
    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?")
    
    if(next_selection == 1){
      cat("You selected 'Yes'.\n")
      cat("Saving Haul ", haul_id, " error report and starting error checks for the next haul.\n\n", sep = "")
      next
    }
    
    if(next_selection == 2){
      cat(col_red("You selected 'No'.\n"))
      cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
      break
    }
    
  }
  

  
  
  # next_selection <- menu(c("Yes", "No"), title = "Would you like to move on to the next haul in the meantime?")
  # 
  # if(next_selection == 1){
  #   #**WRITE ERROR REPORT??* Think about if need 2 error report locations....
  #   #*one in QAQC as temp, and then a final one that gets moved once the haul is "approved"?
  #   cat("\nYou selected 'Yes'.\n\n", sep = "")
  #   cat("Saving Haul ", haul_id, " error report.\n", sep = "") 
  #   cat("Starting error checks for the next haul.\n\n", sep = "")
  #   # cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
  #   next
  # }
  # 
  # if(next_selection == 2){
  #   #**WRITE ERROR REPORT*
  #   
  #   cat(col_red("You selected 'No'.\n"))
  #   cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
  #   break
  # }

# Y/N move on to next haul?


  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# At the end: ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
#**STOP -- take a look at the error report. Do you need to make any changes??*
#*If no, do you want to move the files?? if yes, do you want to archive the existing files for the haul??

  
  
#**Then re-compile catch and specimen db tables!*ONLY* from the "clean" folder (by leg)
# - should I have a pre-check to make sure there are no duplicate files in the clean??
#   - only check duplicate timestamps for haul/tablet, might have 2 tablets for a haul still...
#*this function also automatically has lines to copy db to sFTP and backup USB

  compile_db <- compile_db_files(vessel = vessel,
                                 leg = leg,
                                 path = path)
  
  

  
  
  
  
