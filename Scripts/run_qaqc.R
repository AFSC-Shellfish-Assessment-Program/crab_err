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

#**- when extracting files, do they all have the *exact* same timestamp, or can they vary???
#    - need to check this when running through all hauls as a test!
#    - also need to BOLD in protocol to please re-extract ALL files if making changes....


#**Write up a step-by-step narrative in code comments!*
#*also set up/push the .Rproj so it opens the right stuff for everyone??
# and have a copy of the protocol in the repo readme


#**Compare total catch vs. estimated weights by species...?*
#* would need to do something with joining the raw files to get the subsample groups/weights
#* with specimens -- can expand weights from there and ID lg in "small" etc.

# How to append .txt files??
# re-error check appends to the separate error report for that haul 


#**Things for main function to need:*
# *make sure to incorporate error checks/messages to validate function inputs!
# - vessel
# - leg
# - recorder (object class)
# - check that paths are valid? (but will be pre-specified...unless good reason for folks to be able to change)


# DOUBLE CHECK folder names/syntax --> "Clean", capital Error Report, sFTP queue, USB Backup, "Archive", QA/QC Queue
#
# Within the haul loop, code in "final options" based on [h vs. length(hauls)] so that it will say ending protocol
# vs. moving on to next haul -- "This is the final haul in the queue. Stopping error checking protocol"




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# START HERE for QAQC setup ---------------------------------------------------- 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Step 1. Source overall workflow setup script - loads libraries, functions, etc. 

  source("./Scripts/functions/setup.R")


# Step 2. Set vessel, leg, and person running the error checks
##        User must specify these objects, and they are sensitive to syntax
##        Please only use the following exact inputs:
##           - vessel: "AKK", "NWEx"
##           - leg: "Leg1", "Leg2, "Leg3", "Leg4"

  vessel <- "AKK"  
  leg <- "Leg1" 
  recorder <- "Shannon Hennessey"


# Step 3. Check inputs to ensure subsequent functions will run smoothly
  
  check <- check_inputs(vessel = vessel,
                        leg = leg, 
                        recorder = recorder)
  
# Step 4. Run global file checks on all hauls in the QAQC Queue ----------------
# These will confirm which hauls are present in the queue 
  
  haul_info_all <- file_checks_global(queue_dir)
  

# Run haul-specific file checks ------------------------------------------------
# For each haul in the queue, these checks will:
#   - Check for existing Error Reports and create check or recheck template
#   - Validate vessel, cruise, and station ID entries
#   - ID potential 0-catch stations
#   - Inventory files to check for multiple copies and to make sure all tablet files are present

# Loop over hauls
  hauls <- unique(haul_info_all$HAUL_NUMBER)
  
  for(h in 1:length(hauls)){
    
    # Set haul number
      haul_number <- hauls[h]
    
    # Maybe error report check/creation goes here, instead of in function??
    
      
    # Run file checks for haul
      haul_checks <- file_checks_haul(haul_info_all = haul_info_all,
                                      haul_number = haul_number,
                                      path = path)
      
    # If haul_checks returns a list of errors, propagate that through
      if(is.list(haul_checks)){
        errors <- haul_checks$errors
      }
      
    # If haul_checks returns a character, set 'next' or 'break' for the haul loop
      if(is.character(haul_checks)){
        if(haul_checks == "next"){
          next
        }
        
        if(haul_checks == "break"){
          break
        }
      }
  }
    
  
  
# Run specimen checks ----------------------------------------------------------
#**maybe needs to be included in above 'h' loop?*
# For each haul, check specimen entries for incomplete or implausible biometrics 
#  
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
#**SOURCE SPECIMEN CHECK FUNCTION HERE* 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  


# Write Error Report -----------------------------------------------------------
  
  
  
  

# After checks are complete ----------------------------------------------------
#
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
    files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
    to_clean <- move_files(files = files_clean,
                           vessel = vessel,
                           leg = leg, 
                           haul_number = haul_number,
                           path = path, 
                           destination = "clean")
    

    cat("\nYou selected 'Yes'.\n\n", sep = "")
    cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 

    
    copy_selection <- menu(c("Yes", "No"), title = "Would you like to copy the clean tablet files and final error report to the sFTP queue and USB backup?")
      
    if(copy_selection == 1){
      
      copyFTP <- copy_files(vessel = vessel,
                            leg = leg, 
                            haul_number = haul_number,
                            file_type = "tablet", 
                            path = path, 
                            destination = "sftp")
      
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
    archive_selection <- menu(c("Yes", "No"), title = paste0("\nWould you like to archive the existing tablet files for Haul ", haul_id, " ?\n"))
      
    if(archive_selection == 1){
      
      #**NEED something different to archive other files out of the queue...*
      #*take the 'files_clean' and feed it through the move to archive function?? 
      # files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
      
      # to_archive <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
      #               map(~file.rename(from = paste0(in_dir, .x),
      #                                to = paste0(clean_dir, "_archive/", .x)))
                
      cat("\nYou selected 'Yes'.\n")
      cat("\nTablet files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
    }
    
    if(archive_selection == 2){
      cat(col_red("\nYou selected 'No'.\n"))
      cat(col_red("\nTablet files for Haul ", haul_id, " have not been moved. Please make sure to move these files out of the 'QAQC_queue' folder and into the ", vessel, " ", leg, " 'Archive' folder before running error checks on this haul again.\n\n"))
    }

    
    # Move on to next haul?
    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?\n")
    
    if(next_selection == 1){
      cat("\nYou selected 'Yes'.\n")
      cat("\nSaving Haul ", haul_id, " error report and starting error checks for the next haul.\n\n", sep = "")
      next
    }
    
    if(next_selection == 2){
      cat(col_red("\nYou selected 'No'.\n"))
      cat(col_red("\nSaving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
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


  
  
# Compile most recent db -------------------------------------------------------
#  
#**STOP -- take a look at the error report. Do you need to make any changes??*
#*If no, do you want to move the files?? if yes, do you want to archive the existing files for the haul??

  
#**Then re-compile catch and specimen db tables!*ONLY* from the "clean" folder (by leg)
# - should I have a pre-check to make sure there are no duplicate files in the clean??
#   - only check duplicate timestamps for haul/tablet, might have 2 tablets for a haul still...
#*this function also automatically has lines to copy db to sFTP and backup USB

  compile_db <- compile_db_files(vessel = vessel,
                                 leg = leg,
                                 path = path)
  
  

  
  
  
  
