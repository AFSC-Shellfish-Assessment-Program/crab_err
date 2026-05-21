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
# DOUBLE CHECK folder names/syntax --> "Clean", capital Error Report, FTP Queue, USB Backup, Archive, QAQC Queue
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
  
  check_inputs(vessel = vessel,
               leg = leg, 
               recorder = recorder)
  
  
# Step 4. Run global file checks on all hauls in the QAQC Queue 
#         These will confirm which hauls are present in the queue 
  
  haul_info_all <- file_checks_global(queue_dir)
  

# Step 5. Run haul-specific checks 
#         For each haul in the queue, these checks will:
#            - Validate vessel, cruise, and station ID entries
#            - ID potential 0-catch stations
#            - Inventory files to check for multiple copies and to make sure all tablet files are present
#            - Run Catch and Specimen checks
#            - Have the user review and validate any errors or flags
#            - Write temporary or final Error Reports
#            - Provide options for moving and copying files to 'Clean', 'FTP Queue' and USB Backup folders 
#              for clean hauls, or to 'Archive' folders for hauls that require corrections  

# Loop over hauls
  hauls <- unique(haul_info_all$HAUL_NUMBER)
  
  for(h in 1:length(hauls)){
    
    # Set haul number
      haul_number <- hauls[h]
    
    # Maybe error report check/creation goes here, instead of in function??
    
      
    # Run file checks for haul
      haul_checks <- file_checks_haul(haul_info_all = haul_info_all,
                                      haul_number = haul_number)
      
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
  
  
# Automatically print EVERY error report in to the Temporary Error Reports folder
#**STOP -- take a look at the error report. Do you need to make any changes??*
#**ANNOTATE ANYTHING IN TEMP ERROR REPORT BEFORE PROCEEDING!!* 
#*Do not add notes to error report once in "clean" or else will not get backed up/FTPd

#**Then source Separate function for final file cleanup (WITHIN HAUL LOOP)*
#*Run function, it gives you the option to say no not clean but please archive, yes clean and please copy to final destinations... 
#*If no, do you want to move the files?? if yes, do you want to archive the existing files for the haul??
  
  
  
# Step 6. Compile the most up-to-date versions of the Catch and Specimen db files
#
#**ONLY* from the "clean" folder (by leg)
# - should I have a pre-check to make sure there are no duplicate files in the clean??
#   - only check duplicate timestamps for haul/tablet, might have 2 tablets for a haul still...
#*this function also automatically has lines to copy db to FTP and backup USB

  compile_db <- compile_db_files(vessel = vessel,
                                 leg = leg)
  
  

# Quick tool to visualize chela measurements taken or anything else???
  
  
  
  
  
