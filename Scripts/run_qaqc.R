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
#
# Please always run from the beginning to make sure everything is loaded properly


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
               recorder = recorder,
               message = TRUE)
  
  
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

  haul_qaqc <- haul_checks(vessel = vessel,
                           leg = leg,
                           recorder = recorder,
                           haul_info_all = haul_info_all)
  
  
  
# Step 6. Compile the most up-to-date versions of the Catch and Specimen db files
#
#**ONLY* from the "clean" folder (by leg)
# - should I have a pre-check to make sure there are no duplicate files in the clean??
#   - only check duplicate timestamps for haul/tablet, might have 2 tablets for a haul still...
#*this function also automatically has lines to copy db to FTP and backup USB

  compile_db <- compile_db_files(vessel = vessel,
                                 leg = leg)
  
  

# Quick tool to visualize chela measurements taken or anything else???
  
  
  
  
  
