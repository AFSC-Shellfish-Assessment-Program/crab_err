
# ON-BOARD CRAB DATA QAQC WORKFLOW ---------------------------------------------
#
# USER NOTES:
#   - Always run from Step 1 to make sure everything is loaded properly
#   - Please don't rename or move folders!
#   - ...
#   - Let Shannon know if you come across any unknown errors or illogical options/pathways
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# Step 1. Source overall workflow setup script.
  source("./Scripts/functions/setup.R")

# Step 2. Set vessel, leg, and person running the error checks.
##        Please only use the exact 'vessel' and 'leg' inputs shown commented on the lines
  metadata <- list(vessel = "AKK",    # "AKK", "NWEx"
                   leg = "Leg1",      # "Leg1", "Leg2, "Leg3", "Leg4"
                   recorder = "Shannon Hennessey")

# Step 3. Check inputs to ensure subsequent functions will run smoothly.
##        Look for the message 'Key inputs have been validated.' before proceeding. 
  check_inputs(metadata = metadata,
               message = TRUE)
  
# Step 4. Check the QAQC Queue to confirm which hauls are present in the queue. 
  haul_info_all <- file_checks_global(queue_dir)
  
# Step 5. Run haul-specific checks. 
  haul_qaqc <- haul_checks(metadata,
                           haul_info_all = haul_info_all)
  
# Step 6. Compile Catch and Specimen databases.
##        Once the haul(s) in the QAQC Queue have been checked and corrected  
##        or annotated (if necessary), compile and back up the most up-to-date 
##        versions of the Catch and Specimen db files.
  compile_db <- compile_db_files(metadata)

  
  
