
# Function: report_save --------------------------------------------------------
#
# Purpose: Function to write error report
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --


report_save <- function(metadata, 
                        haul_number,
                        errors,
                        error_report){ # list object with metadata etc.
  
  
  # Unpack metadata
    vessel <- metadata$vessel
    leg <- metadata$leg
    recorder <- metadata$recorder
    
    
  # Run input checks
    check_inputs(metadata,
                 message = FALSE)
  
  
  # # Check that the argument `type` is one of the correct options,
  # # and only has one input of the correct type.
  #   types <-  c("temporary", "final")
  # 
  #   if(length(x = type) > 1){
  #     stop(paste0("Please limit your input to just one report `type`."))
  #   }
  #   
  #   if(!class(type) == "character"){
  #     stop(paste0("The `type` argument must be specified as a character. ",
  #                 "Please modify your `type` input and try again."))
  #   }
  #   
  #   if(!type %in% types){ 
  #     stop(paste0("The `type` argument must be one of the following options",
  #                 " (case-sensitive): 'temporary', 'final'. Please modify your report `type` ",
  #                 "input and try again."))
  #   }
    
  
  # Set directories
    path <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop"), winslash = "/")
    clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/")
  
  
  # Write message if no errors detected
    if(nrow(errors) == 0){
      error_iter <- nrow(errors) + 1
      errors[error_iter, 1] <- "Clean Haul"
      errors[error_iter, 2] <- paste0("No errors or warnings were detected for Haul ", haul_id)
    }
    
    
  # Add metadata column to error report
    errors <- errors %>%
              mutate(VESSEL = vessel,
                     LEG = leg,
                     HAUL = haul_number,
                     DATE = error_report$report_date,
                     TIME = error_report$report_time,
                     RECHECK = ifelse(error_report$recheck == TRUE, "Y", "N"),
                     RECORDER = recorder,
                     NOTES = "") %>%
              select(VESSEL, LEG, HAUL, DATE, TIME, RECHECK, RECORDER, MESSAGE_TYPE, MESSAGE, NOTES)
    
    
  # Format report
    if(!is.null(error_report$report)){
      report_out <- rbind(error_report$report, errors)
    } else{
      report_out <- errors
    }
  
      
  # #**some other sorting/formatting here....*
  #   report_out <- report_out %>%
  #                 mutate(MESSAGE_TYPE = as.factor(MESSAGE_TYPE, 
  #                                                 levels = c("Clean Haul", "No Catch", "File", "Cruise", "Notes", #station name? what else...
  #                                                            "Specimen_ID", "Specimen_Sex", "Specimen_Size",
  #                                                            "Specimen_Shell", "Specimen_Clutch", "Specimen_Maturity", # thinking small immature RKC?
  #                                                            "Specimen_Disease", "Specimen_Range"))) # could also just have "Specimen" errors....and catch?
  #                 arrange(DATE, TIME, MESSAGE_TYPE)

   
  # Save report  
    write.csv(report_out, paste0(path, "/Temporary Error Reports/", vessel, "_", leg, "_Haul", haul_id, "_report.csv"), row.names = FALSE)
}



# Function: report_check -------------------------------------------------------
#
# Purpose: A function to check for existing Error Reports and read them in for 
#          appending 'recheck' errors if so
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --


report_check <- function(metadata,
                         haul_number){
  
  # Unpack metadata
    vessel <- metadata$vessel
    leg <- metadata$leg
    recorder <- metadata$recorder
  
  
  # Set directories
    path <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop"), winslash = "/")
    in_dir <- paste0(path, "/QAQC Queue/")
    clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/")
    
    
  # Create haul ID for output file naming
    haul_id <- str_remove(haul_number, "^0+")
    
  
  # Read in existing report file (if any)
    report_file_temp <- list.files(paste0(path, "/Temporary Error Reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))
    report_file_clean <- list.files(paste0(clean_dir, "Error Reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))
    
    
  # Check if there's a report in the clean --> throw error if so
    if(length(report_file_clean) > 0){
      
      # Print messages
        cat(col_red("An Error Report for Haul ", haul_id, " was found in the 'clean' directory. Please go back and review this file.\n"))
        cat(col_red("   If you are re-checking this haul after designating it 'clean', please:\n"))
        cat(col_red("      (1) Check for any Haul " , haul_id, " temporary Error Reports in the Desktop folder (reconcile/combine these reports if so);\n"))
        cat(col_red("      (2) Move the 'clean' Error Report file back to the 'Temporary Error Reports' folder on the Desktop; and\n"))
        cat(col_red("      (3) Try again for this haul.\n\n"))
        cat(rep("-", getOption("width")), sep = "")
        cat("\n\n\n\n\n\n")
        
        out <- "break"
        return(out)
    } 
    
    
  # Check if there's a temporary report --> read in if so
    if(length(report_file_temp) == 1 & length(report_file_clean) == 0){
      
      # Read in existing error report file
        report <- read.delim(paste0(clean_dir, "Error Reports/", report_file)) 
      
      # Set metadata for error report
        report_date <- Sys.Date()
        report_time <- format(Sys.time(), "%T")
        recheck <- TRUE
      
      # Print message 
        cat("A Temporary Error Report is detected for Haul ", haul_id, ".\n", sep = "")
        cat("Reading in the existing file and setting up the 'Recheck' metadata.\n\n", sep = "")
      
      # Return a list of report metadata
        return(list(report = report,
                    report_date = report_date,
                    report_time = report_time,
                    recheck = recheck,
                    recorder = recorder))
    } 
    
  # If no report anywhere, create new metadata
    if((length(report_file_temp) & length(report_file_clean)) == 0){
      
      # Set metadata for error report
        report_date <- Sys.Date()
        report_time <- format(Sys.time(), "%T")
        recheck <- FALSE
    
      # Return a list of report metadata
        return(list(report = c(),
                    report_date = report_date,
                    report_time = report_time,
                    recheck = recheck,
                    recorder = recorder))
    } 
    
  # If multiple temporary files, throw error to reconcile....
    if(length(report_file_temp) > 1){
      
      # Print messages
        cat(col_red("Multiple temporary Error Report files were detected for Haul ", haul_id, ".\n   To correct this, please:\n"))
        cat(col_red("      (1) Review the files in the 'Temporary Error Reports' folder on the Desktop;\n"))
        cat(col_red("      (2) Reconcile any differences and combine into one comprehensive Error Report file;\n"))
        cat(col_red("      (3) Delete the extra file(s), and try again.\n\n"))
        cat(rep("-", getOption("width")), sep = "")
        cat("\n\n\n\n\n\n")
        
        
      #**do I want to provide an option to move on to the next haul in the meantime??*
        
        out <- "break"
        return(out)
    }
}
