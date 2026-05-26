
# Run all checks for a given haul

haul_checks <- function(haul_info_all,
                        vessel,
                        leg,
                        recorder){
  
  # Run haul-specific checks 
  # For each haul in the queue, these checks will:
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
      
      
      # Set indicator that this is the only/last haul
        final_haul <- ifelse(h == length(hauls), TRUE, FALSE)
      
        
      # Run Error Report checks and setup
        error_report <- report_check(vessel = vessel,
                                     leg = leg,
                                     haul_number = haul_number)
        
      # If report_check returns a list, propagate that through
        if(is.list(error_report)){
          report <- file_checks$report
          report_date <- file_checks$report_date
          report_time <- file_checks$report_time
          recheck <- file_checks$recheck
        }
        
      # If haul_checks returns a character, set 'break' for the haul loop
        if(is.character(error_report)){
          if(error_report == "break"){
              break
          } else{
            # Print error message
              cat(col_red("An unknown error has occurred when checking the Error Reports for Haul ", haul_id, ".\n"))
              cat(col_red("Please review any Error Report files in the Desktop or 'clean' folders for this haul and try again.\n"))
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n\n\n\n\n")
              break
          }
        }
        
        
      #**CREATE ERROR VECTOR/ITERATOR FOR HAUL*
        errors <- data.frame(ERROR_TYPE = character(),
                             ERROR_MESSAGE = character(),
                             stringsAsFactors = FALSE)
        
        
        
      # Run file checks for haul -----------------------------------------------
        file_checks <- file_checks_haul(haul_info_all = haul_info_all,
                                        haul_number = haul_number,
                                        final_haul = final_haul)
      
      # If haul_checks returns a list of errors, propagate that through
        if(is.list(file_checks)){
          errors <- file_checks$errors
          files_all <- file_checks$files_all
          # haul_info_all <- file_checks$haul_info_all
          haul_info <- file_checks$haul_info
        }
      
      # If haul_checks returns a character, set 'next' or 'break' for the haul loop
        if(is.character(file_checks)){
          if(file_checks == "next"){
            next
          }
          
          if(file_checks == "break"){
            break
          }
        }
        
        
      # Run specimen checks ----------------------------------------------------
        specimen_errs <- specimen_checks(files_all = files_all,
                                         haul_info = haul_info,
                                         errors = errors)
        
          # If haul_checks returns a list of errors, propagate that through
            if(is.list(specimen_errs)){
              errors <- specimen_errs$errors
            }

      
        
        

      # Run final haul checks/options ------------------------------------------  
      # Automatically print EVERY error report in to the Temporary Error Reports folder
      #**STOP -- take a look at the error report. Do you need to make any changes??*
      #**ANNOTATE ANYTHING IN TEMP ERROR REPORT BEFORE PROCEEDING!!*
      #*Do not add notes to error report once in "clean" or else will not get backed up/FTPd
        final_checks <- final_haul_checks(haul_id = haul_id,
                                          errors = errors,
                                          final_haul = final_haul)
        
        
        
        
        
    } # end haul loop

    return()
  
}