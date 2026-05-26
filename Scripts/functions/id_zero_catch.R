
# Function to identify zero-catch stations -------------------------------------

#  **do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
#  and verify 0-catch and say please add a note??

#**NEED TO UPDATE WITH FUNCTION TO SAVE ERROR REPORT AND MOVE FILES TO CLEAN!!!*

id_zero_catch <- function(files, 
                          errors,
                          vessel,
                          leg,
                          haul_number,
                          final_haul){

  # Set 'haul_id'
    # haul_id <- str_remove(haul_number, "^0+")
    haul_id <- str_remove(haul_number, "^0+")
      
  # Print message: Was this a 0-catch station? (YES/NO)
    no_catch_selection <- menu(c("Yes", "No"), title = paste0("Was Haul ", haul_id, " a zero-catch station?"))
      
      
  # Select 'YES' this was 0-catch:
    if(no_catch_selection == 1){
      
      # If this was the final haul, prompt to review any reports in the Temporary Error Reports folder 
      # and make any corrections etc.
        if(final_haul == TRUE){

          ## OUTPUT ERROR REPORT HERE indicating 0-catch station -- make function for outputting error report and call here!!!!
          error_iter <- nrow(errors) + 1
          errors[error_iter, 1] <- "No Catch"
          errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
          
          ## MOVE FILES
          ## AND COPY TO sFTP and USB (including error report....)
          
          cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " 'Clean' folders.\n", sep = "") 
          cat("This is the final haul in the QAQC Queue. Stopping the error checking protocol.\n\n") 
          # More instructions here?? Please validate things, or continue with the final lines of the script or something?
          cat(rep("-", getOption("width")), sep = "")
          cat("\n\n\n\n\n\n")
          
          out <- "break" 
        } 
      
      
      # If this was not the final haul, print message: Move to next haul? (YES/NO)
        if(final_haul == FALSE){
            move_selection <- menu(c("Yes", "No"), title = "\nYou selected 'Yes' - would you like to move on to the next haul?")
            
            
          # Select 'YES' to move to next haul:
            if(move_selection == 1){
                cat("\nYou selected 'Yes'.\n\n", sep = "")
                
              ## OUTPUT ERROR REPORT HERE indicating 0-catch station -- make function for outputting error report and call here!!!!
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "No Catch"
                errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
                
              ## MOVE FILES
              ## AND COPY TO sFTP and USB (including error report....)
                
                cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " 'Clean' folders.\n", sep = "") 
                cat("Starting error checks for the next haul.\n\n", sep = "")
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n\n\n\n\n")
                
                out <- "next"
            }
          
          
          # Select 'NO' to *NOT* move to next haul:
            if(move_selection == 2){
                cat("\nYou selected 'No'.\n\n", sep = "")
                
              ## OUTPUT ERROR REPORT HERE indicating 0-catch station
                error_iter <- nrow(errors) + 1
                errors[error_iter, 1] <- "No Catch"
                errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
                
              ## MOVE FILES
              ## AND COPY TO sFTP and USB (including error report...)
                
                cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " 'Clean' folders.\n", sep = "")
                cat("Copying Haul ", haul_id, " files to the FTP Queue and USB Backup.\n\n", sep = "")
                cat(col_red("Stopping the error checking protocol.\n\n", sep = ""))
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n\n\n\n\n")
                
                out <- "break"
            }
        }
    } 
    
    
  # Select 'NO' there was a catch:
    if(no_catch_selection == 2){
        cat("You selected 'No'.\n\n")
        cat(col_red("No 'CATCH' or 'SPECIMEN' files for Haul ", haul_id, " are present in the QAQC Queue.\n", sep = ""))
        cat(col_red("Please make sure those files are in the 'QAQC Queue' folder and try again for this haul.\n\n"))
          
      #**PRINT ERROR REPORT*
        error_iter <- nrow(errors) + 1
        errors[error_iter, 1] <- "File"
        errors[error_iter, 2] <- paste0("No 'CATCH' or 'SPECIMEN' files are present in the QAQC Queue")
      
        
      # If this was the final haul, prompt to review any reports in the Temporary Error Reports folder 
      # and make any corrections etc.
        if(final_haul == TRUE){
          
          # SAVE ERROR REPORT
          
            cat("This is the final haul in the QAQC Queue. Saving Haul", haul_id, "temporary Error Report and stopping the error checking protocol.\n\n") 
            # More instructions here?? Please validate things, or continue with the final lines of the script or something?
            cat(rep("-", getOption("width")), sep = "")
            cat("\n\n\n\n\n\n")
          
            out <- "break" 
        } 
        
      
      # If there are more hauls to check, print message: Move on to next haul? (YES/NO)
        if(final_haul == FALSE){
            
          next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
          
          # Select 'YES' move to next haul:
            if(next_selection == 1){
                cat("You selected 'Yes'.\n")
              
              # SAVE ERROR REPORT
              
              
                cat("Saving Haul ", haul_id, " temporary Error Report and starting error checks for the next haul.\n\n", sep = "")
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n\n\n\n\n")
                
                out <- "next"
            }
          
          # Select 'NO' don't move to next haul:
            if(next_selection == 2){ #**PRINT ERROR REPORT?*
                cat(col_red("You selected 'No'.\n"))
              
              # SAVE ERROR REPORT
              
              
                cat(col_red("Saving Haul ", haul_id, " temporary Error Report and stopping the error checking protocol.\n\n", sep = ""))
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n\n\n\n\n")
                
                out <- "break"
            }
        }  
    }
    
    
    # return(list(errors = errors,
    #             out = out))  
    
    return(out)
}

