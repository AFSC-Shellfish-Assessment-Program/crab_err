
# Function: final_haul_checks --------------------------------------------------
#
# Purpose: Function to print error report, have user check, file cleanup options
#
# output messages summary, say will pop up, check it then verify if good/not
#**MAKE EXTRA FUNCTION TO BE ABLE TO DO CLEANUP FOR A HAUL INDEPENDENTLY?? EG move to clean...*
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --


final_haul_checks <- function(haul_number,
                              errors,
                              error_report,
                              final_haul,
                              metadata){

  # Unpack metadata
    vessel <- metadata$vessel
    leg <- metadata$leg
    recorder <- metadata$recorder
  
  
  # Set file directory
    in_dir <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop/QAQC Queue"), winslash = "/")
    
  
  # Create haul ID for output file naming
    haul_id <- str_remove(haul_number, "^0+")
    
  
  # Print messages
    cat("\nSpecimen checks for Haul ", haul_id, " have been completed.", sep = "")
  
    if(nrow(errors) == 0){
      cat("\nNo errors or flags were found for Haul ", haul_id, ".\n\n", sep = "")
    }
  
    if(nrow(errors) > 0){
      cat("\nPrinting error summary for Haul ", haul_id, ".\n\n", sep = "")
      print(errors)
    }
  
    cat(style_bold("\n\nPlease pause here and review the printed errors for Haul ", haul_id, " above.\n" ))
    cat("   - If there no corrections or Error Report annotations that need to be made, please select 'Yes' when prompted to move the tablet files to their final folders.\n")
    cat("   - If corrections for Haul ", haul_id, " are needed, or flags need to be annotated in the temporary Error Report, please select 'No'.\n\n", sep = "")
    
    
  # Option: is the haul clean?  
    ok_selection <- menu(c("Yes", "No"), title = paste0("Is Haul ", haul_id, " clean and ready to be moved out of the 'QAQC Queue' folder?"))
    
  # YES, clean
    if(ok_selection == 1){
    
      # Save error report
        report_save(metadata, 
                    haul_number = haul_number,
                    errors = errors,
                    error_report = error_report)
      
      # Move Tablet Files
        files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
        to_clean <- move_files(files = files_clean,
                               metadata = metadata, 
                               haul_number = haul_number,
                               file_type = "tablet",
                               destination = "clean")
        
      # Move Error Report
        final_report <- move_files(files = list.files(paste0(path, "/Temporary Error Reports/"), 
                                                      pattern = paste0(vessel, "_", leg, "_Haul", haul_id)),
                                   metadata = metadata, 
                                   haul_number = haul_number,
                                   file_type = "error_report",
                                   destination = "clean")
        
      # Print messages
        cat("\nYou selected 'Yes'.\n", sep = "")
        cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 
        
        
      # Option to copy files to FTP Queue and USB  
        copy_selection <- menu(c("Yes", "No"), title = "Would you like to copy the clean tablet files and final Error Report to the FTP Queue and USB Backup?")
        
      # YES, copy  
        if(copy_selection == 1){
          
          # Copy files
            copyFTP <- copy_files(metadata = metadata,  
                                  haul_number = haul_number,
                                  file_type = "tablet", 
                                  destination = "ftp")
            
            copyUSB <- copy_files(metadata = metadata,  
                                  haul_number = haul_number,
                                  file_type = "tablet", 
                                  destination = "backup")
          
          # Print messages 
            cat("\nYou selected 'Yes'.\n", sep = "")
            cat("Copying Haul ", haul_id, " tablet files and final Error Report to the FTP Queue and USB Backup.\n\n", sep = "")
        } # end 'copy'
        
      # NO, don't copy
        if(copy_selection == 2){
          
          # Print messages
            cat(col_red("\nYou selected 'No'.\n"))
            cat(col_red("Tablet files and the final Error Report for Haul ", haul_id, " have not been backed up. Please make sure to copy these files into the FTP Queue and USB Backup folders.\n\n"))
        } # end 'no copy'
        
        
      # Move on to next haul?
        if(final_haul == TRUE){
          
          # Print messages
            cat("This is the final haul in the QAQC Queue. Stopping the error checks.\n\n") 
            cat("Please go back and review any files in the 'Temporary Error Reports' folder on the Desktop and make any data corrections or report annotations before running the final lines of the QAQC script.\n\n")
            cat(rep("-", getOption("width")), sep = "")
            cat("\n\n\n\n\n\n")
            
            out <- "break" # Do I need this line??
        } # BREAK
        
        if(final_haul == FALSE){

          # Option to move to next haul
            next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul?")
            
          # YES, next haul
            if(next_selection == 1){
              # Print messages
                cat("\nYou selected 'Yes' - starting error checks for the next haul.\n\n")
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n\n\n\n\n")
                
                out <- "next"
            } # NEXT
            
          # NO, stop checks
            if(next_selection == 2){
              # Print messages
                cat(col_red("\nYou selected 'No' - stopping the error checks.\n\n"))
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n\n\n\n\n")
                
                out <- "break"
            } # BREAK
        } # end 'move to next haul' chunk
    } # end 'yes clean haul' chunk
    
   
  # NO, not clean 
    if(ok_selection == 2){
      
      #**WRITE ERROR REPORT*
    
      
      # Print messages
        cat(col_red("\nYou selected 'No'.\n"))
        cat(col_red(style_bold("Please pause here and review any errors or flags in the temporary Error Report file for Haul ", haul_id, ".\n\n")))
        cat(col_red("Determine if there are either:\n"))
        cat(col_red("   (1) Data corrections that need to be made in the tablet; OR \n"))
        cat(col_red("   (2) No data corrections needed but messages to annotate\n"))
        cat(col_red("before responding to the prompt below.\n\n\n"))
        
      #**Option: Are there any corrections that need to be made in the tablet?*
        corrections_selection <- menu(c("Yes", "No"), title = paste0("\nDo any data corrections need to be made in the tablet for Haul ", haul_id, "?"))
      
      # YES, corrections needed
        if(corrections_selection == 1){
          
          # Print messages
            cat("\nYou selected 'Yes'. Please closely follow the options below.\n")
          
          # Option: move old files to the archive? 
            archive_selection <- menu(c("Yes", "No"), title = paste0("\nFirst, would you like to Archive the existing tablet files for Haul ", haul_id, " that have errors?"))
            
          # YES, archive
            if(archive_selection == 1){
              
              #**NEED something different to archive other files out of the queue...*
              #*take the 'files_clean' and feed it through the move to archive function?? 
              # files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
              
              # to_archive <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
              #               map(~file.rename(from = paste0(in_dir, .x),
              #                                to = paste0(clean_dir, "_archive/", .x)))
              
              # Print messages
                cat("\nYou selected 'Yes'.\n")
                cat("Tablet files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
                
                cat("Now, please", style_bold("pause"), "here and:\n")
                cat("   (1) Make any data corrections on the tablet and re-extract all files;\n")
                cat("   (2) Annotate all messages in the temporary Error Report 'NOTES' column with corrections made or other context;\n")
                cat("   (3) Add the re-extracted tablet files into the 'QAQC Queue' folder;\n")
                cat("   (4) Re-run the error checks for this haul when you're able.\n\n")
              
              
              # Move on to next haul?
                if(final_haul == TRUE){
                  
                  # Print messages
                    cat("This is the final haul in the QAQC Queue. Stopping the error checks.\n\n") 
                    cat("Please go back and review any files in the 'Temporary Error Reports' folder on the Desktop and make any data corrections or report annotations before running the final lines of the QAQC script.\n\n")
                    cat(rep("-", getOption("width")), sep = "")
                    cat("\n\n\n\n\n\n")
                    
                    out <- "break" # Do I need this line??
                } # BREAK
                
                if(final_haul == FALSE){
                  
                  # Option to move to next haul
                    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?")
                  
                  # YES, next haul
                    if(next_selection == 1){
                      # Print messages
                        cat("\nYou selected 'Yes' - starting error checks for the next haul.\n\n")
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "next"
                    } # NEXT
                  
                  # NO, stop checks
                    if(next_selection == 2){
                      # Print messages
                        cat(col_red("\nYou selected 'No' - stopping the error checks.\n\n"))
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "break"
                    } # BREAK
                } # end 'move to next haul' chunk
            } # end 'yes archive' section
            
              
          # NO, don't archive
            if(archive_selection == 2){
              
              # Print messages
                cat(col_red("\nYou selected 'No'.\n"))
                cat(col_red("Tablet files for Haul ", haul_id, " have not been moved.\nPlease make sure to move these files out of the 'QAQC Queue' folder and into the ", vessel, " ", leg, " 'Archive' folder before running error checks on this haul again.\n\n"))
              
                cat("When you're ready to address this haul:\n")
                cat("   (1) Make any data corrections on the tablet and re-extract all files;\n")
                cat("   (2) Annotate all messages in the temporary Error Report 'NOTES' column with corrections made or other context;\n")
                cat("   (3) Move old Haul ", haul_id, " tablet files out of the 'QAQC Queue' folder and into the ", vessel, " ", leg, " 'Archive' folder;\n", sep = "")
                cat("   (4) Add the re-extracted tablet files into the 'QAQC Queue' folder;\n")
                cat("   (5) Re-run the error checks for this haul.\n\n")  
              
                
              # Move on to next haul?
                if(final_haul == TRUE){
                  
                  # Print messages
                    cat("This is the final haul in the QAQC Queue. Stopping the error checks.\n\n") 
                    cat("Please go back and review any files in the 'Temporary Error Reports' folder on the Desktop and make any data corrections or report annotations before running the final lines of the QAQC script.\n\n")
                    cat(rep("-", getOption("width")), sep = "")
                    cat("\n\n\n\n\n\n")
                    
                    out <- "break" # Do I need this line??
                } # BREAK
                
                if(final_haul == FALSE){
                  
                  # Option to move to next haul
                    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?")
                  
                  # YES, next haul
                    if(next_selection == 1){
                      # Print messages
                        cat("\nYou selected 'Yes' - starting error checks for the next haul.\n\n")
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "next"
                    } # NEXT
                  
                  # NO, stop checks
                    if(next_selection == 2){
                      # Print messages
                        cat(col_red("\nYou selected 'No' - stopping the error checks.\n\n"))
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "break"
                    } # BREAK
                } # end 'move to next haul' chunk
            } # end 'no don't archive' section
        } # end 'tablet corrections' chunk
      
      
      # NO, just annotations to report
        if(corrections_selection == 2){
          
          # Print messages
            cat("\nYou selected 'No'. Please", style_bold("pause"), "here and:\n")
            cat("   (1) Open the temporary Error Report file for Haul", haul_id, "in the Desktop folder;\n")
            cat("   (2) Annotate any flags to provide context in the 'NOTES' column;\n")
            cat("   (3) Save and close the file;\n")
            cat("   (4) Proceed with the prompts here in the RStudio console.\n\n")
          
          #**option to move to clean once messages are annotated....*
          # Option: is the haul clean now?  
            ok_selection <- menu(c("Yes", "No"), title = paste0("Is Haul ", haul_id, " now clean, annotated, and ready to be moved out of the 'QAQC Queue' folder?"))
            
          # YES, clean
            if(ok_selection == 1){
              
              #**WRITE ERROR REPORT HERE* TO TEMP, then move to FINAL
              
              
              #**MOVE FILES*
                files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
                to_clean <- move_files(files = files_clean,
                                       metadata = metadata,  
                                       haul_number = haul_number,
                                       destination = "clean")
              
              #**MOVE ERROR REPORT*
              
              # Print messages
                cat("\nYou selected 'Yes'.\n", sep = "")
                cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 
              
              
              # Option to copy files to FTP Queue and USB  
                copy_selection <- menu(c("Yes", "No"), title = "Would you like to copy the clean tablet files and final Error Report to the FTP Queue and USB Backup?")
              
              # YES, copy  
                if(copy_selection == 1){
                  
                  # Copy files
                    copyFTP <- copy_files(metadata = metadata, 
                                          haul_number = haul_number,
                                          file_type = "tablet", 
                                          destination = "ftp")
                    
                    copyUSB <- copy_files(metadata = metadata, 
                                          haul_number = haul_number,
                                          file_type = "tablet", 
                                          destination = "backup")
                  
                  # Print messages 
                    cat("\nYou selected 'Yes'.\n", sep = "")
                    cat("Copying Haul ", haul_id, " tablet files and final Error Report to the FTP Queue and USB Backup.\n\n", sep = "")
                } # end 'copy'
              
              # NO, don't copy
                if(copy_selection == 2){
                  
                  # Print messages
                    cat(col_red("\nYou selected 'No'.\n"))
                    cat(col_red("Tablet files and the final Error Report for Haul ", haul_id, " have not been backed up. Please make sure to copy these files into the FTP Queue and USB Backup folders.\n\n"))
                } # end 'no copy'
              
              
              # Move on to next haul?
                if(final_haul == TRUE){
                  
                  # Print messages
                    cat("This is the final haul in the QAQC Queue. Stopping the error checks.\n\n") 
                    cat("Please go back and review any files in the 'Temporary Error Reports' folder on the Desktop and make any data corrections or report annotations before running the final lines of the QAQC script.\n\n")
                    cat(rep("-", getOption("width")), sep = "")
                    cat("\n\n\n\n\n\n")
                    
                    out <- "break" # Do I need this line??
                } # BREAK
              
                if(final_haul == FALSE){
                  
                  # Option to move to next haul
                    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul?")
                  
                  # YES, next haul
                    if(next_selection == 1){
                      
                      # Print messages
                        cat("\nYou selected 'Yes' - starting error checks for the next haul.\n\n")
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "next"
                    } # NEXT
                    
                  # NO, stop checks
                    if(next_selection == 2){
                      
                      # Print messages
                        cat(col_red("\nYou selected 'No' - stopping the error checks.\n\n"))
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "break"
                    } # BREAK
                } # end 'move to next haul' chunk
            } # end 'yes now clean haul' chunk
            
            
            if(ok_selection == 2){
              
              # Print messages
                cat(col_red("\nYou selected 'No'. Annotations to the Haul ", haul_id, " temporary Error Report are still needed.\n"))
                cat(col_red("Please follow the steps listed above at some point and move the files to the 'clean' folders when completed.\n\n"))
              
              #**MAKE EXTRA FUNCTION TO BE ABLE TO DO CLEANUP FOR A HAUL INDEPENDENTLY?? EG move to clean...*

              
              
              # Move on to next haul?
                if(final_haul == TRUE){
                  
                  # Print messages
                    cat("This is the final haul in the QAQC Queue. Stopping the error checks.\n\n") 
                    cat("Please go back and review any files in the 'Temporary Error Reports' folder on the Desktop and make any data corrections or report annotations before running the final lines of the QAQC script.\n\n")
                    cat(rep("-", getOption("width")), sep = "")
                    cat("\n\n\n\n\n\n")
                    
                    out <- "break" # Do I need this line??
                } # BREAK
              
                if(final_haul == FALSE){
                  
                  # Option to move to next haul
                    next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?")
                  
                  # YES, next haul
                    if(next_selection == 1){
                      
                      # Print messages
                        cat("\nYou selected 'Yes' - starting error checks for the next haul.\n\n")
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "next"
                    } # NEXT
                  
                  # NO, stop checks
                    if(next_selection == 2){
                      
                      # Print messages
                        cat(col_red("\nYou selected 'No' - stopping the error checks.\n\n"))
                        cat(rep("-", getOption("width")), sep = "")
                        cat("\n\n\n\n\n\n")
                        
                        out <- "break"
                    } # BREAK
                } # end 'move to next haul' chunk
            } # end 'no not clean haul' chunk
        } # end 'messages annotation' chunk
    } # end 'not clean' section
    
    return(out)
}
