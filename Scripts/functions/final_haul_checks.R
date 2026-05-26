

# Function to print error report, have user check, file cleanup options

final_haul_checks <- function(haul_id,
                              errors,
                              final_haul){
  
  
  
  # After checks are complete ----------------------------------------------------
  #
  # summary of any notes associated with haul (e.g. saw rotting clutches, etc.)
  #**STOP -- take a look at the error report. Do you need to make any changes??*
  #*If no, do you want to move the files?? if yes, do you want to archive the existing files for the haul??
  
  
  # output summary, say will pop up, check it then verify if good/not
  #   -- how to disregard errors so they don't come up again in recheck....might be too tricky for now?
    cat("\nSpecimen checks for Haul ", haul_id, " have been completed.\nPrinting error summary for Haul ", haul_id, ".\n\n", sep = "")
    print(errors)
    
    # ALSO SAVE TEMP ERROR REPORT REGARDLESS??
  
  # message Y/N move to clean folder? Copy to thumb drive??
  # - need directories for "clean", thumb drive backup, to FTP folder?
  # - also need a message/reminder to double check that the files went where you think they did!!
  
    cat(style_bold("Please pause here and review the errors for Haul ", haul_id, ".\n" ))
    cat("  - If there no corrections or Error Report annotations that need to be made, please select 'Yes' when prompted to move the tablet files to their final folders.\n")
    cat("  - If corrections for Haul ", haul_id, " are needed, or flags need to be annotated in the temporary Error Report, please select 'No'.\n\n", sep = "")
    
    ok_selection <- menu(c("Yes", "No"), title = paste0("Is Haul ", haul_id, " clean and ready to be moved out of the 'QAQC Queue' folder?\n"))
    
    if(ok_selection == 1){
      
      #**WRITE ERROR REPORT HERE* TO FINAL
      
      
      #**MOVE FILES*
        files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
        to_clean <- move_files(files = files_clean,
                               vessel = vessel,
                               leg = leg, 
                               haul_number = haul_number,
                               destination = "clean")
      
      
        cat("\nYou selected 'Yes'.\n\n", sep = "")
        cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 
        
        
        copy_selection <- menu(c("Yes", "No"), title = "Would you like to copy the clean tablet files and final Error Report to the FTP Queue and USB Backup?")
        
        if(copy_selection == 1){
          
          copyFTP <- copy_files(vessel = vessel,
                                leg = leg, 
                                haul_number = haul_number,
                                file_type = "tablet", 
                                destination = "ftp")
          
          copyUSB <- copy_files(vessel = vessel,
                                leg = leg, 
                                haul_number = haul_number,
                                file_type = "tablet", 
                                destination = "backup")
          
          cat("\nYou selected 'Yes'.\n\n", sep = "")
          cat("Copying Haul ", haul_id, " tablet files and final Error Report to the FTP Queue and USB Backup.\n\n", sep = "")
        }
        
        if(copy_selection == 2){
          cat(col_red("You selected 'No'.\n"))
          cat(col_red("Tablet files and the final Error Report for Haul ", haul_id, " have not been backed up. Please make sure to copy these files into the FTP Queue and USB Backup folders.\n\n"))
        }
        
        
      # Move on to next haul?
        if(final_haul == TRUE){
          cat("This is the final haul in the QAQC Queue. Stopping the error checking protocol.\n\n") 
          # More instructions here?? Please validate things, or continue with the final lines of the script or something?
          
          out <- "break" # Do I need this line??
        } 
        
        if(final_haul == FALSE){

            next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul?")
            
            if(next_selection == 1){
              cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n\n\n\n\n")
              
              out <- "next"
            }
            
            if(next_selection == 2){
              cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n\n\n\n\n")
              
              out <- "break"
            }
        }
    }
    
    
    if(ok_selection == 2){
      #**WRITE ERROR REPORT*
      
        cat(col_red("You selected 'No'.\n"))
        cat(col_red("Please pause here and review any errors or flags in the temporary Error Report file for Haul ", haul_id, ".\n\n"))
        cat(col_red("If no data corrections are needed, please:\n"))
        cat(col_red("      (1) Open the temporary Error Report file in the Desktop folder;\n"))
        cat(col_red("      (2) Annotate any flags to provide context;\n"))
        cat(col_red("      (3) Save and close the file;\n"))
        cat(col_red("      (4) Proceed with the prompts in the console.\n\n"))
        
        cat(col_red("If there are corrections that need to be made, please:  and make changes in tablet as needed.\n"))
"  and make changes in tablet as needed."
        cat(col_red("When the errors have been corrected, please put the re-extracted files into the 'QAQC Queue' folder and run them through the error checking protocol again. Don't forget to annotate any changes in the existing temporary Error Report for Haul ", haul_id, " as well.\n\n"))
        
      
      ##**MOVE FILES TO ARCHIVE??*
        archive_selection <- menu(c("Yes", "No"), title = paste0("\nWould you like to Archive the existing tablet files for Haul ", haul_id, " ?\n"))
      
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
          cat(col_red("\nTablet files for Haul ", haul_id, " have not been moved. Please make sure to move these files out of the 'QAQC Queue' folder and into the ", vessel, " ", leg, " 'Archive' folder before running error checks on this haul again.\n\n"))
        }
      
      
      # Move on to next haul?
        
        if(final_haul == TRUE){
          cat("This is the final haul in the QAQC Queue. Stopping the error checking protocol.\n\n") 
          # More instructions here?? Please review any temporary error reports and re-run things as needed
          # before continue with the final lines of the script or something?
          
          out <- "break" # Do I need this line??
        } 
        
        
        if(final_haul == FALSE){
            next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?\n")
            
            if(next_selection == 1){
              cat("\nYou selected 'Yes'.\n")
              cat("\nSaving Haul ", haul_id, " temporary Error Report and starting error checks for the next haul.\n\n", sep = "")
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n\n\n\n\n")
              
              out <- "next"
            }
            
            if(next_selection == 2){
              cat(col_red("\nYou selected 'No'.\n"))
              cat(col_red("\nSaving Haul ", haul_id, " temporary Error Report and stopping the error checking protocol.\n\n"))
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n\n\n\n\n")
              
              out <- "break"
            }
        }
    }
    
  
    
    return(out)
}




# # After checks are complete ----------------------------------------------------
# #
# # summary of any notes associated with haul (e.g. saw rotting clutches, etc.)
# 
# # output summary, say will pop up, check it then verify if good/not
# #   -- how to disregard errors so they don't come up again in recheck....might be too tricky for now?
# cat(paste0("Printing error summary for Haul ", haul_id, "...\n\n" ))
# print(errors)
# 
# # message Y/N move to clean folder? Copy to thumb drive??
# # - need directories for "clean", thumb drive backup, to FTP folder?
# # - also need a message/reminder to double check that the files went where you think they did!!
# 
# cat(paste0("Please review the errors for Haul ", haul_id, ".\n" ))
# cat(paste0("If there is nothing that needs correcting, please select 'Yes' when prompted to output the final Error Report and move the tablet files to their final folders.\n" ))
# cat(paste0("If corrections for Haul ", haul_id, " are needed, please select 'No'.\n\n" ))
# 
# ok_selection <- menu(c("Yes", "No"), title = paste0("Is Haul ", haul_id, " clean and ready to be moved out of the 'QAQC Queue' folder?\n"))
# 
# if(ok_selection == 1){
#   
#   #**WRITE ERROR REPORT HERE* 
#   
#   
#   #**MOVE FILES*
#   files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
#   to_clean <- move_files(files = files_clean,
#                          vessel = vessel,
#                          leg = leg, 
#                          haul_number = haul_number,
#                          # path = path, 
#                          destination = "clean")
#   
#   
#   cat("\nYou selected 'Yes'.\n\n", sep = "")
#   cat("Saving Haul ", haul_id, " final Error Report and moving files to the ", vessel, " ", leg, " folders.\n\n", sep = "") 
#   
#   
#   copy_selection <- menu(c("Yes", "No"), title = "Would you like to copy the clean tablet files and final Error Report to the FTP Queue and USB Backup?")
#   
#   if(copy_selection == 1){
#     
#     copyFTP <- copy_files(vessel = vessel,
#                           leg = leg, 
#                           haul_number = haul_number,
#                           file_type = "tablet", 
#                           # path = path, 
#                           destination = "ftp")
#     
#     copyUSB <- copy_files(vessel = vessel,
#                           leg = leg, 
#                           haul_number = haul_number,
#                           file_type = "tablet", 
#                           # path = path, 
#                           destination = "backup")
#     
#     cat("\nYou selected 'Yes'.\n\n", sep = "")
#     cat("Copying Haul ", haul_id, " tablet files and final Error Report to the ", vessel, " ", leg, " folders in the FTP Queue and USB Backup.\n\n", sep = "")
#   }
#   
#   if(copy_selection == 2){
#     cat(col_red("You selected 'No'.\n"))
#     cat(col_red("Tablet files and the final Error Report for Haul ", haul_id, " have not been backed up. Please make sure to copy these files into the FTP Queue and USB Backup folders.\n\n"))
#   }
#   
#   
#   # Move on to next haul?
#   next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul?")
#   
#   if(next_selection == 1){
#     cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
#     cat(rep("-", getOption("width")), sep = "")
#     cat("\n\n\n\n\n\n")
#     next
#   }
#   
#   if(next_selection == 2){
#     cat(col_red("You selected 'No' - stopping the error checking protocol.\n\n"))
#     cat(rep("-", getOption("width")), sep = "")
#     cat("\n\n\n\n\n\n")
#     break
#   }
# }
# 
# 
# if(ok_selection == 2){
#   #**WRITE ERROR REPORT*
#   
#   cat(col_red("You selected 'No'.\n"))
#   cat(col_red("Please review the errors in the temporary Error Report and make changes in tablet as needed.\n"))
#   cat(col_red("When the errors have been corrected, please put the re-extracted files into the 'QAQC Queue' folder and run them through the error checking protocol again. Don't forget to annotate any changes in the existing temporary Error Report for Haul ", haul_id, " as well.\n\n"))
#   
#   
#   ##**MOVE FILES TO ARCHIVE??*
#   archive_selection <- menu(c("Yes", "No"), title = paste0("\nWould you like to Archive the existing tablet files for Haul ", haul_id, " ?\n"))
#   
#   if(archive_selection == 1){
#     
#     #**NEED something different to archive other files out of the queue...*
#     #*take the 'files_clean' and feed it through the move to archive function?? 
#     # files_clean <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
#     
#     # to_archive <- list.files(in_dir, pattern = archive_timestamps, recursive = TRUE) %>% # ok if multiple timestamps, would need to iterate... probs for multiple tablets too?
#     #               map(~file.rename(from = paste0(in_dir, .x),
#     #                                to = paste0(clean_dir, "_archive/", .x)))
#     
#     cat("\nYou selected 'Yes'.\n")
#     cat("\nTablet files for Haul ", haul_id, " have been moved to the ", vessel, " ", leg, " 'Archive' folder.\n\n", sep = "")
#   }
#   
#   if(archive_selection == 2){
#     cat(col_red("\nYou selected 'No'.\n"))
#     cat(col_red("\nTablet files for Haul ", haul_id, " have not been moved. Please make sure to move these files out of the 'QAQC Queue' folder and into the ", vessel, " ", leg, " 'Archive' folder before running error checks on this haul again.\n\n"))
#   }
#   
#   
#   # Move on to next haul?
#   next_selection <- menu(c("Yes", "No"), title = "Would you like to start error checks for the next haul in the meantime?\n")
#   
#   if(next_selection == 1){
#     cat("\nYou selected 'Yes'.\n")
#     cat("\nSaving Haul ", haul_id, " temporary Error Report and starting error checks for the next haul.\n\n", sep = "")
#     cat(rep("-", getOption("width")), sep = "")
#     cat("\n\n\n\n\n\n")
#     next
#   }
#   
#   if(next_selection == 2){
#     cat(col_red("\nYou selected 'No'.\n"))
#     cat(col_red("\nSaving Haul ", haul_id, " temporary Error Report and stopping the error checking protocol.\n\n"))
#     cat(rep("-", getOption("width")), sep = "")
#     cat("\n\n\n\n\n\n")
#     break
#   }
#   
# }
# 
# 
# 
# 
# # next_selection <- menu(c("Yes", "No"), title = "Would you like to move on to the next haul in the meantime?")
# # 
# # if(next_selection == 1){
# #   #**WRITE ERROR REPORT??* Think about if need 2 error report locations....
# #   #*one in QAQC as temp, and then a final one that gets moved once the haul is "approved"?
# #   cat("\nYou selected 'Yes'.\n\n", sep = "")
# #   cat("Saving Haul ", haul_id, " error report.\n", sep = "") 
# #   cat("Starting error checks for the next haul.\n\n", sep = "")
# #   # cat("You selected 'Yes' - starting error checks for the next haul.\n\n")
# #   next
# # }
# # 
# # if(next_selection == 2){
# #   #**WRITE ERROR REPORT*
# #   
# #   cat(col_red("You selected 'No'.\n"))
# #   cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
# #   break
# # }
# 
# # Y/N move on to next haul?



