
# function to ID zero-catch stations
# - maybe nest this inside the bigger file_ID one?? 
#   - actually census notes files too and then do this check first before doing the group_ID stuff....


# - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
#   as "error report" and moves to next haul
#
# - Also, do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
#  and verify 0-catch and say please add a note??

#**NEED TO UPDATE WITH FUNCTION TO SAVE ERROR REPORT AND MOVE FILES TO CLEAN!!!*

id_zero_catch <- function(files, 
                          errors,
                          vessel,
                          leg,
                          haul_number,
                          path){
  
  
  
  # # ID any 'Notes' files
  #   if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
  #     notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
  #              map_df(~read.csv(paste0(in_dir, .x))) %>%
  #              select(HAUL_ID, NOTE_TABLE, NOTES)
  #   }


  # Set 'haul_id'
    haul_id <- str_sub(haul_number, 2, 4)


  # # Check for only 2 files in the haul with a 'Note'
  #   if(length(files) == 2 & exists("notes")){
      
    # Print message: Was this a 0-catch station? (YES/NO)
      no_catch_selection <- menu(c("Yes", "No"), title = paste0("Was Haul ", haul_id, " a zero-catch station?"))
      
      
    # Select 'YES' this was 0-catch:
      if(no_catch_selection == 1){
        
        # Print message: Move to next haul? (YES/NO)
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
            
              cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " 'Clean' folders.\n", sep = "") 
              cat("Starting error checks for the next haul.\n\n", sep = "")
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n")
            
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
              
              cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " 'Clean' folders.\n", sep = "")
              cat(col_red("Stopping the error checking protocol.\n\n", sep = ""))
              cat(rep("-", getOption("width")), sep = "")
              cat("\n\n")
              
              out <- "break"
          }
          
    # Select 'NO' was a catch:
      } else if(no_catch_selection == 2){
          cat("You selected 'No'.\n\n")
          cat(col_red("No 'CATCH' or 'SPECIMEN' files for Haul ", haul_id, " are present in the queue.\n", sep = ""))
          cat(col_red("Please make sure those files are in the 'QAQC_queue' folder and try again for this haul.\n\n"))
        
        #**PRINT ERROR REPORT*
          error_iter <- nrow(errors) + 1
          errors[error_iter, 1] <- "File"
          errors[error_iter, 2] <- paste0("No 'CATCH' or 'SPECIMEN' files are present in the QAQC queue")
        
        # Print message: Move on to next haul? (YES/NO)
          next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
          
          # Select 'YES' move to next haul:
            if(next_selection == 1){
                cat("You selected 'Yes'.\n")
              
              # SAVE ERROR REPORT
              
              
                cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n")
                
                out <- "next"
            }
            
          # Select 'NO' don't move to next haul:
            if(next_selection == 2){ #**PRINT ERROR REPORT?*
                cat(col_red("You selected 'No'.\n"))
                
              # SAVE ERROR REPORT
              
              
                cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
                cat(rep("-", getOption("width")), sep = "")
                cat("\n\n")
                
                out <- "break"
            }
      } # do I need a final "else" here? Selection not recognized, please run these lines again?
    # }
  
  
  
  # return(list(errors = errors,
  #             out = out))  
      
    return(out)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## ID potential zero-catch station ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#
#**MAKE THIS A FUNCTION TO SOURCE*
#
# - if 0-catch, have a little popup Y/N to confirm 0-catch, if Y, outputs that 
#   as "error report" and moves to next haul
#
# - Also, do a 2nd iteration of this when just RAW_HAUL is present without a note?? 
#  and verify 0-catch and say please add a note??


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## ID any note files ----
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# if(length(list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE)) > 0){
#   notes <- list.files(in_dir, pattern = paste0(haul_number, "_NOTES_"), recursive = TRUE) %>%
#     map_df(~read.csv(paste0(in_dir, .x))) %>%
#     select(HAUL_ID, NOTE_TABLE, NOTES)
# }


# if(length(files) == 2 & exists("notes")){
#   no_catch_selection <- menu(c("Yes", "No"), title = paste0("\nWas Haul ", haul_id, " a zero-catch station?"))
#   
#   if(no_catch_selection == 1){
#     move_selection <- menu(c("Yes", "No"), title = "\nYou selected 'Yes' - would you like to move on to the next haul?")
#     
#     if(move_selection == 1){
#       cat("\nYou selected 'Yes'.\n\n", sep = "")
#       
#       ## OUTPUT ERROR REPORT HERE indicating 0-catch station -- make function for outputting error report and call here!!!!
#       error_iter <- nrow(errors) + 1
#       errors[error_iter, 1] <- "No Catch"
#       errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
#       
#       ## MOVE FILES
#       
#       cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n", sep = "") 
#       cat("Starting error checks for the next haul.\n\n", sep = "")
#       
#       next
#     }
#     
#     if(move_selection == 2){
#       cat("\nYou selected 'No'.\n\n", sep = "")
#       
#       ## OUTPUT ERROR REPORT HERE indicating 0-catch station
#       error_iter <- nrow(errors) + 1
#       errors[error_iter, 1] <- "No Catch"
#       errors[error_iter, 2] <- paste0("Haul ", haul_id, " is a zero-catch station")
#       
#       ## MOVE FILES
#       
#       cat("Saving Haul ", haul_id, " error report and moving files to the ", vessel, " ", leg, " folders.\n", sep = "")
#       cat(col_red("Stopping the error checking protocol.\n\n", sep = ""))
#       
#       break
#     }
#   } else if(no_catch_selection == 2){
#     cat("You selected 'No'.\n\n")
#     cat(col_red("No 'CATCH' or 'SPECIMEN' files for Haul ", haul_id, " are present in the queue.\n", sep = ""))
#     cat(col_red("Please make sure those files are in the 'QAQC_queue' folder and try again for this haul.\n\n"))
#     
#     #**PRINT ERROR REPORT*
#     error_iter <- nrow(errors) + 1
#     errors[error_iter, 1] <- "File"
#     errors[error_iter, 2] <- paste0("No 'CATCH' or 'SPECIMEN' files are present in the QAQC queue")
#     
#     next_selection <- menu(c("Yes", "No"), title = "\nWould you like to move on to the next haul in the meantime?")
#     
#     if(next_selection == 1){
#       cat("You selected 'Yes'.\n")
#       cat("Saving Haul", haul_id, "error report and starting error checks for the next haul.\n\n")
#       next
#     }
#     
#     if(next_selection == 2){ #**PRINT ERROR REPORT?*
#       cat(col_red("You selected 'No'.\n"))
#       cat(col_red("Saving Haul ", haul_id, " error report and stopping the error checking protocol.\n\n"))
#       break
#     }
#   } # do I need a final "else" here? Selection not recognized, please run these lines again?
# }