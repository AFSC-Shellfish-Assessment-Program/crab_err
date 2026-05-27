
# Function: file_checks_global -------------------------------------------------
#
# Purpose: To detect all haul numbers present in the QAQC Queue and have the user
#          verify that these are the correct hauls to be checked. Gives the user 
#          an opportunity to recognize that files for a certain haul have not been 
#          either extracted from the tablet, copied to the Survey Laptop, or added
#          to the 'QAQC Queue' folder on the Desktop.
#          The `haul_info_all` object is then passed to the haul checking functions
#          in the subsequent lines of the script workflow.
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --


file_checks_global <- function(path){
  
  # Read in all files and pull distinct haul number, tablet, and timestamp combinations
    haul_info_all <- data.frame(FILE = list.files(path, pattern = ".csv", recursive = TRUE)) %>%
                     separate_wider_delim(FILE, 
                                          delim = regex("(_HAUL|_CRAB_SPECIMEN_|_CRAB_CATCH_)"), 
                                          names = c("TABLET", "HAUL_NUMBER"),
                                          too_many = "merge") %>%
                     mutate(DATETIME = str_sub(HAUL_NUMBER, -12, -5),
                            HAUL_NUMBER = str_sub(HAUL_NUMBER, 1, 4)) %>%
                     distinct() %>%
                     arrange(HAUL_NUMBER, TABLET, DATETIME)
  
  # Identify all unique haul numbers in the queue
    haul_queue <- unique(haul_info_all$HAUL_NUMBER)
    
    
  # Print message confirming the hauls detected in the queue  
    if(length(haul_queue) == 0){
        cat(col_red("\nNo hauls were identified in the QAQC Queue. Please go back and make sure the tablet files are in the appropriate folder.\n"))
    } else{
        cat(paste0("\nThe following ", length(haul_queue), " hauls are present in the QAQC Queue:\n"))
        for(n in 1:length(haul_queue)){
            cat(paste0("   - Haul ", haul_queue[n], "\n"))
        }
        
        queue_check <- menu(c("Yes", "No"), title = "\nDoes this look correct?")
        
        if(queue_check == 1){
            cat("\nYou selected 'Yes'.\nPlease proceed with the error checks.\n\n") 
        }
        
        if(queue_check == 2){
            cat("\nYou selected 'No'.\n\n", sep = "")
            cat(col_red("Stopping the error checks.\n", sep = ""))
            cat(col_red("Please review the files in the 'QAQC Queue' folder and ensure that the files for all intended hauls are present.\n\n"))
            cat(rep("-", getOption("width")), sep = "")
            cat("\n\n\n\n\n\n")
        }
    }
  
    return(haul_info_all)
}

