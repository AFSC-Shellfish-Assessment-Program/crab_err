
# file_checks_global


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
  
  
  
  #**popup to verify # and which hauls to be checked?? Ie. there are 4 hauls to be checked, correct?*
  #  and if no, put a reminder to go back and make sure all files are there....
  #  - something to look back/ID any sequential haul #s missing and verify that those are bad hauls?
  #    (or maybe that's something I can do on my end with the temp haul file...)
  # Option for "recheck" if made changes, and wouldn't run this check?
  
    haul_queue <- unique(haul_info_all$HAUL_NUMBER)
    
    if(length(haul_queue) == 0){
        cat(col_red("\nNo hauls were identified in the QAQC queue. Please go back and make sure the tablet files are in the appropriate folder.\n"))
    } else{
        cat(paste0("\nThe following ", length(haul_queue), " hauls are present in the QAQC queue:\n"))
        for(n in 1:length(haul_queue)){
            cat(paste0("   - Haul ", haul_queue[n], "\n"))
        }
        
        queue_check <- menu(c("Yes", "No"), title = "\nDoes this look correct?")
        
        if(queue_check == 1){
            cat("\nYou selected 'Yes'.\nProceeding with the error checking protocol.\n\n") #**maybe update this wording a bit?*
        }
        
        if(queue_check == 2){
            cat("\nYou selected 'No'.\n\n", sep = "")
            cat(col_red("Stopping the error checking protocol.\n", sep = ""))
            cat(col_red("Please review the files in the QAQC queue folder and ensure that the files for all intended hauls are present.\n\n"))
            cat(rep("-", getOption("width")), sep = "")
            cat("\n\n")
            
            # break() #**something different to stop the flow?*
        }
    }
  
  
    return(haul_info_all) #anything else need to carry through??
}

