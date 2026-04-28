

# Function to check for/read in existing error report, create template if none, 
# and set up iterator for this round 


report_setup <- function(path,
                         vessel,
                         leg,
                         haul_number){
  
  # call input check function....
  # - do we need something to validate directory path?? I suppose it would throw an error if the dir didn't exist when trying to read/write....  
  
  
  ##**READ IN/CHECK FOR ERROR REPORT HERE*
  
  ##  - Make note of if one exists, then have a "RE-CHECK" indicated in the file??
  ##  - MAKE NOTE IN PROTOCOL --> please don't rename things!!!!
  ##  - Will also need to collect some other haul/time info for report output...come up with header format

  
  # Set clean directory
    clean_dir <- paste0(path, vessel, "/", leg, "/")
    
  # Create haul ID for output file naming
    haul_id <- str_remove(haul_number, "^0+")
  
  # Read in existing report file (if any)
    report_file <- list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))
    
    
    if(length(report_file) == 1){
      # Read in existing error report file
        report <- read.delim(paste0(clean_dir, "_error_reports/", report_file)) 
      
      #**...then what...create "recheck" header for appending...*
      
      # Print message 
        cat("A prior Error Report is detected for Haul ", haul_id, ".\n", sep = "")
        cat("Reading in the existing file and setting up the 'Recheck' header.\n\n", sep = "")
      
    } else if(length(report_file) == 0){
      
      #**DO SOMETHING HERE...*
      # if it doesn't exist already, create new file....
      # need some standard things here for header etc. 
      # How do delim line breaks, blank lines, etc? can we preserve those with output/input through R?
        report <- nullfile()
      
      # Print message 
        cat("Setting up Error Report file for Haul ", haul_id, ".\n\n", sep = "")
      
    } else{
      # THROW ERROR - multiple files for same haul?? go back and look/reconcile....
      #**do I want to provide an option to move on to the next haul in the meantime??*
        cat(col_red("Multiple Error Report files were detected for Haul ", haul_id, ". \n   To correct this, please:\n"))
        cat(col_red("      (1) Review the files in the ", vessel, " ", leg, " 'Error Reports' folder;\n"))
        cat(col_red("      (2) Reconcile any differences and combine into one comprehensive Error Report file;\n"))
        cat(col_red("      (3) Delete the extra file(s), and try again.\n\n"))
        cat(rep("-", getOption("width")), sep = "")
        cat("\n\n")
        # break()
    }
    
  
  return(report) #maybe just have the error iterator created in regular workflow/master function??
}




# # call input check function....
# # - do we need something to validate directory path?? I suppose it would throw an error if the dir didn't exist when trying to read/write....  
# 
#   
#   
# # ##**CREATE ERROR VECTOR/ITERATOR FOR HAUL*
# # errors <- data.frame(ERROR_TYPE = character(),
# #                      ERROR_MESSAGE = character(),
# #                      stringsAsFactors = FALSE)
# 
# 
# ##**READ IN/CHECK FOR ERROR REPORT HERE*
# 
# ##  - Make note of if one exists, then have a "RE-CHECK" indicated in the file??
# ##  - MAKE NOTE IN PROTOCOL --> please don't rename things!!!!
# ##  - Will also need to collect some other haul/time info for report output...come up with header format
# # report_file <- list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul113"))
# report_file <- list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", str_sub(haul_number, 2, 4)))
# 
# 
# if(length(report_file) == 1){
#   # read in existing error report file
#   report <- read.delim(paste0(clean_dir, "_error_reports/", report_file)) 
#   
#   #...then what...create "recheck" header for appending...
#   
# } else if(length(report_file) == 0){
#   
#   #**DO SOMETHING HERE...*
#   # if it doesn't exist already, create new file....
#   # need some standard things here for header etc. 
#   # How do delim line breaks, blank lines, etc? can we preserve those with output/input through R?
#   
# } else{
#   # THROW ERROR - multiple files for same haul?? go back and look/reconcile....
#   #**do I want to provide an option to move on to the next haul in the meantime??*
#   cat(col_red("Multiple error report files were detected for Haul ", haul_id, ". \n   To correct this, please:\n"))
#   cat(col_red("      (1) Review the files in the ", vessel, " ", leg, " 'Error Reports' folder;\n"))
#   cat(col_red("      (2) Reconcile any differences and combine into one comprehensive error report file;\n"))
#   cat(col_red("      (3) Delete the extra file(s), and try again.\n\n"))
#   break()
# }
