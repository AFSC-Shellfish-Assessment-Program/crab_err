

# Function to write error report
report_save <- function(vessel,
                        leg, 
                        haul_number,
                        errors,
                        type, # temporary vs. final
                        report, 
                        report_date,
                        report_time,
                        recheck){
  
  # Run input checks
    check_inputs(vessel = vessel, 
                 leg = leg,
                 recorder = NULL,
                 message = FALSE)
  
  
  # Check that the argument `type` is one of the correct options,
  # and only has one input of the correct type.
    types <-  c("temporary", "final")
  
    if(length(x = type) > 1){
      stop(paste0("Please limit your input to just one report `type`."))
    }
    
    if(!class(type) == "character"){
      stop(paste0("The `type` argument must be specified as a character. ",
                  "Please modify your `type` input and try again."))
    }
    
    if(!type %in% types){ 
      stop(paste0("The `type` argument must be one of the following options",
                  " (case-sensitive): 'temporary', 'final'. Please modify your report `type` ",
                  "input and try again."))
    }
    
  
  # Set directories
    path <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop"), winslash = "/")
    # in_dir <- paste0(path, "/QAQC Queue/")
    clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/")
  
    
    
  # Format report
    if(!is.null(report)){
      report_out <- rbind(report, errors)
    } else{
      report_out <- errors
    }
    
    # some other sorting/formatting here....
    report_out <- report_out #%>%

   
  # Save report  
    if(type == "temporary"){
      
    }
    
    if(type == "final"){
      write.csv(report_out, paste0(clean_dir))
    }
    
    
    
    
}




# Function to check for/read in existing error report, create template if none, 
# and set up iterator for this round 


report_check <- function(vessel,
                         leg,
                         haul_number){
  
  # call input check function....
  # - do we need something to validate directory path?? I suppose it would throw an error if the dir didn't exist when trying to read/write....  
  
  
  ##**READ IN/CHECK FOR ERROR REPORT HERE*
  
  ##  - Make note of if one exists, then have a "RE-CHECK" indicated in the file??
  ##  - MAKE NOTE IN PROTOCOL --> please don't rename things!!!!
  ##  - Will also need to collect some other haul/time info for report output...come up with header format

  
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
      
      # stop(cat("An Error Report for Haul ", haul_id, " was found in the 'clean' directory. Please go back and review this file.\n\nIf you are re-checking this haul after designating it 'clean', please move the Error Report file back to the 'Temporary Error Reports' folder on the Desktop and try again for this haul.\n\n\n\n"))
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
      
      #**...then what...create "recheck" header for appending...*
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
                    recheck = recheck))
    } 
    
  # If no report anywhere, create new metadata
    if((length(report_file_temp) & length(report_file_clean)) == 0){
      
      # Set metadata for error report
        report_date <- Sys.Date()
        report_time <- format(Sys.time(), "%T")
        recheck <- FALSE
      
      # #**DO SOMETHING HERE...*
      # # if it doesn't exist already, create new file....
      # # need some standard things here for header etc. 
      # # How do delim line breaks, blank lines, etc? can we preserve those with output/input through R?
      #   report <- nullfile()
      # 
      # # Print message 
      #   cat("Setting up Error Report file for Haul ", haul_id, ".\n\n", sep = "")
    
      # Return a list of report metadata
        return(list(report = c(),
                    report_date = report_date,
                    report_time = report_time,
                    recheck = recheck))
    } 
    
  # If multiple temporary files, throw error to reconcile....
    if(length(report_file_temp) > 1){
      # THROW ERROR - multiple files for same haul?? go back and look/reconcile....
      #**do I want to provide an option to move on to the next haul in the meantime??*
        cat(col_red("Multiple temporary Error Report files were detected for Haul ", haul_id, ".\n   To correct this, please:\n"))
        cat(col_red("      (1) Review the files in the 'Temporary Error Reports' folder on the Desktop;\n"))
        cat(col_red("      (2) Reconcile any differences and combine into one comprehensive Error Report file;\n"))
        cat(col_red("      (3) Delete the extra file(s), and try again.\n\n"))
        cat(rep("-", getOption("width")), sep = "")
        cat("\n\n\n\n\n\n")
        
        out <- "break"
        return(out)
    }
    
  
  # return(report) #maybe just have the error iterator created in regular workflow/master function??
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
