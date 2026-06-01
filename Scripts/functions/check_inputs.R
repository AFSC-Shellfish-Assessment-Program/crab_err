
# Function: check_inputs -------------------------------------------------------
#
# Purpose: Function to check vessel, leg, and recorder inputs for QAQC functions.
#          Ensures valid inputs so metadata and folder directories are correct.
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --


check_inputs <- function(metadata,
                         message){
  
  # Unpack metadata
    vessel <- metadata$vessel
    leg <- metadata$leg
    recorder <- metadata$recorder
  
  # Set valid inputs -- these need to be updated every year
    vessels <- c("AKK", "NWEx")
    legs <-  c("Leg1", "Leg2", "Leg3", "Leg4")
  
  
  # Check that the `vessel` argument is one of the correct options,
  # and only has one input of the correct type.
    if(length(x = vessel) > 1){
      stop(paste0("The error checks are designed to only be run for one vessel at",
                  " a time. Please limit your query to just one vessel."))
    }
    
    if(!class(vessel) == "character"){
      stop(paste0("The `vessel` argument must be specified as a character. ",
                  "Please modify your `vessel` input and try again."))
    }
    
    if(!vessel %in% vessels){ 
      stop(paste0("The `vessel` argument must be one of the following options",
                  " (case-sensitive): 'AKK', 'NWEx'. Please modify your `vessel` ",
                  "input and try again."))
      
    }
  
  
  # Check that the `leg` argument is one of the correct options,
  # and only has one input of the correct type.
    if(length(x = leg) > 1){
      stop(paste0("The error checks are designed to only be run for one leg at",
                  " a time. Please limit your query to just one leg."))
    }
    
    if(!class(leg) == "character"){
      stop(paste0("The `leg` argument must be specified as a character. ",
                  "Please modify your `leg` input and try again."))
    }
    
    if(!leg %in% legs){
      stop(paste0("The `leg` argument must be one of the following options",
                  " (case-sensitive): 'Leg1', 'Leg2', 'Leg3', or 'Leg4'. Please modify your ",
                  "`leg` input and try again."))
      
    }
  
  
  # Check that the `recorder` argument is the correct class
    if(!is.null(recorder)){
      if(!class(recorder) == "character"){
        stop(paste0("The `recorder` argument must be specified as a character. ",
                    "Please modify your `recorder` input and try again."))
      } 
    }
    
    if(message == TRUE){
      return(cat("\nKey inputs have been validated.\n\n"))
    }
}
