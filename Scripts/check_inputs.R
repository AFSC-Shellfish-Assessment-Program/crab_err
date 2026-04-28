# Function to check vessel and leg inputs for all QAQC functions
# - ensures valid inputs so directories are valid

check_inputs <- function(vessel = c("AKK", "NWEx"), 
                         leg = c("Leg1", "Leg2", "Leg3", "Leg4")){
  
  # Set valid inputs -- these may need to be updated every year
  vessels <- c("AKK", "NWEx")
  legs <-  c("Leg1", "Leg2", "Leg3", "Leg4")
  
  
  # Check that the argument `vessel` is one of the correct options,
  # and only has one input of the correct type.
  if(length(x = vessel) > 1){
    stop(paste0("The error checking protocol is designed to only be run for one vessel at",
                " a time. Please limit your query to just one vessel."))
  }
  
  if(!class(vessel) == "character"){
    stop(paste0("The `vessel` argument must be specified as a character. ",
                "Please modify your `vessel` input and try again."))
  }
  
  if(!vessel %in% vessels){ 
    stop(paste0("The `vessel` argument must be one of these options",
                " (case-sensitive): 'AKK', 'NWEx'. Please modify your `vessel` ",
                "input and try again."))
    
  }
  
  
  # Check that the argument `leg` is one of the correct options,
  # and only has one input of the correct type.
  if(length(x = leg) > 1){
    stop(paste0("The error checking protocol is designed to only be run for one leg at",
                " a time. Please limit your query to just one leg."))
  }
  
  if(!class(leg) == "character"){
    stop(paste0("The `leg` argument must be specified as a character. ",
                "Please modify your `leg` input and try again."))
  }
  
  if(!leg %in% legs){
    stop(paste0("The `leg` argument must be one of these options",
                " (case-sensitive): 'Leg1', 'Leg2', 'Leg3', or 'Leg4'. Please modify your ",
                "`leg` input and try again."))
    
  }
}
