
# file_checks_global


file_checks_global <- function(path){
  
  
  in_dir <- paste0(path, "QAQC_queue/")
  
  haul_info_all <- data.frame(FILE = list.files(in_dir, pattern = ".csv", recursive = TRUE)) %>%
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






# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# # Start global file checks ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# ## compile temporary/placeholder "haul" file? ----
# # - ID 0-catch stations
# # - where do I want to draw this from?? just the files in the queue? or all hauls so far?
# 
# 
# ##**Might need to do a special file check here first!* 
# ##**inventory all files in queue and make sure a timestamp doesn't change with extraction variation....*
# ##  - if tablet and haul number are same, and list # of files that matches isn't complete, 
# ##     see if there's a complementary timestamp for that haul that makes the set....
# ##  - might add complication to moving files, but we'll see? I think at end, can just move by haul# and don't need timestamp
# 
# 
# # Read in all files first to make list of which hauls to be checked... 
# # - extract haul #, tablet, and timestamp for use down the line....
# # haul_info_all <- data.frame(FILE = list.files(in_dir, pattern = "_HAUL_", recursive = TRUE)) %>%
# #                  separate_wider_delim(FILE, "_HAUL", names = c("TABLET", "HAUL_NUMBER", "DATETIME")) %>%
# #                  mutate(DATETIME = str_sub(DATETIME, 2, 9),
# #                         TABLET_STR = str_remove(str_sub(TABLET, 1, 6), "_$")) %>%
# #                  # join with all unique tablet/timestamp combos in queue files to verify have a haul file for them all...
# #                  # (which should be present for every good haul)
# #                  full_join(., data.frame(FILE = list.files(in_dir, recursive = TRUE)) %>%
# #                                  mutate(TABLET_STR = str_remove(str_sub(FILE, 1, 6), "_$"),
# #                                         DATETIME = str_sub(FILE, -12, -5)) %>%
# #                                  select(-FILE) %>%
# #                                  distinct(),
# #                            by = join_by(DATETIME, TABLET_STR)) %>%
# #                  # order by haul#, tablet, timestamp:
# #                  mutate(haul_temp = as.numeric(HAUL_NUMBER),
# #                         datetime_temp = as.numeric(DATETIME)) %>%
# #                  arrange(haul_temp, TABLET, datetime_temp) %>%
# #                  select(-haul_temp, -datetime_temp)
# 
# haul_info_all <- data.frame(FILE = list.files(in_dir, pattern = ".csv", recursive = TRUE)) %>%
#   separate_wider_delim(FILE, 
#                        delim = regex("(_HAUL|_CRAB_SPECIMEN_|_CRAB_CATCH_)"), 
#                        names = c("TABLET", "HAUL_NUMBER"),
#                        too_many = "merge") %>%
#   mutate(DATETIME = str_sub(HAUL_NUMBER, -12, -5),
#          HAUL_NUMBER = str_sub(HAUL_NUMBER, 1, 4)) %>%
#   distinct() %>%
#   arrange(HAUL_NUMBER, TABLET, DATETIME)
# 
# 
# 
# #**popup to verify # and which hauls to be checked?? Ie. there are 4 hauls to be checked, correct?*
# #  and if no, put a reminder to go back and make sure all files are there....
# #  - something to look back/ID any sequential haul #s missing and verify that those are bad hauls?
# #    (or maybe that's something I can do on my end with the temp haul file...)
# # Option for "recheck" if made changes, and wouldn't run this check?
# 
# haul_queue <- unique(haul_info_all$HAUL_NUMBER)
# 
# if(length(haul_queue) == 0){
#   cat(col_red("\nNo hauls were identified in the QAQC queue. Please go back and make sure the tablet files are in the appropriate folder.\n"))
# } else{
#   cat(paste0("\nThe following ", length(haul_queue), " hauls are present in the QAQC queue:\n"))
#   for(n in 1:length(haul_queue)){
#     cat(paste0("   - Haul ", haul_queue[n], "\n"))
#   }
#   
#   queue_check <- menu(c("Yes", "No"), title = "\nDoes this look correct?")
#   
#   if(queue_check == 1){
#     cat("\nYou selected 'Yes'.\nProceeding with the error checking protocol.\n\n") #**maybe update this wording a bit?*
#   }
#   
#   if(queue_check == 2){
#     cat("\nYou selected 'No'.\n\n", sep = "")
#     cat(col_red("Stopping the error checking protocol.\n", sep = ""))
#     cat(col_red("Please review the files in the QAQC queue folder and ensure that the files for all intended hauls are present.\n\n"))
#     
#     break() #**something different to stop the flow?*
#   }
# }
# 
# 
