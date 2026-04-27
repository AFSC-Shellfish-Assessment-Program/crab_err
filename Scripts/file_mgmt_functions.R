# Other file management functions (move, copy, compile):

# move_to_clean()
move_to_clean <- function(files, 
                          haul_number, 
                          in_dir, 
                          clean_dir){
  
  files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
  
  # Move CRAB_CATCH files
    files[grepl(paste0("_CRAB_CATCH_", haul_number), files)] %>%
      map(~file.rename(from = paste0(in_dir, .x),
                       to = paste0(clean_dir, "Crab CATCH Files/", .x)))
    
  # Move CRAB_SPECIMEN files
    files[grepl(paste0("_CRAB_SPECIMEN_", haul_number), files)] %>%
                      map(~file.rename(from = paste0(in_dir, .x),
                                       to = paste0(clean_dir, "Crab SPECIMEN Files/", .x)))
  
  # Move RAW files
    files[grepl(paste0("_HAUL", haul_number), files)] %>%
      map(~file.rename(from = paste0(in_dir, .x),
                       to = paste0(clean_dir, "RAW Haul Files/", .x)))
   
  # # Move Error Report file
  #   list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))%>%
  #     map(~file.rename(from = paste0(in_dir, .x),
  #                      to = paste0(clean_dir, "RAW Haul Files/", .x)))
  
  return()
}


## different actions/locations:
#  - move to archive
#    - also copy to sFTP/backup archive??
#    - catch, specimen, raw?
#  - move to clean
#    - catch, specimen, raw
#  - copy to sFTP
#    - need to make directories if don't exist
#    - catch, specimen, raw, error, db, archive?
#  - copy to USB backup
#    - catch, specimen, raw, error, db, archive?
#  - compile db files...
#
# error, db, and archive automatically get written in clean_dir...
#
#
# so if we had a function that you specify "move" or "copy", and the location,
# we can have multiple if statements to set the directories and deal with the appropriate
# files, folder creation, etc. 

move_files <- function(files, # feed function a list of files (either ALL files for clean, or files for archive)
                       vessel,
                       leg, 
                       haul_number,
                       path, 
                       destination = c("clean", "archive")){
  
  # files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
  
  
  # Set directories
    in_dir <- paste0(path, "QAQC_queue/")
    
    if(destination == "clean"){
      clean_dir <- paste0(path, vessel, "/", leg, "/")
    }

    if(destination == "archive"){
      clean_dir <- paste0(path, vessel, "/", leg, "/_archive/")
      # sftp_dir <- paste0(path, "to_sFTP/", vessel, "/", leg, "/_archive/")
      # backup_dir <- paste0("D:/", vessel, "/", leg, "/_archive/")
    }
    
  
  # Move CRAB_CATCH files
    catch <- files[grepl(paste0("_CRAB_CATCH_", haul_number), files)] %>%
             map(~file.rename(from = paste0(in_dir, .x),
                              to = paste0(clean_dir, "Crab CATCH Files/", .x)))
  
  # Move CRAB_SPECIMEN files
    specimen <- files[grepl(paste0("_CRAB_SPECIMEN_", haul_number), files)] %>%
                map(~file.rename(from = paste0(in_dir, .x),
                                 to = paste0(clean_dir, "Crab SPECIMEN Files/", .x)))
  
  # Move RAW files
    raw <- files[grepl(paste0("_HAUL", haul_number), files)] %>%
           map(~file.rename(from = paste0(in_dir, .x),
                            to = paste0(clean_dir, "RAW Haul Files/", .x)))
  
  # # Move Error Report file
  #   list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))%>%
  #     map(~file.rename(from = paste0(in_dir, .x),
  #                      to = paste0(clean_dir, "RAW Haul Files/", .x)))
  
  return()
}




# files = to_archive

copy_files <- function(files, # specify exactly which files we're applying this to, and then if it's ever to archive, then that'll also copy to sFTP and USB?
                       vessel,
                       leg, 
                       haul_number,
                       file_type = c("tablet", "archive", "db"), # "tablet" includes error reports, "archive" goes straight from queue not clean (must be used before archive files are moved)
                       path, # overall root path, can have in/clean_dir specified here...
                       destination = c("sftp", "backup")){ #action = c("move", "copy")
  
  # Set directories
    # in_dir <- paste0(path, "QAQC_queue/")
    clean_dir <- paste0(path, vessel, "/", leg, "/")
    # sftp_dir <- paste0(path, "to_sFTP/", vessel, "/", leg, "/")
    # backup_dir <- paste0("D:/", vessel, "/", leg, "/")
  
    
  # write checks for proper destinations, file_types....
  # - only 1 input for each
  # - make sure 'files' is a list....
  # - must have 'haul_number' if file_type == "tablet" or "archive"
    

  # Copy to sFTP or USB Backup
  # - need to make directories if don't exist (sFTP only)
  # - catch, specimen, raw, error, db, archive?

  # Set destination path
    if(destination == "sftp"){
      # Set base path
        dest_path <- paste0(path, "to_sFTP/") 
        
      # Create vessel and leg subfolders if don't already exist 
        dir.create(file.path(dest_path, vessel, leg), recursive = TRUE, showWarnings = FALSE)
      
      # Update with full path
        dest_path <- paste0(path, "to_sFTP/", vessel, "/", leg, "/") 
    }
    
    if(destination == "backup"){
      # dest_path <- paste0("D:/", vessel, "/", leg, "/") 
      dest_path <- paste0(path, "USB_backup/", vessel, "/", leg, "/") 
    }
    
    

    if(file_type == "tablet"){
      
      # Set destination subfolders
        folders <- c("Crab CATCH Files/", 
                     "Crab SPECIMEN Files/",
                     "RAW Haul Files/",
                     "_error_reports/")
      
      # Loop over folder pathways
        for(i in 1:length(folders)){
          
        # For files that go into subfolders:
          if(folders[i] %in% c("Crab CATCH Files/", "Crab SPECIMEN Files/", "RAW Haul Files/", "_error_reports/")){
            
          # Create subfolders if don't already exist 
            if(destination == "sftp"){
              dir.create(file.path(dest_path, folders[i]), recursive = TRUE, showWarnings = FALSE)
            }
            
            
          # ID "pattern" based on file category
            if(folders[i] %in% c("Crab CATCH Files/", "Crab SPECIMEN Files/", "RAW Haul Files/")){
              pattern <- haul_number
            } 
            
            if(folders[i] == "_error_reports/"){
              pattern <- paste0(vessel, "_", leg, "_Haul", haul_id)
            } 
            
  
          # Copy files
            copy <- list.files(paste0(clean_dir, folders[i]), pattern = pattern) %>%
                    map(~file.copy(from = paste0(clean_dir, folders[i], .x),
                                   to = paste0(dest_path, folders[i], .x),
                                   overwrite = TRUE))
          }
       }
    }

      
    
    if(file_type == "archive"){
      # Set directories
        in_dir <- paste0(path, "QAQC_queue/")
        dest_path <- paste0(dest_path, "_archive/")
      
      # Set destination subfolders
        folders <- c("Crab CATCH Files/", 
                     "Crab SPECIMEN Files/",
                     "RAW Haul Files/")
        
        types <- c("_CRAB_CATCH_",
                   "_CRAB_SPECIMEN_",
                   "_HAUL")
                   
        # Loop over folder pathways
          for(i in 1:length(folders)){
              
          # Create subfolders if don't already exist 
            if(destination == "sftp"){
              dir.create(file.path(dest_path, folders[i]), showWarnings = FALSE)
            }
              
          # ID file pattern based on type
            copy <- files[grepl(paste0(types[i], haul_number), files)] %>%
                    map(~file.copy(from = paste0(in_dir, .x),
                                   to = paste0(dest_path, folders[i], .x),
                                   overwrite = TRUE))
  
          }
    }
    
    
    if(file_type == "db"){

      # Copy db files
        copy <- list.files(clean_dir, pattern = "_db.csv") %>%
                map(~file.copy(from = paste0(clean_dir, .x),
                               to = paste0(dest_path, .x),
                               overwrite = TRUE))
    }
}


  
# compile_db_files() 
#**MAKE a pre-check to make sure there are no duplicate files in the clean!!*
# - only check duplicate timestamps for haul/tablet, might have 2 tablets for a haul still...
compile_db_files <- function(vessel,
                             leg, 
                             path){
  
  
  # Set directory to source clean files from
    clean_dir <- paste0(path, vessel, "/", leg, "/")
  
  
  # Read in, combine, and save clean 'Crab Specimen' files
    specimen_db <- list.files(paste0(clean_dir, "Crab SPECIMEN Files/"), pattern = paste0("_CRAB_SPECIMEN_"), recursive = TRUE) %>%
                   map_df(~read.csv(paste0(clean_dir, "Crab SPECIMEN Files/", .x))) %>%
                   write.csv(., paste0(clean_dir, "SPECIMEN_db.csv"), row.names = FALSE)
  
  
  # Read in, combine, and save clean 'Crab Catch' files
    catch_db <- list.files(paste0(clean_dir, "Crab CATCH Files/"), pattern = paste0("_CRAB_CATCH_"), recursive = TRUE) %>%
                map_df(~read.csv(paste0(clean_dir, "Crab CATCH Files/", .x))) %>%
                # group_by(VESSEL, CRUISE, HAUL, STATION, COMMON_NAME, SPECIES_CODE) %>%
                # # combine weights and catch numbers by species (if 2 tablets were used for the haul)
                # summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE),
                #           NUMBER_CRAB = sum(NUMBER_CRAB, na.rm = TRUE), 
                #           .groups = "drop_last") %>%
                # # summarize catch numbers by species from specimen table and update catch numbers 
                # # if there were rounding discrepancies from using 2 tablets 
                # # (ie. tablet rounds to whole numbers but if the catch was split, 0.4 and 0.4 round down, but 0.8 rounds up)
                # left_join(., specimen_db %>%
                #              group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
                #              summarise(CATCH = sum(SAMPLING_FACTOR)),
                #           by = join_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE)) %>%
                # mutate(NUMBER_CRAB = ifelse(CATCH > NUMBER_CRAB, round(CATCH), NUMBER_CRAB)) %>% # will the specimen #s always be larger??
                # # and really should only be off by 1, right?? hmm think about more
                # select(-CATCH) %>%
                write.csv(., paste0(clean_dir, "CATCH_db.csv"), row.names = FALSE)
                #**.^^ DO WE WANT THIS AUTOMATICALLY COMBINED HERE? ^^.* Would have to drop the HAUL_ID, RECORDING_DEVICE, and ID columns...
    
  # Print message confirming db files have been updated
    cat("The 'CATCH_db.csv' and 'SPECIMEN_db.csv' files have been successfully updated in the ", vessel, " ", leg, " folder with all available data for cleaned hauls.\n", sep = "")
    
    
  # Copy the db files to the sFTP queue and the USB backup
    copyFTP <- copy_files(vessel = vessel,
                          leg = leg, 
                          file_type = "db", 
                          path = path, 
                          destination = "sftp")
    
    copyUSB <- copy_files(vessel = vessel,
                          leg = leg, 
                          file_type = "db", 
                          path = path, 
                          destination = "backup")
    
    
  # Print message confirming db files have been copied to the sFTP queue and the USB backup
    cat("These files have also been copied to the sFTP queue and the ", vessel, " ", leg, " USB backup.\n\n", sep = "")
}
  
  




  
# # move_to_archive()  # vessel, leg
# #**also make a standalone version (or option?) to call `haul_id` and move that to archive after the fact??
# # - think about the archive backup too....harder to move to 'backup' if already in the archive...unless we just overwrite?
# #   - also a thing for FTPing....maybe it just automatically moves to all 3 locations if an action is done??
# #     (ie. if want to archive, it goes to all 3 locations....)
# 
# 
# # copy_to_sFTP() - maybe just for clean tablet files, final haul reports, and db files?? 
# 
# 
# 
# 
# # copy_to_USB()
# copy_to_USB <- function(){
#   
#   sftp_dir <- paste0(path, "to_sFTP/", vessel, "/", leg, "/") # files for sFTP - clean tablet files, error reports, archive??
#   #**is there a way to check for/create the "Crab CATCH Files", "Crab SPECIMEN Files", "RAW Haul Files" folders if they don't exist?
#   #*also need to rewrite db files into here too....
#   backup_dir <- paste0("D:/", vessel, "/", leg, "/") # thumb drive backup, only clean files/error reports
#   
#   files <- list.files(clean_dir, pattern = haul_number, recursive = TRUE)
#   
#   # Create subfolders if don't already exist:
#   dir.create(file.path(mainDir, subDir))
#   
#   # Copy CRAB_CATCH files
#   list.files(paste0(clean_dir, "Crab CATCH Files/"), pattern = haul_number) %>%
#     map(~file.copy(from = paste0(clean_dir, "Crab CATCH Files/", .x),
#                    to = paste0(sftp_dir, "Crab CATCH Files/", .x),
#                    overwrite = TRUE))
#   
#   # Copy CRAB_SPECIMEN files
#   list.files(paste0(clean_dir, "Crab SPECIMEN Files/"), pattern = haul_number) %>%
#     map(~file.copy(from = paste0(clean_dir, "Crab SPECIMEN Files/", .x),
#                    to = paste0(sftp_dir, "Crab SPECIMEN Files/", .x),
#                    overwrite = TRUE))
#   
#   # Copy RAW files
#   list.files(paste0(clean_dir, "RAW Haul Files/"), pattern = haul_number) %>%
#     map(~file.copy(from = paste0(clean_dir, "RAW Haul Files/", .x),
#                    to = paste0(sftp_dir, "RAW Haul Files/", .x),
#                    overwrite = TRUE))
#   
#   # Copy Error Report file
#     list.files(paste0(clean_dir, "_error_reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))%>%
#       map(~file.copy(from = paste0(clean_dir, "_error_reports/", .x),
#                      to = paste0(sftp_dir, "_error_reports/", .x),
#                      overwrite = TRUE))
#   
#   # Copy db files
#     list.files(clean_dir, pattern = "_db.csv") %>%
#       map(~file.copy(from = paste0(clean_dir, .x),
#                      to = paste0(sftp_dir, .x),
#                      overwrite = TRUE))
#     
#   # Copy Archive files??
#   # might have to think about a different path for these....send each time something's archived??
#   
# }
# 
# 










#_____________________________________
# Move to Clean
#   - catch, specimen, raw
#
# Move to Archive
#   - also always copy to sFTP/backup archive??
#     - would need to write folders in sFTP....
#   - catch, specimen, raw



if(action == "move"){
  
  # Set destination path
  if(destination == "clean"){
    dest_path <- paste0(path, vessel, "/", leg, "/") 
  }
  
  if(destination == "archive"){
    dest_path <- paste0(path, vessel, "/", leg, "/_archive/")
  }
  
  # Set destination subfolders
  folders <- c("Crab CATCH Files/", 
               "Crab SPECIMEN Files/",
               "RAW Haul Files/")
  
  
  # # Get list of files
  #**NO, need to set these**
  #   files <- list.files(in_dir, pattern = haul_number, recursive = TRUE)
  
  
  # Loop over folder pathways
  for(i in 1:length(folders)){
    
    # Move files into the appropriate folder
    list.files(paste0(clean_dir, folders[i]), pattern = haul_number) %>%
      map(~file.copy(from = paste0(clean_dir, folders[i], .x),
                     to = paste0(dest_path, folders[i], .x),
                     overwrite = TRUE))
    
    
    
    # # For files that go into subfolders:
    #   if(folders[i] %in% c("Crab CATCH Files/", "Crab SPECIMEN Files/", "RAW Haul Files/", "_error_reports/")){
    #     
    #     # # Create subfolders for archive if don't already exist
    #     #   if(destination == "archive"){
    #     #     dir.create(file.path(dest_path, folders[i]), showWarnings = FALSE)
    #     #   }
    #     
    #     # # ID "pattern" based on file category
    #     #   if(folders[i] %in% c("Crab CATCH Files/", "Crab SPECIMEN Files/", "RAW Haul Files/")){
    #     #     pattern <- haul_number
    #     #   } 
    #     # 
    #     #   # if(folders[i] == "_error_reports/"){ # these get written differently
    #     #   #   pattern <- paste0(vessel, "_", leg, "_Haul", haul_id)
    #     #   # } 
    #     # 
    #     # # Copy files
    #     #   list.files(paste0(clean_dir, folders[i]), pattern = haul_number) %>%
    #     #     map(~file.copy(from = paste0(clean_dir, folders[i], .x),
    #     #                    to = paste0(dest_path, folders[i], .x),
    #     #                    overwrite = TRUE))
    #   } 
    # 
    # 
    # # For db files: these get written differently...
    #   if(folders[i] == "db"){
    #     # Copy db files
    #     list.files(clean_dir, pattern = "_db.csv") %>%
    #       map(~file.copy(from = paste0(clean_dir, .x),
    #                      to = paste0(sftp_dir, .x),
    #                      overwrite = TRUE))
    #   }
  } # end folder loop
  # }
  
  
  
  
  
}











