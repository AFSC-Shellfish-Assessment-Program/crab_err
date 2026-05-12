# File management functions (move, copy, compile_db):


## different actions/locations:
#  - move to archive
#    - also copy to FTP/backup archive??
#    - catch, specimen, raw?
#  - move to clean
#    - catch, specimen, raw
#  - copy to FTP
#    - need to make directories if don't exist
#    - catch, specimen, raw, error, db, archive?
#  - copy to USB backup
#    - catch, specimen, raw, error, db, archive?
#  - compile db files
#
# error, db, and archive automatically get written into clean_dir...
##**error reports??* have temp ones written into the queue and then they get moved when they're ready
##*how do we incorporate this with additional errors after we think they're clean...maybe I just take care of post-hoc


move_files <- function(files, # feed function a list of files (either ALL files for clean, or files for archive)
                       vessel,
                       leg, 
                       haul_number,
                       # path, 
                       destination = c("clean", "archive")){
  
  # Set directories
    path <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop"), winslash = "/")
    in_dir <- paste0(path, "/QAQC_queue/")
    
    #**NEED TO UPDATE THESE FOR FINAL!!*
    if(destination == "clean"){
      clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/")
    }

    if(destination == "archive"){
      clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/Data Files from Tablet/Archive/")
    }
    
  
  # Move CRAB_CATCH files
    catch <- files[grepl(paste0("_CRAB_CATCH_", haul_number), files)] %>%
             map(~file.rename(from = paste0(in_dir, .x),
                              to = paste0(clean_dir, "Data Files from Tablet/Crab CATCH Files/", .x)))
  
  # Move CRAB_SPECIMEN files
    specimen <- files[grepl(paste0("_CRAB_SPECIMEN_", haul_number), files)] %>%
                map(~file.rename(from = paste0(in_dir, .x),
                                 to = paste0(clean_dir, "Data Files from Tablet/Crab SPECIMEN Files/", .x)))
  
  # Move RAW files
    raw <- files[grepl(paste0("_HAUL", haul_number), files)] %>%
           map(~file.rename(from = paste0(in_dir, .x),
                            to = paste0(clean_dir, "Data Files from Tablet/RAW Haul Files/", .x)))
  
  # Move Error Report file
    report <- list.files(paste0(clean_dir, "Error Reports/"), pattern = paste0(vessel, "_", leg, "_Haul", haul_id))%>%
              map(~file.rename(from = paste0(in_dir, .x),
                               to = paste0(clean_dir, "Error Reports/", .x)))
  
  return()
}



# Copy to FTP or USB Backup
copy_files <- function(files, # specify exactly which files we're applying this to, and then if it's ever to archive, then that'll also copy to FTP and USB?
                       vessel,
                       leg, 
                       haul_number,
                       file_type = c("tablet", "archive", "db"), # "tablet" includes error reports, "archive" goes straight from queue not clean (must be used before archive files are moved)
                       # path, # overall root path, can have in/clean_dir specified here...
                       destination = c("ftp", "backup")){
  
  # Set source directories
    path <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop"), winslash = "/")
    clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/")
  
    
  # write checks for proper destinations, file_types....
  # - only 1 input for each
  # - make sure 'files' is a list....
  # - must have 'haul_number' if file_type == "tablet" or "archive"
    

  # Set destination path
    if(destination == "ftp"){
      # Set base path
        dest_path <- paste0(path, "/to_FTP/") 
        
      # Create 'Data Files from Tablet' subfolder if doesn't already exist
        dir.create(file.path(dest_path, "/Data Files from Tablet/"), recursive = TRUE, showWarnings = FALSE)
        
      # # Create vessel and leg subfolders if don't already exist 
      #   dir.create(file.path(dest_path, vessel, leg), recursive = TRUE, showWarnings = FALSE)
      # 
      # # Update with full path
      #   dest_path <- paste0(dest_path, vessel, "/", leg, "/") 
    }
    
    if(destination == "backup"){
      # dest_path <- paste0("D:/", leg, " - both boats/") 
      dest_path <- paste0(path, "/USB_backup/", leg, " - both boats/") 
    }
    
    
  # For clean tablet files: ----
    if(file_type == "tablet"){
      
      # Set destination subfolders
        folders <- c("Crab CATCH Files/", 
                     "Crab SPECIMEN Files/",
                     "RAW Haul Files/",
                     "Error Reports/")
      
      # Loop over folder pathways
        for(i in 1:length(folders)){
          
        # For files that go into subfolders:
          if(folders[i] %in% c("Crab CATCH Files/", "Crab SPECIMEN Files/", "RAW Haul Files/", "Error Reports/")){
            
          # ID "pattern" based on file category and modify destination directory
            if(folders[i] %in% c("Crab CATCH Files/", "Crab SPECIMEN Files/", "RAW Haul Files/")){
              
              # Create subfolders if don't already exist, set destination directory based on which folder  
                if(destination == "ftp"){
                  dir.create(file.path(dest_path, "Data Files From Tablet/", folders[i]), recursive = TRUE, showWarnings = FALSE)
                }
              
              # # Set pattern
              #   pattern <- haul_number
                
              # Copy files
                copy <- list.files(paste0(clean_dir, folders[i]), pattern = haul_number) %>%
                        map(~file.copy(from = paste0(clean_dir, folders[i], .x),
                                       to = paste0(dest_path, "Data Files From Tablet/", folders[i], .x),
                                       overwrite = TRUE))
            } 
            
            if(folders[i] == "Error Reports/"){
              
              # Create subfolders if don't already exist, set destination directory based on which folder  
                if(destination == "ftp"){
                  dir.create(file.path(dest_path, folders[i]), recursive = TRUE, showWarnings = FALSE)
                }
              
              # # Set pattern
              #   pattern <- paste0(vessel, "_", leg, "_Haul", haul_id)
                
              # Copy files
                copy <- list.files(paste0(clean_dir, folders[i]), pattern = paste0(vessel, "_", leg, "_Haul", haul_id)) %>%
                        map(~file.copy(from = paste0(clean_dir, folders[i], .x),
                                       to = paste0(dest_path, folders[i], .x),
                                       overwrite = TRUE))
            } 
            
          # # Create subfolders if don't already exist, set destination directory based on which  
          #   if(destination == "ftp"){
          #       dir.create(file.path(dest_path, folders[i]), recursive = TRUE, showWarnings = FALSE)
          #   }
          #
          # # Copy files
          #   copy <- list.files(paste0(clean_dir, folders[i]), pattern = pattern) %>%
          #           map(~file.copy(from = paste0(clean_dir, folders[i], .x),
          #                          to = paste0(dest_path, folders[i], .x),
          #                          overwrite = TRUE))
          }
       }
    }

    
  # For archive files: ----  
    if(file_type == "archive"){
      
      # Create archive subfolder if doesn't already exist 
        dir.create(file.path(dest_path, "Data Files from Tablet/Archive/"), recursive = TRUE, showWarnings = FALSE)
      
      # Set directories
        in_dir <- paste0(path, "/QAQC_queue/")
        dest_path <- paste0(dest_path, "Data Files from Tablet/Archive/")
      
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
            if(destination == "ftp"){
              dir.create(file.path(dest_path, folders[i]), showWarnings = FALSE)
            }
              
          # ID file pattern based on type
            copy <- files[grepl(paste0(types[i], haul_number), files)] %>%
                    map(~file.copy(from = paste0(in_dir, .x),
                                   to = paste0(dest_path, folders[i], .x),
                                   overwrite = TRUE))
          }
    }
    
    
  # For db files: ----
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
                             leg){
  
  
  # Set directory to source clean files from
    # path <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop"), winslash = "/")
    clean_dir <- paste0("C:/EBS Shelf 2026/Database and Data/", vessel, "/", leg, "/")
  
  
  # Read in, combine, and save clean 'Crab Specimen' files
    specimen_db <- list.files(paste0(clean_dir, "Data Files from Tablet/Crab SPECIMEN Files/"), pattern = paste0("_CRAB_SPECIMEN_"), recursive = TRUE) %>%
                   map_df(~read.csv(paste0(clean_dir, "Data Files from Tablet/Crab SPECIMEN Files/", .x))) %>%
                   write.csv(., paste0(clean_dir, "SPECIMEN_db.csv"), row.names = FALSE)
  
  
  # Read in, combine, and save clean 'Crab Catch' files
    catch_db <- list.files(paste0(clean_dir, "Data Files from Tablet/Crab CATCH Files/"), pattern = paste0("_CRAB_CATCH_"), recursive = TRUE) %>%
                map_df(~read.csv(paste0(clean_dir, "Data Files from Tablet/Crab CATCH Files/", .x))) %>%
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
    
    
  # Copy the db files to the FTP queue and the USB backup
    copyFTP <- copy_files(vessel = vessel,
                          leg = leg, 
                          file_type = "db", 
                          # path = path, 
                          destination = "ftp")
    
    copyUSB <- copy_files(vessel = vessel,
                          leg = leg, 
                          file_type = "db", 
                          # path = path, 
                          destination = "backup")
    
    
  # Print message confirming db files have been copied to the FTP queue and the USB backup
    cat("These files have also been copied to the FTP queue and the ", vessel, " ", leg, " USB backup.\n\n", sep = "")
}
