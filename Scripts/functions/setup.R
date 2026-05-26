# Setup

# Start from scratch each time you run
  rm(list = ls())
  
# Load libraries
  libraries <- list("tidyverse",
                    "purrr",
                    "cli", # for colored text and pluralization
                    "stringi", # read .txt files
                    "data.table", # read .txt files?
                    "rlist") # append to list
  suppressMessages(suppressWarnings(invisible(lapply(libraries, library, character.only = TRUE))))
  
  
# Set directory for QAQC Queue
  queue_dir <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop/QAQC Queue"), winslash = "/")

  
# Set directory for sourced function scripts
  scripts_dir <- "./Scripts/functions/"

  
# Source scripts with functions for workflow
  source(paste0(scripts_dir, "check_inputs.R"))
  source(paste0(scripts_dir, "file_mgmt_functions.R"))
  source(paste0(scripts_dir, "error_report.R"))
  source(paste0(scripts_dir, "file_checks_global.R"))
  source(paste0(scripts_dir, "id_zero_catch.R"))
  source(paste0(scripts_dir, "file_checks_haul.R"))
  # source(paste0(scripts_dir, "specimen_checks.R"))
  # source(paste0(scripts_dir, "final_haul_checks.R"))
  # source(paste0(scripts_dir, "haul_checks.R"))
  
  
# Print confirmation message
  cat("\nLibraries and source functions have been successfully loaded.\n\n")
  
