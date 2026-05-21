# Setup??

# Start from scratch each time you run
  rm(list = ls())
  
# Load libraries-
  library(tidyverse)
  library(purrr)
  library(cli) # for colored text and pluralization
  library(stringi) # read .txt files
  library(data.table) # read .txt files??
  library(rlist) # append to list

# Set directory for sourced function scripts
  scripts_dir <- "./Scripts/functions/"
  
# Set directory for QAQC Queue
  queue_dir <- normalizePath(path = file.path(Sys.getenv("USERPROFILE"), "Desktop/QAQC Queue"), winslash = "/")

# Source scripts with functions for workflow
  source(paste0(scripts_dir, "check_inputs.R"))
  source(paste0(scripts_dir, "file_mgmt_functions.R"))
  source(paste0(scripts_dir, "error_report.R"))
  source(paste0(scripts_dir, "file_checks_global.R"))
  source(paste0(scripts_dir, "id_zero_catch.R"))
  source(paste0(scripts_dir, "file_checks_haul.R"))
  # source(paste0(scripts_dir, "specimen_checks.R"))
  # source(paste0(scripts_dir, "final_haul_checks.R"))
