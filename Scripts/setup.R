# Setup??

# Load libraries ----
library(tidyverse)
library(purrr)
library(cli) # for colored text
library(stringi) # read .txt files
library(data.table) # read .txt files??
library(rlist) # append to list

# Scripts directory....
scripts_dir <- "./Scripts/"
source(paste0(scripts_dir, "check_inputs.R"))
source(paste0(scripts_dir, "file_mgmt_functions.R"))
source(paste0(scripts_dir, "file_checks_global.R"))
source(paste0(scripts_dir, "report_setup.R"))
source(paste0(scripts_dir, "id_zero_catch.R"))
source(paste0(scripts_dir, "file_checks_haul.R"))
