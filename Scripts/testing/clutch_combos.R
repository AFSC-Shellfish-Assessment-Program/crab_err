## Generate final lookup of "allowed" clutch combinations

# Notes:
# - need to verify BKC SC4/5 egg conditions
# - chionoecetes SC2 can they have eyed eggs?
# - dead eggs - flag anything other than 031, have them verify and make note...
# - empty egg cases - same as dead eggs
#
# - immature girls?? what's the max Shell condition they can have??




lookups <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/lookup_tables/"

clutch_codes <- read.csv(paste0(lookups, "clutch_codes.csv"))
# color_combos <- read.csv(paste0(lookups, "egg_color_species_combos.csv"))
shell_egg_combos <- read.csv(paste0(lookups, "shell_egg_condition_combos.csv"))


clutch_combos <- clutch_codes %>%
                 # filter(!is.na(DESCRIPTION)) %>%
                 right_join(., shell_egg_combos, relationship = "many-to-many") %>%
                 # right_join(., color_combos, relationship = "many-to-many") %>%
                 select(SPECIES, SHELL_CONDITION, EGG_CONDITION, CLUTCH_SIZE, DESCRIPTION) #%>%
                 # filter(!(CLUTCH_SIZE == 0 & SHELL_CONDITION > 3))

# make lookup of invalid clutch combos (ignoring egg color....)
clutch_lookup <- clutch_combos %>%
                 # select(-EGG_COLOR) %>%
                 right_join(., expand.grid(SPECIES = c("RKC", "BKC", "SNOW", "TANNER", "HYBRID"),
                                           SHELL_CONDITION = c(0:5),
                                           EGG_CONDITION = c(0:5),
                                           CLUTCH_SIZE = c(0:5))) %>%
                 distinct() %>%
                 group_by(SPECIES, SHELL_CONDITION, EGG_CONDITION, CLUTCH_SIZE) %>%
                 mutate(N = nrow(.))


left_join(., .$data %>% group_by(SPECIES, SHELL_CONDITION, EGG_CONDITION, CLUTCH_SIZE) %>% summarise(N = n()))
                 filter(is.na(DESCRIPTION))

# need to make different error messages depending on if invalid biologically 
# vs. invalid ecologically? (ie. species-specific or not...)


# EXPLORATORY ------------------------------------------------------------------
data_dir <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/"
ebscrab <- readRDS(paste0(data_dir, "ebscrab.rds"))
haul_ebs <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/haul_ebs.csv")

district_stratum <- read.csv(paste0(data_dir, "lookup_tables/district_stratum.csv"))
stratum_stations <- read.csv(paste0(data_dir, "lookup_tables/stratum_stations.csv"))
stratum_area <- read.csv(paste0(data_dir, "lookup_tables/stratum_area.csv"))
stratum_design <- read.csv(paste0(data_dir, "lookup_tables/stratum_design.csv"))
weight_regression <- read.csv(paste0(data_dir, "lookup_tables/weight_regression.csv"))
species_lookup <- tibble(SPECIES_CODE = c(68560, 68580, 68590, 69322, 69323,
                                          69400, 69310, 68550, 68541),
                         SPECIES_NAME = c("Tanner Crab", "Snow Crab", "Hybrid Crab",
                                          "Red King Crab", "Blue King Crab", "Horsehair Crab",
                                          "Golden King Crab", "Grooved Tanner Crab", "Chinocetes Mix"),
                         SPECIES = c("TANNER", "SNOW", "HYBRID", "RKC", "BKC",
                                     "HAIR", "GKC", "GROOVED", "CMIX"))

specimens <- ebscrab %>%
             select(-c('HAUL', 'CRUISE', 'VESSEL','GIS_STATION', 'STATION')) %>%
             # join with haul data to filter only specimens in relevant hauls
             right_join(., haul_ebs %>% select(HAULJOIN, STATION_ID, MID_LATITUDE, YEAR, HAUL_TYPE), by = c('HAULJOIN')) %>%
             filter(!is.na(SEX),
                    # Remove HT 17 for all species but RKC
                    !(HAUL_TYPE == 17 & SPECIES_CODE != 69322)) %>%
             # add species name
             left_join(., species_lookup %>% select(-SPECIES_NAME)) %>%
             mutate(# make SIZE and SIZE_1MM columns
                    SIZE = ifelse(is.na(LENGTH), WIDTH, LENGTH),
                    SIZE_1MM = floor(SIZE),
                    # make NA clutch into 999 for filtering
                    CLUTCH_SIZE = ifelse(SEX == 2 & is.na(CLUTCH_SIZE), 999, CLUTCH_SIZE),
                    # # reassign empty egg cases (EC4) to clutch size 1 (mature, no eggs) -- EC3/dead eggs always clutch >=2?
                    # CLUTCH_SIZE = ifelse(EGG_CONDITION == 4, 1, CLUTCH_SIZE),
                    # reassign clutch size 7 -> 6
                    CLUTCH_SIZE = ifelse(CLUTCH_SIZE == 7, 6, CLUTCH_SIZE)) %>%
             filter(# remove hermaphrodites; KEEP SEX = 3(unsexed) for numerical accounting (total num CPUE, abundance)
                    # SEX %in% c(1, 2, 4), 
                    # remove non-standard species
                    SPECIES_CODE %in% species_lookup$SPECIES_CODE[1:6],
                    # filter females with clutch code 999 or NA
                    !(SEX == 2 & CLUTCH_SIZE %in% c(999))) %>%
             # assign ovig/nonovig, and areas based on lat
             mutate(OVIGEROUS = case_when(SEX == 2 & CLUTCH_SIZE > 1 ~ "OVIG",
                                          SEX == 2 & CLUTCH_SIZE <= 1 ~ "NONOVIG", 
                                          TRUE ~ NA),
                    WEIGHT_AREA = case_when((SPECIES_CODE == 69323 & MID_LATITUDE <= 58.65) ~ "PRIB", # S male
                                            (SPECIES_CODE == 69323 & MID_LATITUDE > 58.65) ~ "STMATT", # N male,
                                            TRUE ~ "ALL")) %>%
             # join length/weight regression parameters
             left_join(., weight_regression) %>%
             # calculate weights (based on length/width-weight regression)
             mutate(CALCULATED_WEIGHT = VARIABLE_A*SIZE^VARIABLE_B, 
                    CALCULATED_WEIGHT_1MM = VARIABLE_A*SIZE_1MM^VARIABLE_B) 



females <- specimens %>% filter(SEX == 2)

combos <- females %>%
          group_by(SPECIES, SHELL_CONDITION, EGG_CONDITION, CLUTCH_SIZE) %>%
          summarise(N = n()) %>%
          full_join(., clutch_combos %>% select(-EGG_COLOR) %>% distinct()) %>%
          filter(is.na(DESCRIPTION),
                 !SPECIES == "HAIR")

write.csv(combos, "C:/Users/Shannon.Hennessey/Desktop/clutch_census.csv", row.names = FALSE)


egg_colors <- females %>%
              group_by(SPECIES, EGG_COLOR) %>%
              summarise(N = n())
shell_egg_cond <- females %>%
                  group_by(SPECIES, SHELL_CONDITION, EGG_CONDITION) %>%
                  summarise(N = n())

rkc031 <- females %>% 
          filter(SPECIES == "RKC", 
                 # SHELL_CONDITION == 3,
                 !EGG_CONDITION == 4, 
                 !(EGG_COLOR == 0 & EGG_CONDITION == 0 & CLUTCH_SIZE == 1)) %>%
          group_by(EGG_CONDITION, CLUTCH_SIZE) %>%
          summarise(N = n())


femX3X_04X <- females %>% 
              filter((EGG_CONDITION == 3 )|(EGG_COLOR == 0 & EGG_CONDITION == 4),
                     !(EGG_COLOR == 0 & EGG_CONDITION == 0 & CLUTCH_SIZE == 1),
                     !(EGG_COLOR == 0 & EGG_CONDITION == 4 & CLUTCH_SIZE == 1),
                     !(EGG_COLOR == 0 & EGG_CONDITION == 3 & CLUTCH_SIZE == 1)) %>%
              group_by(SPECIES, YEAR, EGG_CONDITION) %>%
              summarise(N = n())

retow_girls <- females %>%
               filter(HAUL_TYPE == 17) %>%
               filter(SHELL_CONDITION == 2) %>%
               group_by(YEAR, SHELL_CONDITION, EGG_COLOR, EGG_CONDITION, CLUTCH_SIZE) %>%
               summarise(N = n())


