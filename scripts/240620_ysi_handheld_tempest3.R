## This script imports and cleans up YSI handheld data from TEMPEST3. I'm working
## off of Google Drive where the raw files are. Pro Pluses were used at the flumes
## to measure runoff, Pro DSSes were used to measure source water. 
##
## Peter Regier
## 2024-06-20
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

## Load packages (will need to install pacman if not already installed)
require(pacman)
p_load(tidyverse,
       cowplot, 
       janitor, 
       parsedate,
       googledrive)

## URLs for data folders. Since things are scattered and there are only 4 instruments
## let's just manually set them. DSS = source, Plus = runoff

## Source (DSS) paths
fw_source_path = "https://drive.google.com/drive/folders/1HO8SWjhAS7PDCe9z57kGnng1eI7aVEKt"
sw_source_path = "https://drive.google.com/drive/folders/1g8Q1rpj2gy-jW3XDY4pQbryPSaMJHyI0"

## Runoff (Plus) path
runoff_path = "https://drive.google.com/drive/folders/1gOuGXZYTq7imTMluDjH6RzTxxOohoJZi"

## set ggplot theme
theme_set(theme_bw())


# 2. Download data -------------------------------------------------------------

## If you haven't used the googledrive package, you'll need to authorize it via 
## browser first. Look at drive_auth()
fw_source_files = drive_ls(fw_source_path)
sw_source_files = drive_ls(sw_source_path)
runoff_path = drive_ls(runoff_path)

## Set a raw data folder on local for files
download_path = "data/ysi_handheld_from_gdrive/"

## function to read in all files in a folder from GDrive
drive_download_ <- function(data){
  drive_download(data$id, overwrite = T, path = paste0(download_path, data$name))
}

# ## Download fw source
# for(i in 1:nrow(fw_source_files)){
#   drive_download_(fw_source_files %>% slice(i))
# }
# 
# ## Download sw source
# for(i in 1:nrow(sw_source_files)){
#   drive_download_(sw_source_files %>% slice(i))
# }
# 
# ## Download runoff
# for(i in 1:nrow(runoff_path)){
#   drive_download_(runoff_path %>% slice(i))
# }


## 3. Read in data -------------------------------------------------------------

## Function for DSS (source) data files
read_source <- function(file){
  read_csv(paste0(download_path, file), skip = 8) %>% 
    clean_names() %>% 
    mutate(datetime_raw = parsedate::parse_date(paste(date_mm_dd_yyyy, time_hh_mm_ss))) %>% 
    mutate(datetime_est = datetime_raw - hours(1)) %>% 
    dplyr::select(datetime_raw, datetime_est, temp_c, sp_cond_m_s_cm, sal_psu, odo_mg_l) %>% 
    rename("spcond_us_cm" = sp_cond_m_s_cm) ## renaming because clean_names scrubs micro symbol
}

source <- bind_rows(fw_source_files$name %>% 
  map(read_source) %>% 
  bind_rows() %>% 
  mutate(plot = "FW"), 
  sw_source_files$name %>% 
    map(read_source) %>% 
    bind_rows() %>% 
    mutate(plot = "SW"))


## Since we have both datasets in one file, we'll just use that...
## FW = WARD, SW = SW
runoff <- read_csv(paste0(download_path, runoff_path$name[[2]])) %>% 
  clean_names() %>% 
  mutate(datetime_raw = parsedate::parse_date(timestamp), 
         plot = case_when(unit_id == "WARD" ~ "FW", 
                          TRUE ~ unit_id)) %>% 
  mutate(datetime_est = case_when(plot == "FW" ~ datetime_raw + hours(1), 
                                  plot == "SW" ~ datetime_raw - hours(1))) %>% 
  rename("temp_c" = temperature_c, 
         "spcond_us_cm" = specific_conductance_u_s_cm, 
         "sal_psu" = salinity_ppt, 
         "odo_mg_l" = dissolved_oxygen_mg_l) %>% 
  dplyr::select(datetime_raw, datetime_est, temp_c, spcond_us_cm, sal_psu, odo_mg_l, plot)


df <- bind_rows(source %>% mutate(type = "0_source"), 
                runoff %>% mutate(type = "1_runoff")) %>% 
  mutate_all(~ifelse(. %in% c(-99999, 88888), NA, .))
  

make_plot <- function(var, name){
  ggplot(df, aes(datetime_est, {{var}}, color = type)) + 
    geom_line() + 
    facet_wrap(~plot, ncol = 1, scales = "free_y") + 
    labs(y = name)
  ggsave(paste0("figures/", name, ".png"), width = 6, height = 5)
}

make_plot(temp_c, "temp_c")
make_plot(spcond_us_cm, "spcond_us_cm")
make_plot(sal_psu, "sal_psu")
make_plot(odo_mg_l, "do_mgl")
