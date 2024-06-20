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
       plotly,
       hms,
       googledrive)

## URLs for data folders. Since things are scattered and there are only 4 instruments
## let's just manually set them. DSS = source, Plus = runoff

## Source (DSS) paths
fw_source_path = "https://drive.google.com/drive/folders/1HO8SWjhAS7PDCe9z57kGnng1eI7aVEKt"
sw_source_path = "https://drive.google.com/drive/folders/1g8Q1rpj2gy-jW3XDY4pQbryPSaMJHyI0"

## Runoff (Plus) path
runoff_path = "https://drive.google.com/drive/folders/19R5QL3M2XSIlghL5ZAF4yrJY5wVfCdUO"

## set ggplot theme
theme_set(theme_bw())

## Set a common tz
common_tz = "Etc/GMT+5"


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
## Download runoff
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
    mutate(plot = "SW")) %>% 
  mutate(datetime_est = force_tz(datetime_est, tzone = common_tz))


## Set up a function to read in runoff files from Pro Pluses
read_runoff <- function(file){
  read_csv(paste0(download_path, file)) %>% 
    clean_names() %>% 
    mutate(datetime_raw = parsedate::parse_date(timestamp)) %>% 

    rename("temp_c" = temperature_c, 
           "spcond_us_cm" = specific_conductance_u_s_cm, 
           "sal_psu" = salinity_ppt, 
           "odo_mg_l" = dissolved_oxygen_mg_l) %>% 
    dplyr::select(datetime_raw, temp_c, spcond_us_cm, sal_psu, odo_mg_l)
}

## SW Pro Plus was 1 hour ahead of EST per email, FW was two hours behind
runoff <- bind_rows(read_runoff(runoff_path$name[[1]]) %>% 
              mutate(datetime_est = datetime_raw - hours(1), 
                     plot = "SW"), 
              read_runoff(runoff_path$name[[2]]) %>% 
              mutate(datetime_est = datetime_raw + hours(1), 
                     plot = "FW")) %>% 
  mutate(datetime_est = force_tz(datetime_est, tzone = common_tz)) %>%
  relocate(datetime_est, .after = datetime_raw) 


df <- bind_rows(source %>% mutate(type = "0_source"), 
                runoff %>% mutate(type = "1_runoff")) %>% 
  filter(datetime_est < as.POSIXct("2024-06-14", tz = common_tz)) %>% 
  mutate_if(is.numeric, ~ifelse(. %in% c(-9999, 88888, 99999), NA, .)) %>% 
  mutate(flood = case_when(datetime_est < as.POSIXct("2024-06-12", tz = common_tz) ~ "flood1", 
                           datetime_est > as.POSIXct("2024-06-12", tz = common_tz) & 
                                                       datetime_est < as.POSIXct("2024-06-13", tz = common_tz) ~ "flood2",
                           datetime_est > as.POSIXct("2024-06-13", tz = common_tz) & 
                                                       datetime_est < as.POSIXct("2024-06-14", tz = common_tz) ~ "flood3", 
                           TRUE ~ NA)) %>% 
  mutate(tod = as_hms(datetime_est))

## Test plot
p1 <- ggplot(df, aes(datetime_est, odo_mg_l, color = type)) + 
  geom_line() + 
  facet_wrap(~plot, scales = "free", ncol = 1)

## It looks like the runoff (Pluses) didn't start until close to noon on Flood 1?
ggplotly(p1)


ggplot(df, aes(tod, odo_mg_l, color = type)) + 
  geom_line() + 
  geom_vline(aes(xintercept = as_hms("06:00:00")), linetype = "dashed") + 
  facet_wrap(plot~flood, nrow = 2, scales = "free")

##
make_plot <- function(var, name){
  ggplot(df, aes(tod, {{var}}, color = type)) + 
    geom_line() + 
    geom_vline(aes(xintercept = as_hms("06:00:00")), linetype = "dashed") + 
    facet_wrap(plot~flood, nrow = 2, scales = "free")
    labs(y = name)
  ggsave(paste0("figures/", name, ".png"), width = 10, height = 5)
}

make_plot(temp_c, "temp_c")
make_plot(spcond_us_cm, "spcond_us_cm")
make_plot(sal_psu, "sal_psu")
make_plot(odo_mg_l, "do_mgl")
