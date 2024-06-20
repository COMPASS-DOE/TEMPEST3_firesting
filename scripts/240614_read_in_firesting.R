## Import and clean up TEMPEST3 Firesting datasets
## In Freshwater and Saltwater, we had 4 sensors on a 4-channel logger at 5, 15,
## 30, and 50 cm. In Control, Ch4 on the 4-channel logger was not responding when
## shipped to us, so we used Ch1-3 and used a handheld for 50 cm. The handheld may
## have been collecting untrustworthy data, tbd. We confirmed on 6/14/24 that all
## 4-channel loggers were set to EST, although (for some reason) the handhel was
## set to EDT+1 (ie, 2 hours ahead of EST).
##
## Peter Regier 
## 2024-06-14
##
# ########## #
# ########## #


# 1. Setup ---------------------------------------------------------------------

#Install the pacman package if not installed
#install.packages("pacman")
require(pacman)
p_load(tidyverse, 
       magrittr, 
       respR,
       plotly)

theme_set(theme_bw())

common_tz = "Etc/GMT+5"

## Set flood starts
flood1_start <- as.POSIXct("2024-06-11 06:00:00", tz = common_tz)
flood2_start <- as.POSIXct("2024-06-12 06:00:00", tz = common_tz)
flood3_start <- as.POSIXct("2024-06-13 06:00:00", tz = common_tz)

## Set a vector of depths for each channel (C1, C2, C3, C4)
sensor_depths = c(5, 15, 30, 50)

# 2. Read in data --------------------------------------------------------------

## Set the paths for your datasets
c_path <- "data/firesting/control/"
fw_path <- "data/firesting/freshwater/"
sw_path <- "data/firesting/saltwater/"

## Set up a function to read in data across all channels for a given folder
read_firesting <- function(folder_path){
  
  depths = sensor_depths
  
  paths <- map(1:4, ~paste0(folder_path, "/ChannelData/A_Firesting O2 (4 Channels)_(A Ch.", .x, ")_Oxygen.txt"))
  
  read_file <- function(path, depth){
    
    ## Set raw firesting data column names
    four_channel_names <- c("date", "time", "dt_s", "do_percent_sat", "dphi",
                            "intensity_mv", "light_mv", "do_status", "temp_date", "temp_time",
                            "temp_dt_s", "temp_c", "temp_status", "press_date", "press_time",
                            "press_dt_s", "pressure_mbar", "press_status")
    
    ## Read and format file
    read_delim(path, delim = "\t", skip = 25,  col_names = F) %>%
      magrittr::set_colnames(four_channel_names) %>%
      mutate(datetime_raw = paste(date, time)) %>%
      mutate(datetime = round_date(lubridate::as_datetime(datetime_raw, 
                                                          format = "%d-%m-%Y %H:%M:%S",
                                                          tz = common_tz), "5min")) %>%
      dplyr::select(datetime_raw, datetime, temp_c, do_percent_sat) %>% 
      mutate(depth = depth)
  }
  
  map2(paths, depths, read_file) %>%
    bind_rows()
  
}

read_firesting_3ch <- function(folder_path){
  
  ## Set your timezone - this example is EST
  common_tz = "Etc/GMT+5"
  
  ## Set a vector of depths for each channel (C1, C2, C3, C4)
  depths = sensor_depths[1:3]
  
  paths <- map(1:3, ~paste0(folder_path, "/ChannelData/A_Firesting O2 (4 Channels)_(A Ch.", .x, ")_Oxygen.txt"))
  
  read_file <- function(path, depth){
    
    ## Set raw firesting data column names
    four_channel_names <- c("date", "time", "dt_s", "do_percent_sat", "dphi",
                            "intensity_mv", "light_mv", "do_status", "temp_date", "temp_time",
                            "temp_dt_s", "temp_c", "temp_status", "press_date", "press_time",
                            "press_dt_s", "pressure_mbar", "press_status")
    
    ## Read and format file
    read_delim(path, delim = "\t", skip = 25,  col_names = F) %>%
      magrittr::set_colnames(four_channel_names) %>%
      mutate(datetime_raw = paste(date, time)) %>%
      mutate(datetime = round_date(lubridate::as_datetime(datetime_raw, 
                                                          format = "%d-%m-%Y %H:%M:%S",
                                                          tz = common_tz), "5min")) %>%
      dplyr::select(datetime_raw, datetime, temp_c, do_percent_sat) %>% 
      mutate(depth = depth)
  }
  
  map2(paths, depths, read_file) %>%
    bind_rows()
  
}

## Read in a folder
firesting_fw <- list.files(fw_path, full.names = T) %>% 
  map(read_firesting) %>% 
  bind_rows() %>% 
  mutate(plot = "Freshwater") 

firesting_sw <- list.files(sw_path, full.names = T) %>%
  map(read_firesting) %>%
  bind_rows() %>%
  mutate(plot = "Saltwater")

firesting_c <- list.files(c_path, full.names = T) %>%
  map(read_firesting_3ch) %>%
  bind_rows() %>%
  mutate(plot = "Control")

## Merge datasets if reading more than one folder
firesting <- bind_rows(firesting_fw, firesting_sw, firesting_c)


# 3. Clean data ----------------------------------------------------------------

## Add any additional cleaning or QAQC steps needed for your specific dataset
## In this case, We remove bad data from initial deployment and from the end of deployment
firesting_cleaned <- firesting %>% 
  mutate(do_percent_sat = ifelse(do_percent_sat < 0, 0, do_percent_sat)) %>% 
  filter(datetime >= flood1_start - hours(12))


# 5. Plot data -----------------------------------------------------------------

## Create a simple plot to visualize across time and depth
ggplot(firesting_cleaned, 
             aes(datetime, do_percent_sat, color = as.factor(depth))) + 
  geom_vline(aes(xintercept = flood1_start), linetype = "dashed") + 
  geom_vline(aes(xintercept = flood2_start), linetype = "dashed") + 
  geom_vline(aes(xintercept = flood3_start), linetype = "dashed") + 
  geom_line() +
  facet_wrap(~plot, ncol = 1)

## Alternatively, you can use geom_contour_filled to extrapolate across the 
## depth profile
ggplot(firesting, aes(x = datetime, y = depth, z = do_percent_sat)) + 
  geom_contour_filled() + 
  facet_wrap(~plot) + 
  scale_x_datetime(expand = c(0,0)) + 
  scale_y_reverse(expand = c(0,0)) + 
  scale_fill_viridis_d(direction = -1) + 
  labs(x = "", y = "depth (cm)", fill = "") + 
  theme(strip.background = element_rect(fill = "gray90"), 
        axis.text = element_text(size = 14),    # Adjust size of axis labels
        axis.title = element_text(size = 16),  
        strip.text = element_text(size = 14)) +  # Remove background from legend
  theme(panel.background = element_blank(), 
        plot.background = element_blank())

write_csv(firesting, "data/240613_firesting_fw.csv")



