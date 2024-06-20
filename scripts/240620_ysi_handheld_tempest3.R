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


# 2. Import data ---------------------------------------------------------------

## Set a raw data folder on local for files

## If you haven't used the googledrive package, you'll need to authorize it via 
## browser first. Look at drive_auth()
drive_ls(fw_source_path)




















