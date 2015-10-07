# Load libraries, define global variables, load utility functions --------------

library(httr)
library(dplyr)
library(magrittr)
library(xml2)
library(lubridate)
library(xlsx)
library(yaml)
library(stringr)

config <- yaml.load_file("config.yml")
base_url <- config$base_url


source("./RScripts/utils.R")

# Download, process and save data ----------------------------------------------

assets_df <- download_data(config$folder_names, config$redmine_key,
                           config$alfresco_key) %>%
  filter(name != ".DS_Store", type != "folder") 

write.xlsx(assets_df, "./data/asset_list.xlsx", row.names = FALSE )




