# Load libraries, define global variables, load utility functions --------------

library(httr)
library(plyr)
library(dplyr)
library(magrittr)
library(xml2)
library(lubridate)
library(xlsx)
library(yaml)
library(stringr)

library(knitr)
library(knitrBootstrap)

source("./RScripts/utils.R")

config <- yaml.load_file("config.yml")
base_url <- config$base_url

# Download, process and save data ----------------------------------------------


assets_df <- try(download_data(config$folder_names, config$redmine_key,
                           config$alfresco_key), silent = TRUE)

if (class(assets_df) != "try-error") {
  
  # Save raw data and download timestamp
  write.csv(assets_df, "./data/asset_list_raw.csv", row.names = FALSE )
  write.csv(now(), "./data/date_of_extraction.csv", row.names = FALSE)
  
  # Process data
  assets_df %<>% filter(name != ".DS_Store", type != "folder") %>%
    mutate(
      method_acronym = map_name_to_acronym(methodology, config$mapping_rules)
    )
  
  # Save processed data
  write.xlsx(assets_df, "./data/asset_list_processed.xlsx", row.names = FALSE)
  write.csv(assets_df, "./data/asset_list_processed.csv", row.names = FALSE)
  
}

# Create report ----------------------------------------------------------------

# Why change the working directory?
#
# Answer: Workaround to fix the issue regarding "figure" folder generation 
# through the knit command. 
#
# Although the knit runs in its own process (by default the directory the .Rmd 
# resides), the figure folder gets created in the working directory of the 
# overall R session the knit command was started in.
#
# Therefore the relative paths of the figures in the created .md file would point
# to figure/ instead to report/figure/ should we not switch the working directory.
# In the next step markdownToHTML could not reference the figures because a 
# figure/ folder would not exist in /report.
setwd("report/")

knit('asset_report.Rmd', 'asset_report.md')
knit_bootstrap_md('asset_report.md', 'asset_report.html', boot_style = 'Cerulean')

setwd("..")







