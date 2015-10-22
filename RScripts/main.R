# Load libraries, define global variables, load utility functions --------------

library(httr)
library(plyr)
library(magrittr)
library(dplyr)
library(xml2)
library(lubridate)
library(yaml)
library(stringr)
library(readr)
library(readxl)
library(xlsx)


library(knitr)
library(knitrBootstrap)

source("./RScripts/utils.R")

config <- yaml.load_file("config.yml")
base_url <- config$base_url

# Download, process and save data ----------------------------------------------


assets_lc_df <- try(download_lc_data(config$folder_names, config$redmine_key,
                           config$alfresco_key), silent = TRUE)
assets_aris_df <- try(read_excel("./data_raw/aris_output_raw.xls"))


if (class(assets_lc_df) != "try-error" & class(assets_aris_df) != "try-error") {
  
  # Save raw LC data and download timestamp
  write_csv(assets_lc_df, "./data_raw/labcase_asset_list_raw.csv")
  write.csv(now(), "./data_raw/date_of_extraction.csv", row.names = FALSE)
  
  # Process data
  assets_lc_df <- process_lc_data(assets_lc_df)
  assets_aris_df <- process_aris_data(assets_aris_df)
  
  # Save processed data
  attributes(assets_lc_df)$class <- c("data.frame")
  write.xlsx(assets_lc_df, "./data_processed/labcase_asset_list_complete.xlsx",
             row.names = FALSE)
  
  attributes(assets_aris_df)$class <- c("data.frame")
  write.xlsx(assets_aris_df, "./data_processed/aris_asset_list_complete.xlsx",
             row.names = FALSE)
 
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







