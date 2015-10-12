# Asset Report

The `Asset Report` provides information about released Prime assets from a LabCase point of view. 
We only examine metadata from assets which are stored in the main Prime project in LabCase in one of the methodology-specific folders.

However, the report is written in such a generic way that it can also be used to examine files residing in any other LabCase project.
Its main purpose is to visualize file metadata 

Because downloading file metadata via the LabCase REST API can take considerable time, we decided to separate download/process activities from the report generation. 

The repo consists of 4 main parts:

* `utils.R`: Utility functions to download and process file metadata
* `main.R`: Wrapper for calling different utility functions and for generating the report
* `report/asset_report.Rmd`: Loads the processed data and creates a report
* `config.yml`: Config file specifying the LabCase and Alfresco API keys and folder names whose content (metadata) should be downloaded

## Installation

* R `3.2.1` or later
* You need to install the following R libraries
  * httr
  * plyr
  * dplyr
  * tidyr
  * magrittr
  * xml2
  * lubridate
  * stringr
  * xlsx
  * yaml
  * knitr
  * knitrBootstrap
  * ggplot2
  * grid
  
  

