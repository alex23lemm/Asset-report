download_data <- function(folder_names, redmine_key, alfresco_key) {
  root_folder_df <- get_folder_IDs(folder_names, redmine_key,
                                   alfresco_key)
  documents_df <- data.frame()
  
  for (i in 1:nrow(root_folder_df)) {
    methodology_docs <- get_document_list(root_folder_df$id[i],
                                          root_folder_df$name[i],
                                          redmine_key,
                                          alfresco_key)
    documents_df %<>% rbind(methodology_docs)
  }
  return(documents_df)
}


get_folder_IDs <- function(folder_names, redmine_key, alfresco_key) {
  # Downloads metadata information (ID, name) of folders residing in a LabCase
  # project
  #
  # Args:
  #   folder_names: character vector with folder name acronyms which are matched
  #                 against folder names
  #   redmine_key: Redmine API key
  #   alfresco_key: Alfresco API key
  #        
  # Returns:
  #   Data frame containing folder ID and folder name acronym
  
  root_listings <- GET(paste0(base_url, '.xml'),
                              add_headers(
                                `X-Redmine-API-Key` = redmine_key,
                                `X-LabCase-Token` = alfresco_key
                              ),
                              content_type_xml()
  ) %>% content(as = "text") %>% read_xml
  
  name <- root_listings %>% xml_find_all("//children/asset/name") %>% xml_text
  id <- root_listings %>% xml_find_all("//children/asset/id") %>% xml_text

  root_ID_df <- data.frame(name, id, stringsAsFactors = FALSE)
  
  folder_names %<>% str_c(collapse = "|")
  
  root_ID_df %<>% filter(str_detect(name, folder_names)) %>% 
    mutate(
      name = str_extract(name, folder_names)
    )
  return(root_ID_df) 
}



get_document_list <- function(folder_id, methodology, redmine_key, 
                              alfresco_key) {
  # Downloads information about documents residing in a certain folder of a 
  # LabCase project
  #
  # Args:
  #   folder_id: id of the folder whose content should be extracted as a list
  #   methdodoly: character string specifying the methodology the folder 
  #               belongs to
  #   redmine_key: Redmine API key
  #   alfresco_key: Alfresco API key
  #
  # Returns:
  #   Data frame containing information about every document residing in the 
  #   folder
  
  doc <- GET(paste0(base_url, '/', folder_id, '.xml'),
                         add_headers(
                           `X-Redmine-API-Key` = redmine_key, 
                           `X-Labcase-Token` = alfresco_key
                         ),
                         content_type_xml()
  ) %>% content(as = "text") %>% read_xml
  
  name <- xml_find_all(doc, ".//children/asset/name") %>% xml_text
  id <- xml_find_all(doc, ".//children/asset/id") %>% xml_text
  type <- xml_find_all(doc, ".//children/asset/type") %>% xml_text
  last_modified_raw <- xml_find_all(doc, ".//children/asset/last_modified") %>% 
    xml_text
  
  # description is not a mandatory tag for the child elements. Therefore, we 
  # need to identify the indices of those children for which the description 
  # tag exists
  description_text <- xml_find_all(doc, ".//children/asset/description") %>% xml_text
  xpaths <- xml_find_all(doc, ".//children/asset/description") %>% xml_path
  index <- regexec(".*\\[(.*)\\].*", xpaths) %>% 
    regmatches(xpaths, .) %>% sapply(function(x)x[2]) %>% as.numeric
  description <- rep("", length(name))
  description[index] <- description_text
  
  document_df <- data.frame(methodology, name, id, type, description, last_modified_raw, 
                     stringsAsFactors = FALSE) %>%
    mutate(
      date_last_modified = ymd_hms(last_modified_raw),
      year_last_modified = year(date_last_modified),
      quarter_last_modified = quarter(date_last_modified)
    )
  
  # Get document details for each record
  document_details_df <- get_document_details(document_df$id, redmine_key,
                                              alfresco_key)
  document_df %<>% left_join(document_details_df, by = "name")
  
  return(document_df)
}


get_document_details <- function(document_ids, redmine_key, alfresco_key) {
  # Downloads detailed information about documents. 
  #
  # Function serves as a helper function of get_document_list() because date
  # created and LabCase download URL are stored in a separate versions arrary
  # underneath each asset
  #
  # Args:
  #   document_ids: IDs of documents whose detailed informaiton should be 
  #                 downloaded
  #   redmine_key: Redmine API key
  #   alfresco_key: Alfresco API key
  #
  #        
  # Returns:
  #   Data frame containing additional information about every document
  
  details_df <- data.frame()
  
  for (id in document_ids) {
    
    document_details <- GET(paste0(base_url, '/', id, '.xml'),
                            add_headers(
                              `X-Redmine-API-Key` = redmine_key, 
                              `X-Labcase-Token` = alfresco_key
                            ),
                            content_type_xml()
    ) %>% content(as = "text") %>% read_xml
    
    name <- xml_find_one(document_details, ".//name") %>% xml_text
    created_raw <- xml_find_all(document_details, ".//version/created") %>%
      xml_text %>% last
    download_url <- xml_find_all(document_details, ".//version/download_url") %>%
      xml_text %>% first %>% str_extract(".*\\?") %>% 
      str_sub(1, str_length(.) - 1)
    
    details_df %<>% rbind(data.frame(name, created_raw, download_url, 
                               stringsAsFactors = FALSE))
  }
  
  details_df %<>% mutate(
    date_created = ymd_hms(created_raw),
    year_created = year(date_created),
    quarter_created = quarter(date_created)
  )
  return(details_df)
}


map_name_to_acronym <- function(names, mapping) {
  # Maps the complete methodology names to an abbreviated version of the name
  #
  # Args:
  #   names: character vector containing the complete methodology names
  #   mapping: character vector containing the mapping rules. Each entry has
  #            the following form "full name, acronym"
  #         
  # Returns:
  #   Vector with abbreviated methodology names
  
  full_names <- str_extract(mapping, '^.*,') %>%
    str_sub(1, str_length(.) - 1) %>%
    str_trim(side = "both")
  
  acronyms <- str_extract(mapping, ",.*") %>%
    str_sub(2, str_length(.)) %>%
    str_trim(side = "both")
  
  return(mapvalues(names, full_names, acronyms, warn_missing = FALSE))
  
}







