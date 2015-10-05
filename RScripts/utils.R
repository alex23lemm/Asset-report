


get_document_list <- function(folder_id) {
  
  doc <- GET(paste0(base_url, 
                          '/projects/prime/alfresco/documents/',
                          folder_id, '.xml'),
                         add_headers(
                           `X-Redmine-API-Key` = redmine_key, 
                           `X-Labcase-Token` = alfresco_key
                         ),
                         content_type_xml()
  ) %>% content(as = "text") %>% read_xml
  
  name <- xml_find_all(doc, ".//children/asset/name") %>% xml_text
  id <- xml_find_all(doc, ".//children/asset/id") %>% xml_text
  type <- xml_find_all(doc, ".//children/asset/type") %>% xml_text
  last_modified <- xml_find_all(doc, ".//children/asset/last_modified") %>% 
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
  
  data <- data.frame(name, id, type, last_modified, description, 
                     stringsAsFactors = FALSE) %>%
    mutate(
      date = ymd_hms(last_modified),
      year = year(date),
      quarter = quarter(date)
    )
  return(data)
}



get_document_details <- function(document_ids) {
  
  data <- data.frame()
  
  for (id in document_ids) {
    
    document_details <- GET(paste0(base_url,
                                   '/projects/prime/alfresco/documents/',
                                   id, '.xml'),
                            add_headers(
                              `X-Redmine-API-Key` = redmine_key, 
                              `X-Labcase-Token` = alfresco_key
                            ),
                            content_type_xml()
    ) %>% content(as = "text") %>% read_xml
    
    name <- xml_find_one(document_details, ".//name") %>% xml_text
    first_upload <- xml_find_all(document_details, ".//version/created") %>%
      xml_text %>% last
    download_url <- xml_find_all(document_details, ".//version/download_url") %>%
      xml_text %>% first %>% str_extract(".*\\?") %>% 
      str_sub(1, str_length(.) - 1)
    
    data %<>% rbind(data.frame(name, first_upload, download_url, 
                               stringsAsFactors = FALSE))
  }
  
  return(data)
}







