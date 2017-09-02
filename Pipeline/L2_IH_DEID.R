# Takes a data table 'data', some sort of pii-to-master-id bridge, and some direction, and returns a de-identified data table.


##### FROM PHS #####
ih_deidentify <- function(data, data_id, census_id_bridge, census_id, id_match, pii_match, fuzzy_match, return_id_bridge = FALSE){
  # data - data set containing data that needs to be mathced
  # data_id - character: name of the column containing the identifying id's of the 'data' data set. 
  # census_id_bridge - data: data set matching pii to a master_id column for de-identication
  # census_id: name of the column contianing the identifying id's of the 'census_id_bridge' data set that matches the data_id columne in the 'data' data set
  # id_match - logical: should the data sets be matched by matching the data_id with the census_id?
  # pii_match - logical: should the data sets be matched by matching the first, last, dob, and sex in 'data' with perfect matches in 'census_id_bridge'
  # fuzzy_match - logical: should the data sets be matched by fuzzy matching based on dob and names and/or geography? 
  
  
  #does not yet take into account when a first-last-dob-sex combo matches perfectly to more than one master_id
  
  pii_fields <- colnames(data)[colnames(data) %in% c('first', 'last', 'dob', 'sex')]
  geo_fields <- colnames(data)[colnames(data) %in% c('address', 'city', 'zip', 'state')]
  save_fields <- colnames(data)[!(colnames(data) %in% c(pii_fields, geo_fields, data_id))]
  
  data_unmatched <- data
  data_id_bridge <- tibble(data_id = as.character(c()),
                           'master_id' = as.numeric(c()))
  
  if(id_match){
    if(!(census_id %in% names(data))) return('census_id must match an id column in "data" to do an id_match')
    data_id_bridge <- union_all(data_id_bridge, 
                                data_unmatched %>% 
                                  select_(data_id, census_id) %>% distinct() %>% 
                                  left_join(census_id_bridge, by = census_id) %>% 
                                  select_(data_id, 'master_id') %>% 
                                  filter(!is.na(master_id)))
    
    matched_ids <- data_id_bridge %>% select_(data_id) %>% distinct() %>% .[[1]]
    data_all_ids <- data_unmatched %>% select_(data_id) %>% .[[1]]
    
    data_unmatched <- data_unmatched[!(data_all_ids %in% matched_ids),] %>% distinct()
    
  } 
  
  if(pii_match){
    data_pii <- data_unmatched %>% 
      select_(.dots = c(data_id, pii_fields, geo_fields)) %>% distinct() %>%
      left_join(census_id_bridge %>% 
                  select(-address, -state, -city, -zip), 
                by = pii_fields) %>%
      distinct()
    
    data_id_bridge <- union_all(data_id_bridge, 
                                data_pii %>% 
                                  filter(!is.na(master_id)) %>% 
                                  select_('master_id', data_id) %>% 
                                  distinct())
    
    matched_ids <- data_id_bridge %>% select_(data_id) %>% distinct() %>% .[[1]]
    data_all_ids <- data_pii %>% select_(data_id) %>% .[[1]]
    
    data_unmatched <- data_pii[!(data_all_ids %in% matched_ids),] %>% select(-master_id) %>% distinct()
  }
  
  
  if(fuzzy_match){
    
    data_fuzzy_match <- data_unmatched %>% 
      left_join(census_id_bridge, by = c('dob')) %>% filter(!is.na(master_id)) %>% ### matches on date-of birth only
      distinct() 
    
    data_fuzzy_match <- data_fuzzy_match %>% 
      mutate(last_name_subset = do.call(str_detect, args = list(gsub(" ", "", last.x), gsub(" ", "", last.y))) | ### checks abbreviated last names
               do.call(str_detect, args = list(gsub(" ", "", last.y), gsub(" ", "", last.x))),            
             last_nick_name_len = pmin(nchar(last.x), nchar(last.y)),                                            ### looks at total length of last name
             last_name_distance = do.call(stringdist, args = list(gsub(" ", "", last.y), gsub(" ", "", last.x), 'jw')), ### looks at jw distanct between last names
             
             first_name_subset = do.call(str_detect, args = list(gsub(" ", "", first.x), gsub(" ", "", first.y))) | ### looks at abbreviated first names
               do.call(str_detect, args = list(gsub(" ", "", first.y), gsub(" ", "", first.x))),
             first_nick_name_len = pmin(nchar(first.x), nchar(first.y)),                                             ### overall length of first name
             first_name_distance = do.call(stringdist, args = list(gsub(" ", "", first.y), gsub(" ", "", first.x), 'jw')), ### jw distance between first names
             
             zip_match = if('zip' %in% geo_fields) (.$zip.x == .$zip.y) else TRUE,                  ### do the zips match - zip match is used so that a mis-matched zip is more telling than a matched zip
             address_distance =  if('address' %in% geo_fields) do.call(stringdist, args = list(.$address.y, .$address.x, 'jw')) else 1, ### jw distnace between addresses
             city_match = if('city' %in% geo_fields) (.$city.x == .$city.y) else FALSE) %>%        ### do the cities match
      
      mutate(geo_match = ((zip_match & (address_distance < 0.2501)) | address_distance < 0.1)) %>%           ### does the geography "match"
      mutate(first_name_match = ((first_name_subset & first_nick_name_len > 1) | first_name_distance < 0.1), ### does the first name match
             last_name_match = ((last_name_subset & last_nick_name_len > 1) | last_name_distance < 0.1)) %>% ### does the last names match
      mutate(match = (first_name_match & geo_match) | ### is it a match
               (last_name_match & geo_match) |
               (zip_match & first_name_distance < 0.4 & last_name_match) |
               (zip_match & last_name_distance < 0.2 & first_name_match) |
               (zip_match & first_name_distance < 0.2 & last_name_distance < 0.2) |
               (first_name_match & (last_name_match | last_name_distance < 0.2)) |
               (last_name_match & (first_name_distance < 0.2))) %>% 
      filter(match) %>% select_('master_id', data_id) %>% distinct()
    
    data_id_bridge <- union_all(data_id_bridge, data_fuzzy_match) %>% distinct()
    
  }
  data_id_bridge <- data_id_bridge %>% group_by_(data_id) %>% summarise_at('master_id', first)
  
  if(return_id_bridge) return(data_id_bridge)
  x <- data %>% 
    inner_join(data_id_bridge, by = data_id) %>% 
    select_(.dots = c('master_id', save_fields))
  
  return(x)
}


