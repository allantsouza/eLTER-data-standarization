# libraries
source(file = here::here("code", "R-libraries.R"))
source(file = here::here("code", "R-detecting_case_styles.R"))

# manual curation of the data
curated_sv <- read_excel(here("data", "eLTER-DATA-semantic_matching_to_dwc_curatedSV_20240730.xlsx"))

curated_sv %>% 
  clean_names() %>% 
  dplyr::select(-x1) %>% 
  filter(elter_term == "FIELD_METHOD") %>% 
  slice(1) %>%
  pull(elter_definition)

# data curation ----  
curated_sv %>% 
  clean_names() %>%
  dplyr::select(-x1) %>% 
  distinct(elter_term, elter_definition) %>% 
  # putting all terms in camel case
  mutate(new_elter_term = convert_case_style(input_string = elter_term, target_case = "camel case")) %>% 
  relocate(new_elter_term, .after = elter_term) %>% 
  # adding a column with recommendations
  mutate(definition_quality = NA) %>% 
  # adding a column to check if it is viable to do the DwC mapping
  mutate(mapping_possible = NA) %>% 
  # adding a column for DwC mapping
  mutate(dwc_mapping = NA) %>% 
  # adding a column to indicate the term which is closer in DwC
  mutate(dwc_best_match = NA) %>% 
  mutate(dwc_best_match_iri = NA) %>% 
  # manual suggestions to each term
  ## ABS_POSITION ----
  mutate(definition_quality = replace(definition_quality, elter_term == "ABS_POSITION", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "ABS_POSITION", FALSE)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "ABS_POSITION", "verbatimCoordinates")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "ABS_POSITION", "http://rs.tdwg.org/dwc/terms/verbatimCoordinates")) %>% 
  ## AGG_METHOD ----
  mutate(definition_quality = replace(definition_quality, elter_term == "AGG_METHOD", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "AGG_METHOD", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "AGG_METHOD", "dataGeneralizations")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "AGG_METHOD", "dataGeneralizations")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "AGG_METHOD", "http://rs.tdwg.org/dwc/terms/dataGeneralizations")) %>% 
  ## ALTITUDE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "ALTITUDE", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "ALTITUDE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "ALTITUDE", "verbatimElevation")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "ALTITUDE", "verbatimElevation")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "ALTITUDE", "http://rs.tdwg.org/dwc/terms/verbatimElevation")) %>% 
  ## altitudeMax ----
  mutate(definition_quality = replace(definition_quality, elter_term == "altitudeMax", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "altitudeMax", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "altitudeMax", "maximumElevationInMeters")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "altitudeMax", "maximumElevationInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "altitudeMax", "http://rs.tdwg.org/dwc/terms/maximumElevationInMeters")) %>% 
  ## altitudeMin ----
  mutate(definition_quality = replace(definition_quality, elter_term == "altitudeMin", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "altitudeMin", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "altitudeMin", "minimumElevationInMeters")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "altitudeMin", "minimumElevationInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "altitudeMin", "http://rs.tdwg.org/dwc/terms/minimumElevationInMeters")) %>% 
  ## CODE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "CODE", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "CODE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "CODE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "CODE", "catalogNumber")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "CODE", "http://rs.tdwg.org/dwc/terms/catalogNumber")) %>% 
  ## CODE_URL ----
  mutate(definition_quality = replace(definition_quality, elter_term == "CODE_URL", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "CODE_URL", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "CODE_URL", "measurementID")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "CODE_URL", "measurementID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "CODE_URL", "http://rs.tdwg.org/dwc/terms/measurementID")) %>% 
  ## COMMENT ----
  mutate(definition_quality = replace(definition_quality, elter_term == "COMMENT", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "COMMENT", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "COMMENT", "locationRemarks")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "COMMENT", "locationRemarks")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "COMMENT", "http://rs.tdwg.org/dwc/terms/locationRemarks")) %>% 
  ## Country ----
  mutate(definition_quality = replace(definition_quality, elter_term == "Country", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "Country", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "Country", "countryCode")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "Country", "countryCode")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "Country", "http://rs.tdwg.org/dwc/terms/countryCode")) %>% 
  ## DAY ----
  mutate(definition_quality = replace(definition_quality, elter_term == "DAY", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "DAY", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "DAY", "day")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "DAY", "day")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "DAY", "http://rs.tdwg.org/dwc/terms/day")) %>% 
  ## DEFINITION ----
  mutate(definition_quality = replace(definition_quality, elter_term == "DEFINITON", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "DEFINITON", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "DEFINITON", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "DEFINITON", "measurementMethod")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "DEFINITON", "http://rs.tdwg.org/dwc/terms/measurementMethod")) %>% 
  ## eastBoundingCoordinate ----
  mutate(definition_quality = replace(definition_quality, elter_term == "eastBoundingCoordinate", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "eastBoundingCoordinate", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "eastBoundingCoordinate", "decimalLongitude")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "eastBoundingCoordinate", "decimalLongitude")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "eastBoundingCoordinate", "http://rs.tdwg.org/dwc/terms/decimalLongitude")) %>% 
  ## EUNIS_Habitat_Type ----
  mutate(definition_quality = replace(definition_quality, elter_term == "EUNIS_Habitat_Type", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "EUNIS_Habitat_Type", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "EUNIS_Habitat_Type", "habitat")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "EUNIS_Habitat_Type", "habitat")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "EUNIS_Habitat_Type", "http://rs.tdwg.org/dwc/terms/habitat")) %>% 
  ## EVENT_EFFORT ----
  mutate(definition_quality = replace(definition_quality, elter_term == "EVENT_EFFORT", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "EVENT_EFFORT", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "EVENT_EFFORT", "sampleSizeValue")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "EVENT_EFFORT", "sampleSizeValue")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "EVENT_EFFORT", "http://rs.tdwg.org/dwc/terms/sampleSizeValue")) %>% 
  ## EVENT_ID (there are 2 terms with the same name) ----
  mutate(definition_quality = replace(definition_quality, elter_term == "EVENT_ID", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "EVENT_ID", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "EVENT_ID", "eventID")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "EVENT_ID", "eventID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "EVENT_ID", "http://rs.tdwg.org/dwc/terms/eventID")) %>% 
  ## FIELD_METHOD ----
  mutate(definition_quality = replace(definition_quality, elter_term == "FIELD_METHOD", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "FIELD_METHOD", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "FIELD_METHOD", "measurementMethod")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "FIELD_METHOD", "measurementMethod")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "FIELD_METHOD", "http://rs.tdwg.org/dwc/terms/measurementMethod")) %>% 
  slice(10:20)
  
  
  
  



