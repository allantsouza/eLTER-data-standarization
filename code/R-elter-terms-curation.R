# libraries
source(file = here::here("code", "R-libraries.R"))
source(file = here::here("code", "R-detecting_case_styles.R"))

# manual curation of the data
curated_sv <- read_excel(here("data", "eLTER-DATA-semantic_matching_to_dwc_curatedSV_20240730.xlsx"))

# data checking ----
curated_sv %>% 
  clean_names() %>% 
  dplyr::select(-x1) %>% 
  filter(elter_term == "MIN_LEVEL") %>% 
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
  ## FIELD_NAME ----
  mutate(definition_quality = replace(definition_quality, elter_term == "FIELD_NAME", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "FIELD_NAME", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "FIELD_NAME", "datasetName")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "FIELD_NAME", "datasetName")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "FIELD_NAME", "http://rs.tdwg.org/dwc/terms/datasetName")) %>% 
  ## FLAGQUA ----
  mutate(definition_quality = replace(definition_quality, elter_term == "FLAGQUA", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "FLAGQUA", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "FLAGQUA", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "FLAGQUA", "dataGeneralizations")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "FLAGQUA", "http://rs.tdwg.org/dwc/terms/dataGeneralizations")) %>% 
  ## FLAGSTA ----
  mutate(definition_quality = replace(definition_quality, elter_term == "FLAGSTA", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "FLAGSTA", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "FLAGSTA", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "FLAGSTA", "dataGeneralizations")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "FLAGSTA", "http://rs.tdwg.org/dwc/terms/dataGeneralizations")) %>% 
  ## GEOSPAT_FEATURE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "GEOSPAT_FEATURE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "GEOSPAT_FEATURE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "GEOSPAT_FEATURE", "footprintWKT")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "GEOSPAT_FEATURE", "footprintWKT")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "GEOSPAT_FEATURE", "http://rs.tdwg.org/dwc/terms/footprintWKT")) %>% 
  ## HABITAT_TYPE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "HABITAT_TYPE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "HABITAT_TYPE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "HABITAT_TYPE", "habitat")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "HABITAT_TYPE", "habitat")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "HABITAT_TYPE", "http://rs.tdwg.org/dwc/terms/habitat")) %>% 
  ## HORI_OFFSET ----
  mutate(definition_quality = replace(definition_quality, elter_term == "HORI_OFFSET", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "HORI_OFFSET", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "HORI_OFFSET", "coordinateUncertaintyInMeters")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "HORI_OFFSET", "coordinateUncertaintyInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "HORI_OFFSET", "http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters")) %>% 
  ## HOUR ----
  mutate(definition_quality = replace(definition_quality, elter_term == "HOUR", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "HOUR", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "HOUR", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "HOUR", "eventTime")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "HOUR", "http://rs.tdwg.org/dwc/terms/eventTime")) %>% 
  ## InstHeight ----
  mutate(definition_quality = replace(definition_quality, elter_term == "InstHeight", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "InstHeight", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "InstHeight", "minimumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "InstHeight", "minimumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "InstHeight", "http://rs.tdwg.org/dwc/terms/minimumDistanceAboveSurfaceInMeters")) %>% 
  ## LAB_METHOD ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LAB_METHOD", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LAB_METHOD", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LAB_METHOD", "measurementMethod")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LAB_METHOD", "measurementMethod")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LAB_METHOD", "http://rs.tdwg.org/dwc/terms/measurementMethod")) %>% 
  ## LAT ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LAT", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LAT", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LAT", "decimalLatitude")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LAT", "decimalLatitude")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LAT", "http://rs.tdwg.org/dwc/terms/decimalLatitude")) %>% 
  ## LAYER ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LAYER", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LAYER", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LAYER", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LAYER", "dynamicProperties")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LAYER", "http://rs.tdwg.org/dwc/terms/dynamicProperties")) %>% 
  ## LIST_CODE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LIST_CODE", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LIST_CODE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LIST_CODE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LIST_CODE", "references")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LIST_CODE", "http://rs.tdwg.org/dwc/terms/references")) %>% 
  ## LISTMED ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LISTMED", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LISTMED", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LISTMED", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LISTMED", "bibliographicCitation")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LISTMED", "http://rs.tdwg.org/dwc/terms/bibliographicCitation")) %>% 
  ## LISTSUB ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LISTSUB", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LISTSUB", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LISTSUB", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LISTSUB", "references")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LISTSUB", "http://rs.tdwg.org/dwc/terms/references")) %>% 
  ## LISTTAXA ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LISTTAXA", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LISTTAXA", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LISTTAXA", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LISTTAXA", "acceptedNameUsageID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LISTTAXA", "http://rs.tdwg.org/dwc/terms/acceptedNameUsageID")) %>% 
  ## Local_Habitat_Type ----
  mutate(definition_quality = replace(definition_quality, elter_term == "Local_Habitat_Type", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "Local_Habitat_Type", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "Local_Habitat_Type", "habitat")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "Local_Habitat_Type", "habitat")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "Local_Habitat_Type", "http://rs.tdwg.org/dwc/terms/habitat")) %>% 
  ## LON ----
  mutate(definition_quality = replace(definition_quality, elter_term == "LON", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "LON", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "LON", "decimalLongitude")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "LON", "decimalLongitude")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "LON", "http://rs.tdwg.org/dwc/terms/decimalLongitude")) %>% 
  ## MAX_LEVEL ----
  mutate(definition_quality = replace(definition_quality, elter_term == "MAX_LEVEL", "good")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "MAX_LEVEL", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "MAX_LEVEL", "maximumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "MAX_LEVEL", "maximumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "MAX_LEVEL", "http://rs.tdwg.org/dwc/terms/maximumDistanceAboveSurfaceInMeters")) %>% 
  ## MEDIUM ----
  mutate(definition_quality = replace(definition_quality, elter_term == "MEDIUM", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "MEDIUM", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "MEDIUM", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "MEDIUM", "habitat")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "MEDIUM", "http://rs.tdwg.org/dwc/terms/habitat")) %>% 
  ## METH_DESCR ----
  mutate(definition_quality = replace(definition_quality, elter_term == "METH_DESCR", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "METH_DESCR", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "METH_DESCR", "measurementMethod")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "METH_DESCR", "measurementMethod")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "METH_DESCR", "http://rs.tdwg.org/dwc/terms/measurementMethod")) %>%
  ## MIN_LEVEL ----
  mutate(definition_quality = replace(definition_quality, elter_term == "MIN_LEVEL", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "MIN_LEVEL", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "MIN_LEVEL", "minimumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "MIN_LEVEL", "minimumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "MIN_LEVEL", "http://rs.tdwg.org/dwc/terms/minimumDistanceAboveSurfaceInMeters")) %>%
  rowid_to_column() %>% 
  slice(40:50)
  
  
  
  



