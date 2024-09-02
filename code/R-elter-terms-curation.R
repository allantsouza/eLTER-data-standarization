# libraries
source(file = here::here("code", "R-libraries.R"))
source(file = here::here("code", "R-detecting_case_styles.R"))

# manual curation of the data
curated_sv <- read_excel(here("data", "eLTER-DATA-semantic_matching_to_dwc_curatedSV_20240730.xlsx"))

# data checking ----
curated_sv %>% 
  clean_names() %>% 
  dplyr::select(-x1) %>% 
  filter(elter_term == "YEAR") %>% 
  slice(1) %>%
  pull(elter_definition)

# data curation ----  
curated_mapping <- curated_sv %>% 
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
  ## MINUTE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "MINUTE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "MINUTE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "MINUTE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "MINUTE", "eventTime")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "MINUTE", "http://rs.tdwg.org/dwc/terms/eventTime")) %>%
  ## MONTH ----
  mutate(definition_quality = replace(definition_quality, elter_term == "MONTH", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "MONTH", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "MONTH", "month")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "MONTH", "month")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "MONTH", "http://rs.tdwg.org/dwc/terms/month")) %>%
  ## NAME ----
  mutate(definition_quality = replace(definition_quality, elter_term == "NAME", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "NAME", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "NAME", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "NAME", "scientificName")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "NAME", "http://rs.tdwg.org/dwc/terms/scientificName")) %>%
  ## northBoundingCoordinate ----
  mutate(definition_quality = replace(definition_quality, elter_term == "northBoundingCoordinate", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "northBoundingCoordinate", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "northBoundingCoordinate", "decimalLatitude")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "northBoundingCoordinate", "decimalLatitude")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "northBoundingCoordinate", "http://rs.tdwg.org/dwc/terms/decimalLatitude")) %>%
  ## NOTES ----
  mutate(definition_quality = replace(definition_quality, elter_term == "NOTES", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "NOTES", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "NOTES", "fieldNotes")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "NOTES", "fieldNotes")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "NOTES", "http://rs.tdwg.org/dwc/terms/fieldNotes")) %>%
  ## numberOfSampleUnit ----
  mutate(definition_quality = replace(definition_quality, elter_term == "numberOfSampleUnit", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "numberOfSampleUnit", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "numberOfSampleUnit", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "numberOfSampleUnit", "sampleSizeValue")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "numberOfSampleUnit", "http://rs.tdwg.org/dwc/terms/sampleSizeValue")) %>%
  ## OperationPeriodSince ----
  mutate(definition_quality = replace(definition_quality, elter_term == "OperationPeriodSince", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "OperationPeriodSince", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "OperationPeriodSince", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "OperationPeriodSince", "parentMeasurementID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "OperationPeriodSince", "http://rs.tdwg.org/dwc/terms/parentMeasurementID")) %>%
  ## ORG_NAME ----
  mutate(definition_quality = replace(definition_quality, elter_term == "ORG_NAME", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "ORG_NAME", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "ORG_NAME", "ownerInstitutionCode")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "ORG_NAME", "ownerInstitutionCode")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "ORG_NAME", "http://rs.tdwg.org/dwc/terms/ownerInstitutionCode")) %>%
  ## PARENT_EVENT_ID ----
  mutate(definition_quality = replace(definition_quality, elter_term == "PARENT_EVENT_ID", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "PARENT_EVENT_ID", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "PARENT_EVENT_ID", "parentEventID")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "PARENT_EVENT_ID", "parentEventID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "PARENT_EVENT_ID", "http://rs.tdwg.org/dwc/terms/parentEventID")) %>%
  ## PHYS_SAMPLE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "PHYS_SAMPLE", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "PHYS_SAMPLE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "PHYS_SAMPLE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "PHYS_SAMPLE", "materialSampleID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "PHYS_SAMPLE", "http://rs.tdwg.org/dwc/terms/materialSampleID")) %>%
  ## plotDimension ----
  mutate(definition_quality = replace(definition_quality, elter_term == "plotDimension", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "plotDimension", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "plotDimension", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "plotDimension", "pointRadiusSpatialFit")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "plotDimension", "http://rs.tdwg.org/dwc/terms/pointRadiusSpatialFit")) %>%
  ## PLOTSIZE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "PLOTSIZE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "PLOTSIZE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "PLOTSIZE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "PLOTSIZE", "sampleSizeValue")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "PLOTSIZE", "http://rs.tdwg.org/dwc/terms/sampleSizeValue")) %>%
  ## Potential_natural_vegetation ----
  mutate(definition_quality = replace(definition_quality, elter_term == "Potential_natural_vegetation", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "Potential_natural_vegetation", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "Potential_natural_vegetation", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "Potential_natural_vegetation", "habitat")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "Potential_natural_vegetation", "http://rs.tdwg.org/dwc/terms/habitat")) %>%
  ## SAMP_EFFORT ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SAMP_EFFORT", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SAMP_EFFORT", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SAMP_EFFORT", "samplingEffort")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SAMP_EFFORT", "samplingEffort")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SAMP_EFFORT", "http://rs.tdwg.org/dwc/terms/samplingEffort")) %>%
  ## SAMP_SIZE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SAMP_SIZE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SAMP_SIZE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SAMP_SIZE", "sampleSizeValue")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SAMP_SIZE", "sampleSizeValue")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SAMP_SIZE", "http://rs.tdwg.org/dwc/terms/sampleSizeValue")) %>%
  ## SAMPLE_ID ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SAMPLE_ID", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SAMPLE_ID", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SAMPLE_ID", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SAMPLE_ID", "materialSampleID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SAMPLE_ID", "http://rs.tdwg.org/dwc/terms/materialSampleID")) %>%
  ## SAMPLING_DESIGN ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SAMPLING_DESIGN", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SAMPLING_DESIGN", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SAMPLING_DESIGN", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SAMPLING_DESIGN", "measurementMethod")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SAMPLING_DESIGN", "http://rs.tdwg.org/dwc/terms/measurementMethod")) %>%
  ## SECOND ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SECOND", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SECOND", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SECOND", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SECOND", "eventTime")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SECOND", "http://rs.tdwg.org/dwc/terms/eventTime")) %>%
  ## SITE_CODE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SITE_CODE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SITE_CODE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SITE_CODE", "locationID")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SITE_CODE", "locationID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SITE_CODE", "http://rs.tdwg.org/dwc/terms/locationID")) %>%
  ## SIZE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SIZE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SIZE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SIZE", "sampleSizeValue")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SIZE", "sampleSizeValue")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SIZE", "http://rs.tdwg.org/dwc/terms/sampleSizeValue")) %>%
  ## SNAME ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SNAME", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SNAME", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SNAME", "locality")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SNAME", "locality")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SNAME", "http://rs.tdwg.org/dwc/terms/locality")) %>%
  ## SOIL_TYPE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SOIL_TYPE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SOIL_TYPE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SOIL_TYPE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SOIL_TYPE", "dynamicProperties")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SOIL_TYPE", "http://rs.tdwg.org/dwc/terms/dynamicProperties")) %>%
  ## southBoundingCoordinate ----
  mutate(definition_quality = replace(definition_quality, elter_term == "southBoundingCoordinate", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "southBoundingCoordinate", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "southBoundingCoordinate", "decimalLatitude")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "southBoundingCoordinate", "decimalLatitude")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "southBoundingCoordinate", "http://rs.tdwg.org/dwc/terms/decimalLatitude")) %>%
  ## SPOOL ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SPOOL", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SPOOL", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SPOOL", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SPOOL", "samplingEffort")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SPOOL", "http://rs.tdwg.org/dwc/terms/samplingEffort")) %>%
  ## STATION_CODE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "STATION_CODE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "STATION_CODE", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "STATION_CODE", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "STATION_CODE", "locationID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "STATION_CODE", "http://rs.tdwg.org/dwc/terms/locationID")) %>%
  ## STYPE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "STYPE", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "STYPE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "STYPE", "footprintWKT")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "STYPE", "footprintWKT")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "STYPE", "http://rs.tdwg.org/dwc/terms/footprintWKT")) %>%
  ## SUBPROG ----
  mutate(definition_quality = replace(definition_quality, elter_term == "SUBPROG", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "SUBPROG", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "SUBPROG", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "SUBPROG", "collectionCode")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "SUBPROG", "http://rs.tdwg.org/dwc/terms/collectionCode")) %>%
  ## TAXA ----
  mutate(definition_quality = replace(definition_quality, elter_term == "TAXA", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "TAXA", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "TAXA", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "TAXA", "taxonID")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "TAXA", "http://rs.tdwg.org/dwc/terms/taxonID")) %>%
  ## TIME ----
  mutate(definition_quality = replace(definition_quality, elter_term == "TIME", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "TIME", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "TIME", "eventDate")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "TIME", "eventDate")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "TIME", "http://rs.tdwg.org/dwc/terms/eventDate")) %>%
  ## TIME_FROM ----
  mutate(definition_quality = replace(definition_quality, elter_term == "TIME_FROM", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "TIME_FROM", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "TIME_FROM", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "TIME_FROM", "eventDate")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "TIME_FROM", "http://rs.tdwg.org/dwc/terms/eventDate")) %>%
  ## TIME_TO ----
  mutate(definition_quality = replace(definition_quality, elter_term == "TIME_TO", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "TIME_TO", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "TIME_TO", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "TIME_TO", "eventDate")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "TIME_TO", "http://rs.tdwg.org/dwc/terms/eventDate")) %>%
  ## TLEVEL ----
  mutate(definition_quality = replace(definition_quality, elter_term == "TLEVEL", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "TLEVEL", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "TLEVEL", "dataGeneralizations")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "TLEVEL", "dataGeneralizations")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "TLEVEL", "http://rs.tdwg.org/dwc/terms/dataGeneralizations")) %>%
  ## TPOOL ----
  mutate(definition_quality = replace(definition_quality, elter_term == "TPOOL", "poor")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "TPOOL", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "TPOOL", "dataGeneralizations")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "TPOOL", "dataGeneralizations")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "TPOOL", "http://rs.tdwg.org/dwc/terms/dataGeneralizations")) %>%
  ## UNIT ----
  mutate(definition_quality = replace(definition_quality, elter_term == "UNIT", "bad")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "UNIT", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "UNIT", "measurementUnit")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "UNIT", "measurementUnit")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "UNIT", "http://rs.tdwg.org/dwc/terms/measurementUnit")) %>%
  ## VALUE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "VALUE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "VALUE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "VALUE", "measurementValue")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "VALUE", "measurementValue")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "VALUE", "http://rs.tdwg.org/dwc/terms/measurementValue")) %>%
  ## VARIABLE ----
  mutate(definition_quality = replace(definition_quality, elter_term == "VARIABLE", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "VARIABLE", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "VARIABLE", "measurementType")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "VARIABLE", "measurementType")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "VARIABLE", "http://rs.tdwg.org/dwc/terms/measurementType")) %>%
  ## VERT_OFFSET ----
  mutate(definition_quality = replace(definition_quality, elter_term == "VERT_OFFSET", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "VERT_OFFSET", FALSE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "VERT_OFFSET", NA)) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "VERT_OFFSET", "minimumDistanceAboveSurfaceInMeters")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "VERT_OFFSET", "http://rs.tdwg.org/dwc/terms/minimumDistanceAboveSurfaceInMeters")) %>% 
  ## westBoundingCoordinate ----
  mutate(definition_quality = replace(definition_quality, elter_term == "westBoundingCoordinate", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "westBoundingCoordinate", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "westBoundingCoordinate", "decimalLongitude")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "westBoundingCoordinate", "decimalLongitude")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "westBoundingCoordinate", "http://rs.tdwg.org/dwc/terms/decimalLongitude")) %>% 
  ## YEAR ----
  mutate(definition_quality = replace(definition_quality, elter_term == "YEAR", "ok")) %>% 
  mutate(mapping_possible = replace(mapping_possible, elter_term == "YEAR", TRUE)) %>% 
  mutate(dwc_mapping = replace(dwc_mapping, elter_term == "YEAR", "year")) %>% 
  mutate(dwc_best_match = replace(dwc_best_match, elter_term == "YEAR", "year")) %>% 
  mutate(dwc_best_match_iri = replace(dwc_best_match_iri, elter_term == "YEAR", "http://rs.tdwg.org/dwc/terms/year"))

# Proposing new elter term names ----
curated_mapping <- curated_mapping %>% 
  rename(camelCase_term = new_elter_term) %>% 
  mutate(proposed_elter_term = NA) %>% 
  relocate(proposed_elter_term, .after = camelCase_term) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "ABS_POSITION", "absolutePosition")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "AGG_METHOD", "aggregationMethod")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "ALTITUDE", "altitude")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "altitudeMax", "maximumAltitude")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "altitudeMin", "minimumAltitude")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "CODE", "code")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "CODE_URL", "codeUrl")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "COMMENT", "comment")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "Country", "country")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "DAY", "day")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "DEFINITON", "definiton")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "eastBoundingCoordinate", "eastBoundingCoordinate")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "EUNIS_Habitat_Type", "eunisHabitatType")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "EVENT_EFFORT", "eventEffort")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "EVENT_ID", "eventId")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "FIELD_METHOD", "fieldMethod")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "FIELD_NAME", "fieldName")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "FLAGQUA", "flagQuality")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "FLAGSTA", "flagStatus")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "GEOSPAT_FEATURE", "geoSpatialFeature")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "HABITAT_TYPE", "habitatType")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "HORI_OFFSET", "horizontalOffset")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "HOUR", "hour")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "InstHeight", "InstrumentHeight")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LAB_METHOD", "labMethod")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LAT", "decimalLatitude")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LAYER", "layer")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LIST_CODE", "listCode")) %>% 
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LISTMED", NA)) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LISTSUB", NA)) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LISTTAXA", NA)) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "Local_Habitat_Type", "localHabitatType")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "LON", "decimalLongitude")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "MAX_LEVEL", "maximumLevel")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "MEDIUM", "medium")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "METH_DESCR", "methodDescription")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "MIN_LEVEL", "minimumLevel")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "MINUTE", "minute")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "MONTH", "month")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "NAME", "name")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "northBoundingCoordinate", "northBoundingCoordinate")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "NOTES", "notes")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "numberOfSampleUnit", "numberOfSampleUnit")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "OperationPeriodSince", "operationalSince")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "ORG_NAME", "organizationName")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "PARENT_EVENT_ID", "parentEventId")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "PHYS_SAMPLE", "physicalSample")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "plotDimension", "plotDimension")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "PLOTSIZE", "plotSize")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "Potential_natural_vegetation", "potentialNaturalVegetation")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SAMP_EFFORT", "samplingEffort")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SAMP_SIZE", "sampleSize")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SAMPLE_ID", "sampleId")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SAMPLING_DESIGN", "samplingDesign")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SECOND", "second")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SITE_CODE", "siteCode")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SIZE", "size")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SNAME", "siteName")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SOIL_TYPE", "soilType")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "southBoundingCoordinate", "southBoundingCoordinate")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SPOOL", "spatialPool")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "STATION_CODE", "stationCode")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "STYPE", "spatialType")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "SUBPROG", "subProgram")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "TAXA", "taxa")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "TIME", "time")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "TIME_FROM", "timeFrom")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "TIME_TO", "timeTo")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "TLEVEL", "temporalLevel")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "TPOOL", "temporalPool")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "UNIT", "unit")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "VALUE", "value")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "VARIABLE", "variable")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "VERT_OFFSET", "verticalOffset")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "westBoundingCoordinate", "westBoundingCoordinate")) %>%
  mutate(proposed_elter_term = replace(proposed_elter_term, elter_term == "YEAR", "year"))

curated_mapping

# exporting
write_csv(x = curated_mapping, na = "", file = here::here("data", "data-outputs", "eLTER-DwC-curated-mapping_v01.csv"))

#TODO classify the mapping following proper nomenclature. Maybe take a look at: https://mapping-commons.github.io/semantic-mapping-vocabulary/
  
# some descriptive stats and visualizations ----
p_mapping_viability <- curated_mapping %>% 
  group_by(mapping_possible) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = "", y = count, fill = mapping_possible)) +
  coord_flip() +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("#DA20DF","#25DF20")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "eLTER to DwC mapping", 
       x = "Viability", y = "Terms", fill = "eLTER/DwC mapping") +
  ggthemes::theme_solarized() +
  theme(legend.position = "bottom", text = element_text(size = 14))

ggsave(plot = p_mapping_viability, filename = here("images", "eLTER-IMAGE-mapping_viability.jpeg"), 
       device = "jpeg", units = "cm", dpi = 300, width = 12, height = 9)  

## descriptive stats - mapping viability  
curated_mapping %>%
  group_by(mapping_possible) %>% 
  summarise(count = n()) %>% 
  mutate(percent = (count/(40+55))*100)

p_term_quality <- curated_mapping %>% 
  mutate(definition_quality = as_factor(definition_quality)) %>% 
  mutate(definition_quality = fct_relevel(definition_quality, "good", "ok", "poor", "bad")) %>% 
  distinct(elter_term, definition_quality, mapping_possible) %>% 
  group_by(definition_quality, mapping_possible) %>% 
  tally() %>% 
  ggplot(aes(x = mapping_possible, y = n, fill = definition_quality)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_viridis_d(option = "H") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "eLTER terms definition assessment", x = "Mapping viability", y = "Terms", 
       fill = "Definition quality") +
  ggthemes::theme_solarized() +
  theme(legend.position = "bottom", text = element_text(size = 14))
  
ggsave(plot = p_term_quality, filename = here("images", "eLTER-IMAGE-eLTER_term_quality_assessment.jpeg"), 
       device = "jpeg", units = "cm", dpi = 300, width = 15, height = 12)  


curated_mapping %>% 
  mutate(definition_quality = as_factor(definition_quality)) %>% 
  mutate(definition_quality = fct_relevel(definition_quality, "good", "ok", "poor", "bad")) %>% 
  distinct(elter_term, definition_quality, mapping_possible) %>% 
  group_by(definition_quality) %>% 
  tally() %>% 
  mutate(percent = round((n/(76)*100), 1))


curated_mapping %>% 
  filter(definition_quality == "bad") %>% 
  select(elter_term, proposed_elter_term, elter_definition, dwc_best_match) %>% 
  filter(elter_term == "TAXA") %>% 
  pull(elter_definition)

curated_mapping %>% 
  filter(mapping_possible == FALSE)
