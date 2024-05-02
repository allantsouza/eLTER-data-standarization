# data loading
source(file = here::here("code", "R-libraries.R"))

# Darwin core mapping ----
eLTER_DS_DarwinCore <- read_excel(here("data", "eLTER_DS_DarwinCore.xlsx"))

eLTER_DS_DarwinCore %>% 
  glimpse()

# source file ----
ds_source <- read_excel(here("data", "HCSupplementalTable3_FullTermList_r2_v4_RW.xlsx"))

ds_source %>% 
  glimpse()

# trying to map terms from EnvThes ----
source(file = here::here("code", "R-semantic-mapping.R"))

## loading the list of terms from EnvThes 
envthes_data <- read_parquet(file = here("data", "data-outputs", "envthes_data.parquet"))

### selecting only the column with the terms definitions
envthes_candidate_terms <- envthes_data %>% 
  # can be done by definition or by prefLabel (here is done with the term definition)
  distinct(definition) %>% 
  pull(definition) 

# running the custom made function to look up for terms with closer semantic meaning
## the function provides the ordered terms with higher similarities with the target_term
matching_terms(terms_vector = envthes_candidate_terms, target_term = "fire")

# trying to map terms from DwC ----
## loading the list of terms from Darwin Core 
dwc_data <- read_csv("data/darwin_core_term_versions.csv")

### selecting only the column with the terms definitions
dwc_candidate_terms <- dwc_data %>% 
  pull(definition)

matching_terms(terms_vector = dwc_candidate_terms, target_term = "sampling")

