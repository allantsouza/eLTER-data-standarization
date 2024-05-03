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

# mapping terms eLTER x DwC ----
## loading the list of terms from Darwin Core 
dwc_data <- read_csv("data/darwin_core_term_versions.csv")

### selecting only the column with the terms definitions
dwc_candidate_terms <- dwc_data %>% 
  pull(definition)

matching_terms(terms_vector = dwc_candidate_terms, target_term = "sampling")

# trying to match the eLTER data template terms ----
elter_template <- read_excel(here("data", "eLTER_DS_complete.xlsx"))

elter_candidate_terms <- elter_template %>% 
  clean_names() %>% 
  pull(description)

# function to collect more info together from both input datasets -----
dwc_matching <- function(elter_data, dwc_data, number) {
  
  terms_vector <- dwc_data %>% 
    filter(status == "recommended") %>% 
    pull(definition)
  
  target_term <- elter_data %>% 
    clean_names() %>% 
    pull(description)
  
  term_mapping <- matching_terms(terms_vector = terms_vector, 
                                 target_term = target_term[number])
  
  term_mapping %>% 
    mutate(elter_term = elter_data %>% 
             clean_names() %>% 
             slice(number) %>% 
             pull(field_name)) %>% 
    rename(definition = term) %>% 
    inner_join(dwc_data) %>% 
    select(elter_term, similarity, term_localName, label, definition, status, iri) %>% 
    filter(status == "recommended") %>% 
    rename(dwc_term = term_localName,
           dwc_label = label,
           dwc_definition = definition,
           dwc_status = status,
           dwc_iri = iri) %>% 
    mutate(elter_definition = elter_data %>% 
             clean_names() %>% 
             slice(number) %>% 
             pull(description)) %>% 
    relocate(elter_definition, .after = dwc_definition)
}

# testing the function
dwc_matching(elter_data = elter_template, 
             dwc_data = dwc_data, number = 101)

# empty list to store the results
results <- list()

# for loop to iterate over the row numbers
for (i in seq_along(elter_template %>% 
                        pull(`Field Name`)
                    )
     ) {
  # applying the custom function to all row numbers
  results[[i]] <- dwc_matching(elter_template, dwc_data, number = i)
  }

# binding the results
all_terms <- do.call(rbind, results)

all_terms
  
# exporting the results
xlsx::write.xlsx(x = all_terms, sheetName = "eLTER_vs_DwC",
          file = here("data", "data-outputs", "eLTER-DATA-semantic_matching_to_dwc.xlsx"))

# matching eLTER data template with EnvThes -----
envthes_data <- read_parquet(file = here("data", "data-outputs", "envthes_data.parquet"))

### selecting only the column with the terms definitions
envthes_candidate_terms <- envthes_data %>% 
  # can be done by definition or by prefLabel (here is done with the term definition)
  distinct(definition) %>% 
  pull(definition) 

# matching_terms(terms_vector = dwc_candidate_terms, target_term = "sampling")

elter_candidate_terms <- elter_template %>% 
  clean_names() %>% 
  pull(description)

matching_terms(terms_vector = elter_candidate_terms, 
               target_term = envthes_candidate_terms[1])

# function to collect more info together from both input datasets -----
envthes_matching <- function(elter_data, envthes_data, number) {
  
  terms_vector <- envthes_data %>% 
    distinct(term, definition) %>% 
    filter(!is.na(definition) == TRUE) %>% 
    pull(definition)
  
  target_term <- elter_data %>% 
    clean_names() %>% 
    pull(description)
  
  term_mapping <- matching_terms(terms_vector = terms_vector, 
                                 target_term = target_term[number])
  
  term_mapping %>% 
    mutate(elter_term = elter_data %>% 
             clean_names() %>% 
             slice(number) %>% 
             pull(field_name)) %>% 
    rename(definition = term) %>% 
    inner_join(envthes_data %>% rename(iri = term)) %>% 
    select(elter_term, similarity, prefLabel, definition, deprecated, iri) %>% 
    # filter(status == "recommended") %>% 
    rename(envthes_term = prefLabel,
           # dwc_label = label,
           envthes_definition = definition,
           envthes_status = deprecated,
           envthes_iri = iri) %>% 
    mutate(elter_definition = elter_data %>% 
             clean_names() %>% 
             slice(number) %>% 
             pull(description)) %>% 
    relocate(elter_definition, .after = envthes_definition)
}

# testing the function
test_envthes <- envthes_matching(elter_data = elter_template, 
                                 envthes_data = envthes_data, 
                                 number = 1)

test_envthes

# empty list to store the results
envthes_results <- list()

# for loop to iterate over the row numbers
for (i in seq_along(elter_template %>% 
                    pull(`Field Name`)
)
) {
  # applying the custom function to all row numbers
  envthes_results[[i]] <- envthes_matching(elter_template, envthes_data, number = i)
}

# binding the results
all_envthes_terms <- do.call(rbind, envthes_results)

all_envthes_terms

# exporting the results
xlsx::write.xlsx(x = all_envthes_terms, sheetName = "eLTER_vs_EnvThes",
                 file = here("data", "data-outputs", "eLTER-DATA-semantic_matching_to_envthes.xlsx"))
