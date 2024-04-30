# sourcing libraries
source(file = here::here("code", "R-libraries.R"))

# EnvThes ----
## TURTLE
### Define the URL to the TURTLE file
turtle_url <- "https://raw.githubusercontent.com/LTER-Europe/EnvThes/main/EnvThes.ttl"

#### Load the TURTLE file into a graph
graph <- rdf_parse(turtle_url, format = "turtle")

#### Defining a specific SPARQL query (ex: "invasive alien species") ----
query <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dc: <http://purl.org/dc/terms/>

SELECT ?term ?prefLabel ?altLabel ?broader ?definition ?concept ?deprecated ?exactMatch ?scopeNote
WHERE {
    ?term skos:prefLabel "invasive alien species"@en .
    OPTIONAL { ?term skos:prefLabel ?prefLabel }
    OPTIONAL { ?term skos:altLabel ?altLabel }
    OPTIONAL { ?term skos:broader ?broader }
    OPTIONAL { ?term skos:definition ?definition }
    OPTIONAL { ?term skos:concept ?concept }
    OPTIONAL { ?term owl:deprecated ?deprecated }
    OPTIONAL { ?term skos:exactMatch ?exactMatch }
    OPTIONAL { ?term skos:scopeNote ?scopeNote }
}
'

#### Execute the query
tic(msg = "Time to get the result:") # recording the time elapsed to run the function (tictoc package)
term_query <- rdf_query(graph, query)
toc()

#### View the results
term_query

## one by one approach ----
### SPARQL query to retrieve terms and labels ----
query_labels <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?term ?prefLabel
WHERE {
    ?term skos:prefLabel ?prefLabel .
}
'

# Execute the query and store results
terms_labels <- rdf_query(graph, query_labels)

terms_labels

### SPARQL query to retrieve terms and definition ----
query_definition <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?term ?definition
WHERE {
    ?term skos:definition ?definition .
}
'

terms_definition <- rdf_query(graph, query_definition)

### SPARQL query to retrieve terms and deprecated ----
query_deprecated <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?term ?deprecated
WHERE {
    ?term owl:deprecated ?deprecated .
}
'
terms_deprecated <- rdf_query(graph, query_deprecated)

### SPARQL query to retrieve terms and altLabel ----
query_altLabel <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?term ?altLabel
WHERE {
    ?term skos:altLabel ?altLabel .
}
'
terms_altLabel <- rdf_query(graph, query_altLabel)

### SPARQL query to retrieve terms and broader ----
query_broader <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?term ?broader
WHERE {
    ?term skos:broader ?broader .
}
'
terms_broader <- rdf_query(graph, query_broader)

### SPARQL query to retrieve terms and exactMatch ----
query_exactMatch <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?term ?exactMatch
WHERE {
    ?term skos:exactMatch ?exactMatch .
}
'
terms_exactMatch <- rdf_query(graph, query_exactMatch)

### SPARQL query to retrieve terms and scopeNote ----
query_scopeNote <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?term ?scopeNote
WHERE {
    ?term skos:scopeNote ?scopeNote .
}
'
terms_scopeNote <- rdf_query(graph, query_scopeNote)

## putting all data together ----
envthes_data <- list(terms_labels, 
                     terms_altLabel, 
                     terms_broader, 
                     terms_definition,
                     terms_exactMatch, 
                     terms_deprecated,
                     terms_scopeNote) %>% 
  reduce(full_join, by = "term")

envthes_data <- envthes_data %>% 
  filter(deprecated != TRUE | is.na(deprecated) == TRUE) # removing the deprecated

# exporting the extracted data
## saving as parquet
write_parquet(envthes_data, here("data", "data-outputs", "envthes_data.parquet"))
## saving as csv
write_csv(envthes_data, here("data", "data-outputs", "envthes_data.csv"))
