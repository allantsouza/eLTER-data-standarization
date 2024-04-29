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

#### Defining a general SPARQL query (all terms from the vocabulary: term and definition) ----
query <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dc: <http://purl.org/dc/terms/>

SELECT ?term ?prefLabel ?definition
WHERE {
    ?term skos:prefLabel ?prefLabel .
    OPTIONAL { ?term skos:definition ?definition }
}
'

# NEW GENERAL QUERY (2024-04-29: currently taking too long to run - still needs to be tested to see if the code works)
#### Defining a general SPARQL query (all terms from the vocabulary: columns of interest) ----
query <- '
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX dc: <http://purl.org/dc/terms/>

SELECT ?term ?prefLabel ?altLabel ?altLabelLang ?broader ?definition ?definitionLang ?concept ?deprecated ?exactMatch ?scopeNote ?scopeNoteLang
WHERE {
    ?term skos:prefLabel ?prefLabel .
    OPTIONAL { ?term skos:altLabel ?altLabel }
    OPTIONAL { ?term skos:altLabel ?altLabelLang . FILTER(lang(?altLabelLang) != "") }
    OPTIONAL { ?term skos:broader ?broader }
    OPTIONAL { ?term skos:definition ?definition }
    OPTIONAL { ?term skos:definition ?definitionLang . FILTER(lang(?definitionLang) != "") }
    OPTIONAL { ?term skos:concept ?concept }
    OPTIONAL { ?term owl:deprecated ?deprecated }
    OPTIONAL { ?term skos:exactMatch ?exactMatch }
    OPTIONAL { ?term skos:scopeNote ?scopeNote }
    OPTIONAL { ?term skos:scopeNote ?scopeNoteLang . FILTER(lang(?scopeNoteLang) != "") }
}
'

# Execute the query and store results
tic(msg = "Time to get the result:")
envthes_data <- rdf_query(rdf = graph, query = query)
toc()

envthes_data

