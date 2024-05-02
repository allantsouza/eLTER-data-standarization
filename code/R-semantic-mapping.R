# libraries
source(file = here::here("code", "R-libraries.R"))

# semantic mapping of terms ----

# glove embeddings: https://nlp.stanford.edu/projects/glove/
# Function to read GloVe embeddings
read_glove <- function(file_path) {
  # Reading the file as a data table
  glove_dt <- data.table::fread(file_path, header = FALSE, quote = "")
  
  # Extracting words and vectors
  words <- glove_dt$V1
  vectors <- as.matrix(glove_dt[, -1, with = FALSE])
  
  # Creating a named list with words as keys and vectors as values
  embeddings <- setNames(asplit(vectors, 1), words)
  
  return(embeddings)
}

# getting the model (take several minutes)
## DO NOT FORGET TO DELETE THE FILES AFTER USAGE (there is a code line to do that at the end of this script)
source(here("code", "R-semantic-model-extracting.R"))

# glove_embeddings <- read_glove(here("data", paste0(txt_file)))
glove_embeddings <- read_glove(here("data", "glove.6B.300d.txt"))

gc() # garbage collection

# Function to convert a string to a vector representation
string_to_vector <- function(text, embeddings) {
  # Tokenizing and filtering words present in the embeddings
  words <- strsplit(text, "\\W+")[[1]]
  words <- words[tolower(words) %in% names(embeddings)]
  
  if (length(words) == 0) return(NULL)
  
  # Averaging the vectors of the words
  vectors <- do.call(rbind, lapply(words, function(w) embeddings[[tolower(w)]]))
  vector_mean <- colMeans(vectors)
  
  return(vector_mean)
}

cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}


# making it in a function ----
matching_terms <- function(terms_vector, target_term) {
  # Convert the string of interest to a vector
  vec_interest <- string_to_vector(target_term, glove_embeddings)
  
  # Calculate similarities
  similarities <- sapply(terms_vector, function(cand) {
    vec_cand <- string_to_vector(cand, glove_embeddings)
    if (is.null(vec_cand) || is.null(vec_interest)) return(NA)
    cosine_similarity(vec_cand, vec_interest)
  })
  
  # # Finding the string with the highest similarity
  # best_match_idx <- which.max(similarities)
  # best_match <- terms_vector[[best_match_idx]]
  
  # making a tibble from the terms
  df <- tibble(term = names(similarities), similarities)
  
  # processing the tibble to select the top 5 terms with higher similarity
  df %>% 
    filter(is.na(similarities) == FALSE) %>% 
    arrange(-similarities) %>% 
    slice(1:5) %>% # top 5 terms with higher similarities
    rename(similarity = similarities)

}

# testing the function ----
## loading the list of terms from EnvThes 
envthes_data <- read_parquet(file = here("data", "data-outputs", "envthes_data.parquet"))

### selecting only the column with the terms definitions
candidate_terms <- envthes_data %>% 
  # can be done by definition or by prefLabel (here is done with the term definition)
  distinct(definition) %>% 
  pull(definition) 

## testing the function ----
# ### it will provide the list of the top 5 terms with higher similarity
# matching_terms(terms_vector = candidate_terms, target_term = "fire")
# 
# ## comparing with the input candidates and checking the recommended term
# envthes_data %>% 
#   filter(definition == "An unplanned, unwanted wildland fire") %>% # replace the term here
#   distinct(term, prefLabel, definition) %>% 
#   pull(prefLabel)

# removing the large txt file from the semantic model ----
# file.remove(paste0(here("data"), "/", txt_file))
file.remove(paste0(here("data"), "/", "glove.6B.50d.txt"))
file.remove(paste0(here("data"), "/", "glove.6B.100d.txt"))
file.remove(paste0(here("data"), "/", "glove.6B.200d.txt"))
file.remove(paste0(here("data"), "/", "glove.6B.300d.txt"))
