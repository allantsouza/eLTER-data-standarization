# libraries
source(file = here::here("code", "R-libraries.R"))

# semantic mapping of terms ----
## 2024-04-30: this is a very rough test so far, more polishing is needed 
install.packages(c("textTinyR", "dplyr", "data.table"))

library(data.table)

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

glove_embeddings <- read_glove(here("data", "glove.6B.300d.txt"))

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

# The provided strings
string1 <- "GEOCORDINATES DEPICTING THE LATITUDE IN WSG84"
string2 <- "area where the sample was taken"
string3 <- "water temperature"
string4 <- "soil moisture"
string5 <- "LONGITUDE IN WSG84"
string6 <- "Leaf area index"
string7 <- "coastal zone"
string8 <- "Acid rain is a rain or any other form of precipitation that is unusually acidic, meaning that it possesses elevated levels of hydrogen ions. It can have harmful effects on plants, aquatic animals, and infrastructure through the process of wet deposition. Acid rain is caused by emissions of sulfur dioxide and nitrogen oxides which react with the water molecules in the atmosphere to produce acids. Governments have made efforts since the 1970s to reduce the release of sulfur dioxide into the atmosphere with positive results. Nitrogen oxides can also be produced naturally by lightning strikes and sulfur dioxide is produced by volcanic eruptions. The chemicals found in acid rain can cause paint to peel and stone statues to begin to appear old and worn down, which reduces their value and beauty."
string9 <- "Planting of new trees and, particularly, of native plants in disturbed sites where the vegetation cover has been destroyed, to stabilize the land surface from wind and water erosion and to reclame the land for other uses. Revegetation practices are employed in mined lands, roadsides, parks, wetlands, utility corridors, riparian areas, etc."

# List of candidate strings
candidates <- list(string1, string2, string3, string4, string5, string6, string7, string8, string9)

string_of_interest <- "abundance"

# Convert the string of interest to a vector
vec_interest <- string_to_vector(string_of_interest, glove_embeddings)

# Calculate similarities
similarities <- sapply(candidates, function(cand) {
  vec_cand <- string_to_vector(cand, glove_embeddings)
  if (is.null(vec_cand) || is.null(vec_interest)) return(NA)
  cosine_similarity(vec_cand, vec_interest)
})

# Finding the string with the highest similarity
best_match_idx <- which.max(similarities)
best_match <- candidates[[best_match_idx]]

cat("The string that best matches the string of interest is:", best_match, "\n")

similarities
