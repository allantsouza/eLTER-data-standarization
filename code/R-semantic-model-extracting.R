# URL of the zip file
zip_url <- "https://nlp.stanford.edu/data/glove.6B.zip"
# Local destination for the downloaded zip file
zip_file <- "glove.6B.zip"
# extracted file name
txt_file <- gsub("zip", "txt", zip_file)

# Download the zip file
# download.file(zip_url, destfile = paste0(here("data"), "/", zip_file), method = "wget")
options(timeout = max(600, getOption("timeout")))

download.file(zip_url, 
              destfile = paste0(here("data"), "/", zip_file), 
              method = "libcurl", 
              cacheOK = FALSE)

options(timeout = max(60, getOption("timeout")))

# Extract the zip file
unzip(zipfile = paste0(here("data"), "/", zip_file), exdir = paste0(here("data")))

# removing the zip files
file.remove(paste0(here("data"), "/", zip_file))

# garbage collection
gc()
