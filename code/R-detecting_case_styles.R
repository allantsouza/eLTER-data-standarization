source(file = here::here("code", "R-libraries.R"))

# Updated function to detect naming style
# Function to detect naming style
detect_case_style <- function(input_strings) {
  # Define patterns for each case style
  snake_case_pattern <- "^[a-z]+(?:_[a-z]+)*$"
  camelCase_pattern <- "^[a-z]+(?:[A-Z][a-z]+)*$"
  PascalCase_pattern <- "^[A-Z][a-z]+(?:[A-Z][a-z]+)*$"
  kebab_case_pattern <- "^[a-z]+(?:-[a-z]+)*$"
  flat_case_pattern <- "^[a-z]+$"
  upper_flat_case_pattern <- "^[A-Z]+$"
  Pascal_Snake_Case_pattern <- "^[A-Z][a-z]+(?:_[A-Z][a-z]+)+$"
  camel_Snake_Case_pattern <- "^[a-z]+(?:_[A-Z][a-z]+)+$"
  screaming_snake_case_pattern <- "^[A-Z]+(?:_[A-Z]+)*$"
  train_case_pattern <- "^[A-Z]+(?:-[A-Z]+)*$"
  cobol_case_pattern <- "^[A-Z]+(?:-[A-Z]+)+$"
  
  # Use case_when to handle vectorized input
  case_when(
    str_detect(input_strings, snake_case_pattern) ~ "snake case",
    str_detect(input_strings, camelCase_pattern) ~ "camel case",
    str_detect(input_strings, PascalCase_pattern) ~ "pascal case",
    str_detect(input_strings, kebab_case_pattern) ~ "kebab case",
    str_detect(input_strings, flat_case_pattern) ~ "flat case",
    str_detect(input_strings, upper_flat_case_pattern) ~ "upper flat case",
    str_detect(input_strings, Pascal_Snake_Case_pattern) ~ "pascal snake case",
    str_detect(input_strings, camel_Snake_Case_pattern) ~ "camel snake case",
    str_detect(input_strings, screaming_snake_case_pattern) ~ "screaming snake case",
    str_detect(input_strings, train_case_pattern) ~ "train case",
    str_detect(input_strings, cobol_case_pattern) ~ "cobol case",
    TRUE ~ "unknown case style"
  )
}

# applying on eLTER data template
elter_template <- read_excel(here("data", "eLTER_DS_complete.xlsx"))

elter_variable_cases <- elter_template %>% 
  clean_names() %>% 
  select(table, field_name) %>% 
  mutate(case_style = detect_case_style(field_name))

# exporting
write.xlsx(x = elter_variable_cases, 
           file = here("data", "data-outputs", "eLTER-DATA-variable_case_styles.xlsx"))

# dataviz
p_case_styles <- elter_template %>% 
  clean_names() %>% 
  select(table, field_name) %>%
  mutate(case_style = detect_case_style(field_name)) %>% 
  group_by(case_style) %>% 
  tally() %>% 
  mutate(case_style = as_factor(case_style)) %>% 
  ggplot(aes(x = fct_reorder(case_style, n), y = n)) +
  coord_flip() +
  geom_col(fill = "orange", col = "black") +
  labs(x = "Case style", y = "Number of terms", title = "eLTER terms styles") +
  ggthemes::theme_solarized() +
  theme(text = element_text(size = 14))

p_case_styles

ggsave(plot = p_case_styles, filename = here("images", "eLTER-IMAGE-terms_case_styles.jpeg"), 
       device = "jpeg", units = "cm", dpi = 300, width = 12, height = 9)

# standardizing case style ----
## custom function
convert_case_style <- function(input_string, target_case) {
  # Helper function to split a string into words
  split_to_words <- function(string) {
    str_split(string, "[-_\\s]+|(?<=[a-z])(?=[A-Z])")[[1]]
  }
  # Convert the first letter of each word to uppercase
  capitalize <- function(string) {
    str_to_title(string)
  }
  # Process each input string individually
  result <- map_chr(input_string, function(str) {
    # Split the string into words
    words <- split_to_words(str)
    # Convert to the target case style
    converted <- switch(
      target_case,
      "snake case" = tolower(paste(words, collapse = "_")),
      "camel case" = {
        first_word <- tolower(words[1])
        other_words <- paste(capitalize(words[-1]), collapse = "")
        paste0(first_word, other_words)
      },
      "pascal case" = paste(capitalize(words), collapse = ""),
      "kebab case" = tolower(paste(words, collapse = "-")),
      "flat case" = tolower(paste(words, collapse = "")),
      "upper flat case" = toupper(paste(words, collapse = "")),
      "pascal snake case" = paste(capitalize(words), collapse = "_"),
      "camel snake case" = {
        first_word <- tolower(words[1])
        other_words <- paste(capitalize(words[-1]), collapse = "_")
        paste0(first_word, other_words)
      },
      "screaming snake case" = toupper(paste(words, collapse = "_")),
      "train case" = paste(capitalize(words), collapse = "-"),
      "cobol case" = paste(toupper(words), collapse = "-"),
      NA_character_  # Return NA for any unrecognized case style
    )
    converted
  })
  
  return(result)
}

# converting eLTER data template to camel case style
## TODO save the converted dataset into the data-outputs
elter_template %>% 
  mutate(term = convert_case_style(input_string = `Field Name`, target_case = "camel case")) %>% 
  relocate(term, .after = `Field Name`) %>% 
  mutate(table = convert_case_style(input_string = Table, target_case = "camel case")) %>%
  relocate(table, .after = Table)
