source(file = here::here("code", "R-libraries.R"))

library(stringr)
library(dplyr)

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
elter_template %>% 
  clean_names() %>% 
  select(table, field_name) %>% 
  mutate(case_style = detect_case_style(field_name)) %>% 
  group_by(case_style) %>% 
  tally() %>% 
  mutate(case_style = as_factor(case_style)) %>% 
  ggplot(aes(x = fct_reorder(case_style, n), y = n)) +
  coord_flip() +
  geom_col(fill = "orange", col = "black") +
  labs(x = "Case style", y = "Number of variables") +
  theme_bw() +
  theme(text = element_text(size = 18))
