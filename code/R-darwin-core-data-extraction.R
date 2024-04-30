# libraries
source(file = here::here("code", "R-libraries.R"))

# Darwin Core
dwc_data <- read_csv(here("data", "darwin_core_term_versions.csv"))

dwc_data %>% 
  view()
