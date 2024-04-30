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

