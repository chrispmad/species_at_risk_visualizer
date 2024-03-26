# This script reads in and cleans Greg Wilson's Excel
# document that details key aquatic species-at-risk
# in BC's regions.

library(readxl)
library(tidyverse)

dat = readxl::read_excel(path = 'data/aquatic SAR by region.xlsx')

# Convert data from 'excel format' (e.g. big spaces)
dat_c = dat |>
  set_names(snakecase::to_snake_case) |>
  dplyr::filter(!is.na(species_common_name_population)) |>
  tidyr::fill(nrs_region)

write_csv(dat_c, 'app/www/Aquatic_SAR_GregWilson.csv')
