library(sf)
library(tidyverse)
library(leaflet)
library(ggthemes)

# Datasets
nr_r = bcmaps::nr_regions() |>
  sf::st_transform(crs = 4326) |>
  rmapshaper::ms_simplify()

sar = sf::read_sf('app/www/species_ecosystems_at_risk_publically_available.gpkg') |>
  dplyr::mutate(TAX_CLASS = stringr::str_to_title(TAX_CLASS))

sar_terr = sf::read_sf('data/terrestrial_federal_sp.gpkg')

# Summarise by region

sar_by_region = sar |>
  sf::st_join(nr_r |> dplyr::select(Region = REGION_NAME), st_intersects) |>
  st_drop_geometry() |>
  count(ENG_NAME, Region) |>
  group_by(Region) |>
  reframe(all_info = paste0(ENG_NAME, " (",n,")"),
          total_n = sum(n)) |>
  group_by(Region) |>
  summarise(all_info = paste0(all_info, collapse = ', '),
            total_n = mean(total_n))

sar_terr_by_region = sar_terr |>
  sf::st_join(nr_r |>
                dplyr::select(Region = REGION_NAME) |>
                sf::st_transform(3005), st_intersects) |>
  st_drop_geometry() |>
  count(COMMON_NAME_ENGLISH, Region) |>
  group_by(Region) |>
  reframe(all_info = paste0(COMMON_NAME_ENGLISH, " (",n,")"),
          total_n = sum(n)) |>
  group_by(Region) |>
  summarise(all_info = paste0(all_info, collapse = ', '),
            total_n = mean(total_n))

nr_r = nr_r |>
  dplyr::select(Region = REGION_NAME) |>
  left_join(
    sar_by_region |>
      dplyr::rename(aquatic = all_info,
                    aquatic_number = total_n)
  ) |>
  left_join(
    sar_terr_by_region |>
      dplyr::rename(terrestrial = all_info,
                    terrestrial_number = total_n)
  )

min_aquatic_value = min(nr_r$aquatic_number)
max_terr_value = max(nr_r$terrestrial_number)

# my_breaks = 10^c(log10(min_aquatic_value), log10(max_terr_value) / 2, log10(max_terr_value))

my_breaks = 10^c(log10(min_aquatic_value),
                 log10(max_terr_value) / 2,
                 log10(max_terr_value) / 1.5,
                 log10(max_terr_value) / 1.2,
                 log10(max_terr_value))

my_breaks[1] = my_breaks[1] - 1
# my_breaks[5] = my_breaks[5] + 1
my_breaks[5] = 35000
my_breaks = round(my_breaks)

nr_r = nr_r |>
  mutate(aquatic_number_bin = as.numeric(cut(aquatic_number, breaks = my_breaks))) |>
  mutate(terrestrial_number_bin = as.numeric(cut(terrestrial_number, breaks = my_breaks))) |>
  mutate(terrestrial_number_bin = ifelse(is.na(terrestrial_number_bin), 5, terrestrial_number_bin)) |>
  mutate(aquatic_number_bin = as.character(aquatic_number_bin),
         terrestrial_number_bin = as.character(terrestrial_number_bin))

ggplot(nr_r) +
  geom_sf(aes(fill = terrestrial_number_bin)) +
  scale_fill_discrete(labels = my_breaks) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(fill = 'Log10 of \nNumber of\nRecords')



make_plot = function(dat, var, max_value){
  ggplot(dat) +
    geom_sf(aes(fill = log(.data[[var]]))) +
    scale_fill_continuous(limits = c(0, log(max_terr_value)),
                          labels = c("boop","doop")) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank()) +
    # theme_map() +
    labs(fill = 'Number of Records')
    # labs(fill = stringr::str_to_title(stringr::str_replace_all(var,"_"," ")))
}

make_combo_plot = function(dat, var1, var2){
  ggpubr::ggarrange(
    make_plot(dat, var1),
    make_plot(dat, var2),
    common.legend = T,
    legend = 'right'
  )
}

make_combo_plot(nr_r,
                'aquatic_number',
                'terrestrial_number')
