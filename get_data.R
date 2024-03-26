library(sf)
library(tidyverse)
library(bcdata)
library(gdalUtilities) # for ogr2ogr function

if(!dir.exists('app/www/boundaries')){
  dir.create('app/www/boundaries')
}


# Species at Risk - Publically available
if(!file.exists('app/www/species_ecosystems_at_risk_publically_available_no_insects.gpkg')){
  sp = read_sf('W:/CMadsen/Projects/SpeciesAtRisk/species_ecosystems_at_risk_publically_available.shp')

  sp = st_transform(sp, 4326)

  sp = st_make_valid(sp)

  # sp_simple = sf::st_simplify(sp, dTolerance = 100)

  sp_simple = rmapshaper::ms_simplify(sp, keep = 0.25)

  sp_simple = sf::st_make_valid(sp_simple)

  sp |>
    st_drop_geometry() |>
    count(ENG_NAME, sort = T) |>
    dplyr::mutate(total = sum(n))

  sp_simple |>
    st_drop_geometry() |>
    count(ENG_NAME, sort = T) |>
    dplyr::mutate(total = sum(n))

  st_area(sp |> dplyr::filter(ENG_NAME == 'Northern Red-legged Frog') |> dplyr::summarise())
  st_area(sp_simple |> dplyr::filter(ENG_NAME == 'Northern Red-legged Frog') |> dplyr::summarise())

  sp_no_insects = sp_simple |>
    filter(TAX_CLASS != 'insects')

  # write_sf(sp_simple, 'app/www/species_ecosystems_at_risk_publically_available.gpkg')
  write_sf(sp_no_insects, 'app/www/species_ecosystems_at_risk_publically_available_no_insects.gpkg')
}

# nr districts
if(!file.exists('app/www/boundaries/nr_districts.gpkg')){
  nr_districts = bcmaps::nr_districts() |>
    sf::st_transform(crs = 4326) |>
    sf::st_simplify(dTolerance = 100) |>
    dplyr::select(DISTRICT_NAME)

  sf::write_sf(nr_districts, 'app/www/boundaries/nr_districts.gpkg')
}

# nr regions
if(!file.exists('app/www/boundaries/nr_regions.gpkg')){
  nr_regions = bcmaps::nr_regions() |>
    sf::st_transform(crs = 4326) |>
    sf::st_simplify(dTolerance = 100) |>
    dplyr::select(REGION_NAME)

  sf::write_sf(nr_regions, 'app/www/boundaries/nr_regions.gpkg')
}

# Read the GDB that the Feds provided us with for Critical Habitat.
# This is for the whole country!
if(!file.exists('app/www/federal_critical_habitat_just_BC.gpkg')){
  crit_hab = sf::st_read('../CNF/data/federal_critical_habitat.gpkg')

  ensure_multipolygons <- function(X) {
    tmp1 <- tempfile(fileext = ".gpkg")
    tmp2 <- tempfile(fileext = ".gpkg")
    st_write(X, tmp1)
    ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
    Y <- st_read(tmp2)
    st_sf(st_drop_geometry(X), geom = st_geometry(Y))
  }

  ## Ensure there are no 'multisurface' geometries - convert to multipolygons
  crit_hab_defs_mp <- ensure_multipolygons(crit_hab)

  crit_hab_defs_mp = sf::st_simplify(crit_hab_defs_mp, dTolerance = 100)

  crit_hab_defs_mp = sf::st_transform(crit_hab_defs_mp,3005)

  # Cut the polygon to just BC.
  crit_hab_bc = sf::st_intersection(crit_hab_defs_mp,bcmaps::bc_bound())

  # Reproject to WGS 84.
  crit_hab_bc = sf::st_transform(crit_hab_bc, 4326)

  ggplot() +
    geom_sf(data = bcmaps::bc_bound()) +
    geom_sf(data = crit_hab_bc)

  sf::write_sf(crit_hab_bc, 'app/www/federal_critical_habitat_just_BC.gpkg')
}

if(!file.exists('app/www/DFO_SARA_species_merged.gpkg')){
  # Grab the excessively large file that lists species occurrence data
  # for DFO SARA-listed species within BC's borders.
  dfo_sara_in_bc = sf::read_sf('../CNF/data/dfo_sara_in_BC_boundary.gpkg')

  # Unique species list...
  unique(dfo_sara_in_bc$Common_Name_EN)

  # The following lines describe a polygon that I traced in QGIS that includes
  # many of the highly detailed stream geometries for Bull Trout in NE British Columbia.
  # bulltrout_poly = dfo_sara_qgis_simplified |>
  #   dplyr::mutate(the_area = st_area(geom)) |>
  #   arrange(desc(the_area)) |>
  #   slice(1)

  bulltrout_poly_from_code = structure(
    list(
      structure(
        list(
          structure(
            c(-130.7648949, -130.7245641,
              -130.6741507, -130.6707898, -130.490982, -130.4187228, -130.4775384,
              -130.5329932, -130.4237641, -130.5111474, -130.5279518, -130.6472636,
              -130.7010379, -130.7665753, -130.664068, -130.5918088, -130.5481172,
              -130.4052792, -130.3666289, -130.2674826, -130.190182, -130.0507049,
              -129.5986647, -129.6709239, -129.5667362, -129.3886088, -128.8743921,
              -128.8323809, -128.6794602, -128.5265395, -128.2509462, -128.2929574,
              -128.2391831, -128.2643898, -128.2072546, -128.1602021, -128.0190446,
              -127.9383831, -127.8678043, -127.7232859, -127.750173, -127.6980792,
              -127.4813015, -127.5367563, -127.6106959, -127.5417976, -127.4308881,
              -127.3636702, -127.3182982, -127.2241932, -127.0359831, -126.9469194,
              -126.9284345, -126.9939719, -127.0006937, -126.9099496, -126.8931451,
              -126.9519607, -127.0645507, -127.0914378, -127.1956256, -127.2930915,
              -127.3754334, -127.3871965, -127.5081887, -127.6157373, -127.7703384,
              -127.735049, -127.6039742, -127.261163, -127.1704188, -126.9956524,
              -126.8057619, -126.7486267, -126.6209127, -126.5452926, -126.4763943,
              -126.4209395, -126.3486803, -126.2478535, -126.1537484, -126.2260077,
              -126.2243272, -126.1655116, -126.2781015, -126.437744, -126.6091496,
              -126.817525, -126.6393976, -126.3402781, -126.1991205, -125.9873842,
              -125.6512948, -125.6210468, -125.4261149, -125.1975741, -124.7640188,
              -123.8868255, -123.7490289, -123.8061641, -123.30203, -122.9592188,
              -123.0029104, -122.6802646, -122.3576188, -122.2198221, -122.1122735,
              -122.0316121, -121.6182221, -121.4266512, -121.3594333, -121.211554,
              -121.0451897, -120.8351339, -120.8116076, -120.7275853, -120.7393484,
              -120.625078, -120.5830668, -120.4083003, -120.2268121, -120.2083272,
              -120.2822668, -120.2385752, -120.0940568, -120.072211, -120.0789327,
              -119.9613015, -119.9428165, -119.8100612, -119.737802, -119.5764791,
              -119.7915763, -119.9528992, -119.9915495, -120.0906959, -120.1495115,
              -120.3192367, -120.3427629, -120.324278, -120.3478043, -120.2940299,
              -119.9528992, -119.959621, -119.9781059, -120.2352143, -120.196564,
              -120.4217439, -120.5494579, -120.625078, -120.7544724, -120.8637015,
              -121.0014981, -121.0922422, -121.2098735, -121.3174221, -121.4400948,
              -121.5241171, -121.5005909, -121.3762378, -121.1930691, -121.1426557,
              -121.1191294, -121.1392948, -121.1897082, -121.2401216, -121.3392679,
              -121.3980836, -121.4031249, -121.3224635, -121.2199562, -121.1359339,
              -121.1006445, -121.0166221, -121.0283853, -120.9191562, -120.7645551,
              -120.4654355, -120.5158489, -120.4856009, -120.4049394, -120.2957104,
              -120.1427897, -119.9629819, -119.9495383, -120.1007786, -120.6687696,
              -120.9544456, -121.1981104, -121.3947227, -121.6232635, -122.4668478,
              -123.3860523, -124.3245819, -124.8539227, -126.7133373, -126.7133373,
              -128.0744993, -128.1282736, -128.2963183, -128.3349686, -128.3786602,
              -128.6189641, -128.6777798, -128.8827943, -128.8861552, -130.0372613,
              -130.7648949, 59.9459261, 59.678913, 59.6415641, 59.5428993,
              59.5446028, 59.3943585, 59.288103, 59.2408671, 59.1815147, 59.0702746,
              59.0011069, 58.9690698, 58.8745168, 58.8023439, 58.6872531, 58.5735334,
              58.45241, 58.4559267, 58.5095129, 58.5121462, 58.4568058, 58.3952142,
              58.3749534, 58.2787766, 58.20448, 58.3061563, 58.1681597, 58.2301476,
              58.2372251, 58.1042877, 58.1956248, 58.3105704, 58.3555626, 58.3916914,
              58.4075411, 58.3855257, 58.413703, 58.3705473, 58.3784779, 58.3193969,
              58.2274932, 58.1619549, 58.1158286, 58.027846, 58.0109357, 57.9093056,
              57.8700028, 57.9003769, 57.8557003, 57.7930597, 57.7939554, 57.8619583,
              57.8243936, 57.7527333, 57.6962006, 57.6647554, 57.5882749, 57.5558345,
              57.5450147, 57.6008834, 57.6206863, 57.6017831, 57.5621446, 57.4637632,
              57.4167354, 57.2171019, 57.1624681, 56.8779858, 56.6846584, 56.6809663,
              56.5922472, 56.6088979, 56.6698878, 56.7538182, 56.763951, 56.7160266,
              56.6615768, 56.6449493, 56.6652708, 56.6440253, 56.5774103, 56.5061016,
              56.4485622, 56.3993061, 56.3388105, 56.3248364, 56.3229728, 56.272621,
              56.1043022, 56.0546008, 55.8126859, 55.4684138, 55.3768602, 55.2984899,
              55.1086146, 55.0893849, 54.9795989, 54.9660957, 54.8849809, 54.7668798,
              54.5861586, 54.5549857, 54.3811507, 54.1947808, 54.304743, 54.43592,
              54.4339652, 54.4007196, 54.3635308, 54.3968066, 54.488664, 54.4964721,
              54.43592, 54.4544858, 54.4056103, 54.3870224, 54.2880712, 54.264523,
              54.1446115, 54.1012793, 54.0993086, 54.0519832, 54.0243518, 53.9492596,
              53.9166117, 53.874032, 53.8284337, 53.7629232, 53.7102421, 53.6574951,
              53.5807448, 53.5667752, 56.0864921, 56.9110277, 57.2698378, 57.2562064,
              57.2116421, 57.2462069, 57.3016247, 57.3578656, 57.4167354, 57.4737036,
              57.5134384, 57.7814145, 58.1858816, 58.2027092, 58.3026246, 58.3899299,
              58.5174122, 58.5787901, 58.5682758, 58.5226774, 58.5007339, 58.4954654,
              58.5103907, 58.5340825, 58.5463609, 58.6068128, 58.745718, 58.8267082,
              58.876254, 58.9387369, 58.9656045, 59.0037032, 59.0287907, 59.0322496,
              59.0339798, 59.0504032, 59.1160204, 59.1944262, 59.1961473, 59.1909836,
              59.2365696, 59.2374292, 59.1892622, 59.1513697, 59.0840915, 59.2374292,
              59.4285636, 59.48066, 59.5122203, 59.5130728, 59.48066, 59.4678568,
              59.8911772, 59.9088751, 59.9021342, 59.8625033, 59.863347, 59.8726263,
              59.9029769, 59.9172994, 59.9055048, 59.8949704, 59.8953918, 59.9055048,
              59.9442428, 59.9610714, 59.9871387, 59.9913412, 59.9652772, 59.9459261,
              59.9450845, 59.9871387, 59.9854575, 59.9450845, 59.9450845, 59.9459261
            ), dim = c(208L, 2L))), class = c("XY", "POLYGON", "sfg"))),
    class = c("sfc_POLYGON", "sfc"), precision = 0,
    bbox = structure(c(xmin = -130.7665753, ymin = 53.5667752, xmax = -119.5764791, ymax = 59.9913412), class = "bbox"),
    crs = st_crs('EPSG:4326'),
    n_empty = 0L)

  bulltrout_poly_from_code = bulltrout_poly_from_code |>
    st_as_sf() |>
    dplyr::mutate(in_poly = T)

  # I think we can remove the Shape_Length and Shape_Area columns.
  dfo_sara_in_bc = dfo_sara_in_bc |>
    dplyr::select(-c(Shape_Length,Shape_Area))

  # Update CRS
  dfo_sara_in_bc = sf::st_transform(dfo_sara_in_bc, 4326)

  # And simplify those rows into a single row, with the geometry of the QGIS
  # Can we simplify the geometries at all??
  # dfo_sara_in_bc_s = rmapshaper::ms_simplify(dfo_sara_in_bc)
  # The above line did NOT work.
  # dfo_sara_in_bc_s = sf::st_simplify(dfo_sara_in_bc, dTolerance = 50)
  # The above line worked.
  dfo_sara_in_bc_s = sf::st_simplify(dfo_sara_in_bc, dTolerance = 100)
  # The above line worked.
  # dfo_sara_in_bc_v_s = sf::st_simplify(dfo_sara_in_bc, dTolerance = 500)
  # The above line GREATLY simplifies the DFO data.

  # Find all the bulltrout rows that touch the simplifying QGIS polygon I made.
  dfo_sara_in_bc_qgis_intersect = sf::st_join(
    dfo_sara_in_bc_s,
    bulltrout_poly_from_code
  )

  object.size(dfo_sara_in_bc)
  object.size(dfo_sara_in_bc_s)
  # object.size(dfo_sara_in_bc_v_s)

  # sf::write_sf(dfo_sara_in_bc, 'app/www/DFO_SARA_species_occurrences.gpkg')
  sf::write_sf(dfo_sara_in_bc_s, 'data/DFO_SARA_species_occurrences_100m.gpkg')

  # How about merging geometries by species?
  dfo_sara_merged = dfo_sara_in_bc_s |>
    dplyr::group_by(Common_Name_EN,Population_EN,
                    Common_Name_FR,Population_FR,
                    Scientific_Name,Taxon,
                    Eco_Type,SARA_Status) |>
    dplyr::summarise() |>
    dplyr::ungroup()

  sf::write_sf(dfo_sara_merged, 'data/DFO_SARA_species_merged.gpkg')
}

# And how about a further simplification of bull trout polygons, since they
# are the overwhelming majority of the dataset and VERY detailed?

dfo_sara_occ_dat = read_sf('data/DFO_SARA_occ_data_QGIS_simplified.gpkg')

dfo_sara_occ_dat_s = rmapshaper::ms_simplify(dfo_sara_occ_dat)

plot(dfo_sara_occ_dat_s$geom)

dfo_sara_occ_dat_v_s = rmapshaper::ms_simplify(dfo_sara_occ_dat, keep = 0.015)

dfo_sara_occ_dat_v_s = st_make_valid(dfo_sara_occ_dat_v_s)

write_sf(dfo_sara_occ_dat_s,'app/www/DFO_SARA_occ_data_QGIS_simplified.gpkg')

write_sf(dfo_sara_occ_dat_v_s,'app/www/DFO_SARA_occ_data_QGIS_very_simplified.gpkg')

# if(!file.exists('app/www/DFO_SARA_species_remerged.gpkg')){
#
#   dfo_sara_merged = sf::read_sf('app/www/DFO_SARA_species_merged.gpkg')
#
#   st_is_valid(dfo_sara_merged)
#
#   dfo_sara_merged_as_polys = st_cast(dfo_sara_merged, 'POLYGON')
#
#   dfo_sara_merged_s = rmapshaper::ms_simplify(dfo_sara_merged,
#                                               keep_shapes = TRUE,
#                                               explode = TRUE)
#
#   # Some of the geometries might have gotten messed up from ms_simplify
#   validity_check = st_is_valid(dfo_sara_merged_s)
#
#   sum(!validity_check)
#   # invalid geometries.
#
#   rows_to_fix_geometry = which(!validity_check)
#
#   resimplified = rmapshaper::ms_simplify(st_buffer(dfo_sara_merged_as_polys[rows_to_fix_geometry,],dist = 0.0001),
#                                          keep_shapes = TRUE)
#
#   st_is_valid(resimplified)
#
#   dfo_sara_merged_corrected = dplyr::bind_rows(
#     # The good geometries from the first round of simplification
#     dfo_sara_merged_s[-rows_to_fix_geometry,],
#     # The geometries we had to resimplify to fix.
#     resimplified
#   )
#
#   # Recombine these polygons into multipolygon by species and other columns?
#   dfo_sara_remerged = dfo_sara_merged_corrected |>
#     dplyr::group_by(across(-dplyr::contains('geom'))) |>
#     dplyr::summarise(.groups = 'drop')
#
#   st_is_valid(dfo_sara_remerged)
#
#   sf::write_sf(dfo_sara_remerged, 'app/www/DFO_SARA_species_occurrences_remerged.gpkg')
#
#
#   # Or do we want to just buffer thos problematic polygons, remerge, then simplify again and see
#   # if it worked? Slightly different workflow from that above...
#   dfo_sara_merged_as_polys[!validity_check,] = sf::st_set_geometry(
#     dfo_sara_merged_as_polys[!validity_check,],
#     st_buffer(dfo_sara_merged_as_polys[!validity_check,], dist = 10)$geom
#   )
#
#   dfo_sara_back_to_multipolys = dfo_sara_merged_as_polys |>
#     dplyr::group_by(across(-dplyr::contains('geom'))) |>
#     dplyr::summarise(.groups = 'drop')
#
#   # One last round of simplification...
#   dfo_sara_back_to_multipolys_s = rmapshaper::ms_simplify(dfo_sara_back_to_multipolys)
#
#   dfo_sara_back_to_multipolys_s = st_make_valid(dfo_sara_back_to_multipolys_s)
#
#   sf::write_sf(dfo_sara_back_to_multipolys_s, 'app/www/DFO_SARA_species_occurrences_second_workflow.gpkg')
#
#
#
#
#   leaflet() |>
#     addTiles() |>
#     # addPolygons(data = dfo_sara_merged |> st_transform(4326)) |>
#     addPolygons(data = dfo_sara_remerged |> st_transform(4326),
#                 fillColor = 'purple', fillOpacity = 0.75, col = 'purple') |>
#     addPolygons(data = dfo_sara_back_to_multipolys_s |> st_transform(4326),
#                 fillColor = 'red', fillOpacity = 0.5, color = 'red')
#
#
#
#
#
#
#
#
#
#
#
#
#
#   # For these invalid geometries, try buffering by a tiny bit, then resimplifying.
#   for(i in rows_to_fix_geometry){
#     # Attempt to resimplify the geometry...
#     resimplified = rmapshaper::ms_simplify(st_buffer(dfo_sara_merged_as_polys[rows_to_fix_geometry,],dist = 0.0001),
#                                            keep_shapes = TRUE)
#     # If that worked, replace the geometry for that row in the dfo_sara_merged_s object.
#
#
#   }
#   rmapshaper::ms_simplify(st_buffer(dfo_sara_merged_as_polys[!validity_check,][1,],dist = 0.0001),
#                           keep_shapes = TRUE)
#
#   plot(dfo_sara_merged_as_polys[!validity_check,][1,]$geom)
#   plot(dfo_sara_merged_s[!validity_check,][1,]$geom, add = T, col = 'red')
#
#   test = rmapshaper::ms_simplify(st_buffer(dfo_sara_merged_as_polys[!validity_check,][1,],dist = 0.0001),
#                                  keep_shapes = TRUE)
#
#   st_area(dfo_sara_merged_s[!validity_check,][1,])
#   st_area(test)
#   st_is_valid(test)
#
#   plot(dfo_sara_merged_as_polys[!validity_check,][1,]$geom)
#   plot(test$geom, add = T, col = 'red')
#
#   # Replace with unsimplified geometries.
#   dfo_sara_merged_s_bad_geos = dfo_sara_merged_s[!validity_check,]
#   dfo_sara_merged_s_good_geos = dfo_sara_merged_s[validity_check,]
#
#   dfo_sara_remerged = dplyr::bind_rows(
#     dfo_sara_merged_s_good_geos,
#     dfo_sara_merged_as_polys[!validity_check,]
#   )
#
#   plot(dfo_sara_merged_s_bad_geos[1,])
#   plot(dfo_sara_merged_as_polys[!validity_check,][1,])
#
#   dfo_sara_remerged_mp = dfo_sara_remerged |>
#     dplyr::group_by(across(-dplyr::contains('geom'))) |>
#     dplyr::summarise(.groups = 'drop')
#
#   st_is_valid(dfo_sara_remerged_mp)
#
#   # dfo_sara_merged_bt = dfo_sara_merged |>
#   #   dplyr::filter(Common_Name_EN == 'Bull Trout')
#   #
#   # # We need to make sure the geometries are valid before and after simplifying!
#   # sf::st_is_valid(dfo_sara_merged_bt)
#   #
#   # dfo_sara_merged_bt_ms_s = dfo_sara_merged_bt |>
#   #   rmapshaper::ms_simplify()
#   #
#   # sf::st_is_valid(dfo_sara_merged_bt_ms_s)
#   #
#   # dfo_sara_merged_test = clean_up_geoms(dfo_sara_merged_bt_ms_s)
#   #
#   # sf::st_is_valid(dfo_sara_merged_test)
#
#   # test = readRDS('C:/Users/CMADSEN/Downloads/invalid_geometry.rds')
#   #
#   # test = st_as_sf(test)
#   #
#   # test_c = clean_up_geoms(test)
#
#   # dfo_sara_remerged = dplyr::bind_rows(
#   #   dfo_sara_merged |> dplyr::filter(Common_Name_EN != 'Bull Trout'),
#   #   dfo_sara_merged_test
#   # )
#
#   dfo_sara_remerged_mp = sf::st_transform(dfo_sara_remerged_mp, 4326)
#
#   st_is_valid(dfo_sara_remerged_mp)
#
#   sf::write_sf(dfo_sara_remerged_mp,
#                'app/www/DFO_SARA_species_remerged.gpkg')
# }
#
# file.remove('app/www/DFO_SARA_species_occurrences.gpkg')
# file.remove('app/www/DFO_SARA_species_occurrences_100m.gpkg')
# file.remove('app/www/DFO_SARA_species_merged.gpkg')

# # Split by species
# if(!dir.exists('app/www/DFO_SARA_sp_occ')){
#   dir.create('app/www/DFO_SARA_sp_occ')
# }
#
# # For each species
# unique(dfo_sara_in_bc_s$Common_Name_EN) |>
#   purrr::iwalk(~ {
#     write_sf(dfo_sara_in_bc_s[dfo_sara_in_bc_s$Common_Name_EN == .x,],
#              paste0('app/www/DFO_SARA_sp_occ/',snakecase::to_snake_case(.x),"_occ.gpkg")
#     )
#   })
#
# bull_troat_SARA = dfo_sara_in_bc_s[dfo_sara_in_bc_s$Common_Name_EN == 'Bull Trout',]
# bull_troat_SARA |>
#   sf::st_drop_geometry() |>
#   dplyr::select(-Shape_Length,-Shape_Area) |>
#   dplyr::distinct()

# # Species at Risk - Federal list from CDC (Terrestrial!)
# sp_terr = bcdc_query_geodata('critical-habitat-for-federally-listed-species-at-risk-posted-') |>
#   collect()


if(!exists('sp_no_insects')) sp_no_insects = sf::read_sf('app/www/species_ecosystems_at_risk_publically_available_no_insects.gpkg')
if(!exists('crit_hab_bc')) crit_hab_bc = sf::read_sf('app/www/federal_critical_habitat_just_BC.gpkg')
if(!exists('dfo_sara_occ_dat')) dfo_sara_occ_dat = sf::read_sf('app/www/DFO_SARA_occ_data_QGIS_very_simplified.gpkg')


# Do spatial matches for everything with the administrative boundaries gpkgs
boundary_files = list.files('app/www/boundaries')

for(i in 1:length(boundary_files)){

  boundary_name = boundary_files[i]

  boundary = sf::read_sf(paste0('app/www/boundaries/',boundary_name)) |>
    sf::st_transform(4326)

  boundary_col_name = names(boundary)[1]

  # ===================
  # CDC Aquatic SAR (no insects)

  initial_sp_no_insects_nrow = nrow(sp_no_insects)

  sp_no_insects = sp_no_insects |>
    sf::st_join(
      boundary |> dplyr::select(all_of(boundary_col_name))
    )

  if(nrow(sp_no_insects) > initial_sp_no_insects_nrow){

    col_names = names(sp_no_insects)

    col_names = col_names[!col_names %in% c('geometry','geom',boundary_col_name)]

    sp_no_insects = sp_no_insects |>
      # sf::st_drop_geometry() |>
      dplyr::group_by(across(-all_of(boundary_col_name))) |>
      dplyr::summarise(!!sym(boundary_col_name) := paste0(!!sym(boundary_col_name), collapse = ', '),
                       .groups = 'drop')
  }

  # ===============
  # DFO Critical Habitat

  initial_crit_hab_bc_nrow = nrow(crit_hab_bc)

  crit_hab_bc = crit_hab_bc |>
    sf::st_join(
      boundary |> dplyr::select(all_of(boundary_col_name))
    )

  if(nrow(crit_hab_bc) > initial_crit_hab_bc_nrow){

    col_names = names(crit_hab_bc)

    col_names = col_names[!col_names %in% c('geometry','geom',boundary_col_name)]

    crit_hab_bc = crit_hab_bc |>
      # sf::st_drop_geometry() |>
      dplyr::group_by(across(-all_of(boundary_col_name))) |>
      dplyr::summarise(!!sym(boundary_col_name) := paste0(!!sym(boundary_col_name), collapse = ', '),
                       .groups = 'drop')
  }

  # ===============
  # DFO SARA-listed species occurrence data

  initial_dfo_sara_occ_dat_nrow = nrow(dfo_sara_occ_dat)

  dfo_sara_occ_dat = dfo_sara_occ_dat |>
    sf::st_join(
      boundary |> dplyr::select(all_of(boundary_col_name))
    )

  if(nrow(dfo_sara_occ_dat) > initial_dfo_sara_occ_dat_nrow){

    col_names = names(dfo_sara_occ_dat)

    col_names = col_names[!col_names %in% c('geometry','geom',boundary_col_name)]

    dfo_sara_occ_dat = dfo_sara_occ_dat |>
      # sf::st_drop_geometry() |>
      dplyr::group_by(across(-all_of(boundary_col_name))) |>
      dplyr::summarise(!!sym(boundary_col_name) := paste0(!!sym(boundary_col_name), collapse = ', '),
                       .groups = 'drop')
  }

  print(paste0('Finished ',boundary_name))
}

sf::write_sf(sp_no_insects,'app/www/species_ecosystems_at_risk_publically_available_no_insects.gpkg')
sf::write_sf(crit_hab_bc,'app/www/federal_critical_habitat_just_BC.gpkg')
sf::write_sf(dfo_sara_occ_dat,'app/www/DFO_SARA_occ_data_QGIS_very_simplified.gpkg')

