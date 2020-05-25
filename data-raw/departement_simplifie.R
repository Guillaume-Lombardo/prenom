## code to prepare `departement_simplifie` dataset goes here

departement_simplifie <- sf::read_sf('./donnees/OPENSTREETMAP/departements-20140306-100m.shp') %>%
  dplyr::rename(INSEE_DEP = code_insee) %>%
  dplyr::filter(nchar(INSEE_DEP) == 2)

usethis::use_data(departement_simplifie, overwrite = TRUE)
