## code to prepare `annee_max` dataset goes here

annee_max <- data.table::fread('./donnees/dpt2018.csv', encoding = 'UTF-8', data.table = TRUE) %>%
  .[['annais']] %>%
  (function(.x) suppressWarnings(as.numeric(.x))) %>%
  max(., na.rm = TRUE)

usethis::use_data(annee_max, overwrite = TRUE)
