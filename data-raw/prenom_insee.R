prenom_insee <- data.table::fread('./donnees/dpt2018.csv', encoding = 'UTF-8', data.table = TRUE) %>%
  dplyr::transmute(
    genre = data.table::fifelse(sexe == 1, 'M', 'F'),
    annee = data.table::fifelse(annais == 'XXXX', 1800, suppressWarnings(as.numeric(annais))),
    dpt,
    prenom = stringi::stri_trans_general(preusuel, "Latin-ASCII"),
    nombre
  ) %>%
  dplyr::filter(nchar(prenom) > 1,
                prenom != '_PRENOMS_RARES',
                annee >= 1900) %>%
  dplyr::group_by(genre, annee, dpt, prenom) %>%
  dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) %>%
  dplyr::ungroup(.) %>%
  data.table::setDT(.)

usethis::use_data(prenom_insee, overwrite = TRUE)
