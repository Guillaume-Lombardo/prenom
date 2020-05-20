get_data <- function(refresh = FALSE,
                     url = 'https://www.data.gouv.fr/fr/datasets/r/cf9bbc69-07f1-4246-b643-3477c76794d1',
                     method = 'curl',
                     extra = '-L',
                     extension = 'zip',
                     filetype = 'csv',
                     encoding = 'UTF-8') {

  regexp <- filetype %>%
    gsub('.', '', ., fixed = TRUE) %>%
    strsplit(., '') %>%
    unlist() %>%
    {paste0(tolower(.), toupper(.))} %>%
    paste0('[', . ,']', collapse = '') %>%
    paste0('[.]', .)

  if (refresh) {
    utils::download.file(url = url,
                         destfile = fs::path(getOption("prenom_tmp_dir"), 'prenom', ext = extension),
                         method = method,
                         extra = extra)

    utils::unzip(zipfile = fs::path(getOption("prenom_tmp_dir"), 'prenom', ext = extension),
                 exdir = getOption("prenom_tmp_dir"))

    if (fs::dir_exists('./donnees')) fs::file_copy(path = fs::dir_ls(getOption("prenom_tmp_dir"), regexp = regexp),
                                                   new_path = fs::path('./donnees', fs::path_file((fs::dir_ls(getOption("prenom_tmp_dir"), regexp = regexp)))))
  }


  data.table::fread(fs::dir_ls(getOption("prenom_tmp_dir"), regexp = regexp), encoding = encoding, data.table = TRUE) %>%
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
}
