get_data <- function(refresh = TRUE,
                     url = 'https://www.data.gouv.fr/fr/datasets/r/cf9bbc69-07f1-4246-b643-3477c76794d1',
                     method = 'curl',
                     extra = '-L',
                     extension = 'zip',
                     filetype = 'csv') {

  regexp <- paste0(tolower(unlist(strsplit(gsub('.', '', filetype, fixed = TRUE), ''))),
                   toupper(unlist(strsplit(gsub('.', '', filetype, fixed = TRUE), ''))))
  regexp <- toupper(unlist(strsplit(gsub('.', '', filetype, fixed = TRUE), '')))
  regexp <- paste0('[', ,']')

  utils::download.file(url = url,
                       destfile = fs::path(getOption("prenom_tmp_dir"), 'prenom', ext = extension),
                       method = method,
                       extra = extra)

  utils::unzip(zipfile = fs::path(getOption("prenom_tmp_dir"), 'prenom', ext = 'zip'),
               exdir = getOption("prenom_tmp_dir"))

  data.table::fread(fs::dir_ls(getOption("prenom_tmp_dir"), regexp = '[..][cC][sS][vV]$'), encoding = 'UTF-8')
}
