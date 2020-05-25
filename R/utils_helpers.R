#' fonctions d'aide
#'
#' @name helper_functions
#' @param dt data.frame of prenom.
#' @param sexe 'F', 'M' or c('F', 'M').
#' @param annee either null or a vector of year.
#' @param departement,dpt either null or a vector of departement.
#' @param candidat a vector of names.
#' @param map a sf object of departement.
#' @param texte a character string.
#' @param sep a separator pattern.
#' @param relatif TRUE for relative, FALSE for absolute.
#' @param variables a character(0), 'annee', 'dpt', or c('annee', 'dpt').
proportionneur <- function(dt = prenom_insee, sexe = 'F', annee = NULL, departement = NULL){

  filtre_annee <- annee %||% sort(unique(dt[['annee']]))

  filtre_departement <- departement %||% sort(unique(dt[['dpt']]))

  dt %>%
    dplyr::filter(
      genre %in% sexe,
      annee %in% filtre_annee,
      dpt %in% filtre_departement
    ) %>%
    (function(.x){
      list(
        anne_dpt = .x %>% dplyr::group_by(annee, dpt) %>% dplyr::summarise(total = 1.0 * sum(nombre, na.rm = TRUE)) %>% dplyr::ungroup(),
        dpt      = .x %>% dplyr::group_by(dpt)        %>% dplyr::summarise(total = 1.0 * sum(nombre, na.rm = TRUE)) %>% dplyr::ungroup(),
        annee    = .x %>% dplyr::group_by(annee)      %>% dplyr::summarise(total = 1.0 * sum(nombre, na.rm = TRUE)) %>% dplyr::ungroup()
      )
    })
}

#' @name helper_functions
selecteur <- function(dt = prenom_insee, sexe = 'F', annee = NULL, departement = NULL) {

  filtre_annee <- annee %||% sort(unique(dt[['annee']]))

  filtre_departement <- departement %||% sort(unique(dt[['dpt']]))

  dt %>%
    dplyr::filter(genre %in% sexe,
                  annee %in% filtre_annee,
                  dpt %in% filtre_departement) %>%
    (function(.x){
      list(
        anne_dpt = . %>% dplyr::group_by(prenom, dpt, annee) %>% dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) %>% dplyr::ungroup(),
        dpt      = . %>% dplyr::group_by(prenom, dpt)        %>% dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) %>% dplyr::ungroup(),
        annee    = . %>% dplyr::group_by(prenom, annee)      %>% dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) %>% dplyr::ungroup()
      )
    })
}

#' @name helper_functions
table_mef <- function(dt = prenom_insee, sexe = 'F', annee = NULL, departement = NULL, variables = c('annee', 'dpt')) {
  annee <- annee %||% 1900:annee_max
  departement <- departement %||% fc(c(1:95, 971:974))
  old_names <- c('prenom', variables)
  new_names <- c('Pr\u00E9nom', c(annee = 'Ann\u00E9e', dpt = 'D\u00E9partement')[variables])

  dt %>%
    .[(dt[['annee']] %in% annee &
         dt[['dpt']] %in% departement &
         dt[['genre']] %in% sexe),] %>%
    dplyr::group_by_at(.vars = old_names) %>%
    dplyr::summarise(nombre = sum(nombre, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prenom = stringr::str_to_sentence(prenom)) %>%
    dplyr::arrange(-nombre) %>%
    data.table::setDT() %>%
    data.table::setnames(old = old_names, new = new_names)
}

#' @name helper_functions
graph_annee <- function(dt = prenom_insee, candidat = 'GINETTE', sexe = 'F', departement = NULL) {

  dtp <- dt %>%
    dplyr::filter(genre %in% sexe,
                  prenom %in% candidat,
                  dpt %in% (departement %||% sort(unique(dt[['dpt']])))) %>%
    dplyr::group_by(annee) %>%
    dplyr::summarise(nombre = sum(nombre,na.rm = TRUE)) %>%
    dplyr::ungroup()

  titre <- paste0(paste0(stringr::str_to_sentence(unique(candidat)), collapse = ', '), ' : ', paste0(c('F' = 'Fille', 'M' = 'Gar\u00e7ons')[sexe], collapse = ' et '))

  prop <- proportionneur(prenom_insee, sexe, departement = departement)[['annee']]

  dplyr::left_join(x = dtp, y = prop, by = c('annee')) %>%
    dplyr::transmute(annee, nb_naissance = nombre,  ratio = (1.0 * nombre) / total ) %>%
    tidyr::pivot_longer(-annee, names_to = 'type', values_to = 'value') %>%
    dplyr::mutate(type = forcats::fct_recode(type, 'En nombre de naissances' = 'nb_naissance', 'En part des naissances de l\'ann\u00E9e' = 'ratio')) %>%
    ggplot2::ggplot(data = .) +
    ggplot2::aes(x = annee, y = value) +
    ggplot2::geom_bar(stat = 'identity', fill = 'Firebrick') +
    ggplot2::scale_y_continuous(labels = function(.x) ifelse(0 < .x & .x < 1, scales::percent_format(accuracy = .1)(.x), scales::number_format(accuracy = 1, big.mark = ' ')(.x))) +
    ggplot2::facet_wrap(~type, nrow = 1, scales = 'free_y') +
    ggplot2::labs(title = titre,
                  x = 'ann\u00e9e',
                  y = '')
}

#' @name helper_functions
graph_departement <- function(dt = prenom_insee, relatif = TRUE, candidat = 'GINETTE', annee = NULL, sexe = 'F', dpt = NULL, map = departement_simplifie) {

  filtre_annee <- annee %||% sort(unique(dt[['annee']]))
  filtre_departement <- dpt %||% sort(unique(dt[['dpt']]))

  dtp <- dt %>%
    dplyr::filter(prenom %in% candidat,
                  annee %in% filtre_annee,
                  dpt %in% filtre_departement) %>%
    dplyr::group_by(dpt) %>%
    dplyr::summarise(nombre = sum(nombre,na.rm = TRUE)) %>%
    dplyr::ungroup()

  prop <- proportionneur(sexe = sexe, annee = annee, departement = dpt)[['dpt']]

  titre <- paste0(paste0(stringr::str_to_sentence(unique(candidat)), collapse = ', '), ' : ', paste0(c('F' = 'Fille', 'M' = 'Gar\u00e7ons')[sexe], collapse = ' et '))

  dplyr::left_join(x = dtp, y = prop, by = c('dpt')) %>%
    dplyr::transmute(dpt, nb_naissance = nombre,  ratio = (1.0 * nombre) / total ) %>%
    tidyr::pivot_longer(-dpt, names_to = 'type', values_to = 'value') %>%
    dplyr::filter(type == ifelse(relatif, 'ratio', 'nb_naissance')) %>%
    dplyr::mutate(type = suppressWarnings(forcats::fct_recode(type, 'En nombre de naissances' = 'nb_naissance', 'En part des naissances du d\u00E9partement' = 'ratio'))) %>%
    dplyr::right_join(x = map, y = ., by = c('INSEE_DEP' = 'dpt')) %>%
    ggplot2::ggplot(data = .) +
    ggplot2::aes(fill = value) +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_gradient(low = "LightYellow", high = "Firebrick", labels = ifelse(relatif, scales::percent_format(), scales::number_format(accuracy = 1, big.mark = ' '))) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.direction = "horizontal",
                   legend.key.width = ggplot2::unit(3, 'cm')) +
    ggplot2::labs(title = titre,
                  x = '',
                  y = '')
}

#' @name helper_functions
parseur <- function(texte, sep = ',') {
  if (is.null(texte)) return(NULL)

  res <- stringr::str_split(texte, ',|[[:blank:]]')[[1]] %>%
    stringr::str_replace_all('[^[:digit:]AB]', '') %>%
    stringr::str_trim() %>%
    .[nchar(.) > 1]

  if (length(res) == 0) return(NULL)

  res
}
