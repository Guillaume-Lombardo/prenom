globalVariables(
  c(
    'max_files', # max numbers of files rendered
    'total', 'type', 'value', # graphique
    'sexe', 'annais', 'dpt', 'preusuel', 'nombre', 'prenom', 'annee', 'genre', # get_data
    'prenom_insee', 'departement', 'departement_simplifie', 'annee_max', # datasets
    '.' # for pipe injection
  )
)

#' departement simplifie
#'
#' @name imported_data
#' @docType data
#' @references \url{https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/}
#' @keywords data
'departement_simplifie'

#' #' departement
#' #'
#' #' @name imported_data
#' #' @docType data
#' #' @references \url{https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html#admin-express}
#' #' @keywords data
#' 'departement'

#' prenom INSEE
#'
#' @name imported_data
#' @docType data
#' @references \url{https://www.data.gouv.fr/fr/datasets/ficher-des-prenoms-de-1900-a-2018/}
#' @keywords data
'prenom_insee'

#' annee max
#'
#' @name imported_data
#' @docType data
#' @keywords data
'annee_max'

