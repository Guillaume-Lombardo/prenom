#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  r <- rv(
    prenom_insee = prenom_insee,
    tableau = rv(
      finesse = 'rien',
      annee = c(1900, annee_max),
      genre = 'F',
      departement = NULL,
      variable = character(0)
    ),
    graphique = rv(
      prenom = 'GINETTE',
      annee = c(1900, annee_max),
      departement = NULL,
      genre = 'F'
    )
  )

  callModule(mod_prenom_tableau_server, 'tableau1', r)
  callModule(mod_prenom_carte_graphique_server, 'graphique1', r)

}
