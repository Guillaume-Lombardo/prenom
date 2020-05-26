#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  r <- rv(
    activation_rate = 1,
    prenom_insee = prenom_insee,
    annee = c(1900, annee_max),
    genre = 'F',
    departement = NULL,

    tableau = rv(
      finesse = 'rien',
      variable = character(0)
    ),

    graphique = rv(
      prenom = 'GINETTE'
    )
  )

  callModule(mod_prenom_tableau_server, 'tableau1', r)
  callModule(mod_prenom_carte_graphique_server, 'graphique1', r)

}
