#' prenom_carte_graphique UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prenom_carte_graphique_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_2(
      wellPanel(style = "height: 600px",
                textInput(nc('prenom2'), 'Prénom(s) :', value = 'GINETTE'),
                sliderInput(ns('annee2'), "Année",
                            min = 1900, max = 2017,
                            value = c(1900,2017),
                            step = 1),
                textInput(ns('departement2'), 'Departement : ',
                          placeholder = '75, 77, 78, ...'),
                actionButton(ns('carte'), 'Actualiser la carte')
      )
    ),
    col_10(
      fluidRow(
        plotlyOutput(ns('graph_annee')), height = "200px")
      ),
      fluidRow(
        plotlyOutput(ns('graph_dpart'), height = "600px")
      )
  )
}

#' prenom_carte_graphique Server Function
#'
#' @noRd
mod_prenom_carte_graphique_server <- function(input, output, session){
  ns <- session$ns

}

## To be copied in the UI
# mod_prenom_carte_graphique_ui("prenom_carte_graphique_ui_1")

## To be copied in the server
# callModule(mod_prenom_carte_graphique_server, "prenom_carte_graphique_ui_1")

