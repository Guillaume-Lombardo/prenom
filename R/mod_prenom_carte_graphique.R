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
      wellPanel(style = "height: 800px",
                textInput(ns('prenom'), 'Pr\u00E9nom(s) :', value = 'GINETTE'),
                radioButtons(ns('genre'), 'Genre du pr\u00E9nom',
                             choices = purrr::set_names(1:3, c('Gar\u00E7ons', 'Filles', 'Les deux')),
                             selected = '2'),
                sliderInput(ns('annee'), "Ann\u00E9e",
                            min = 1900, max = annee_max,
                            value = c(1900, annee_max),
                            step = 1),
                textInput(ns('departement'), 'D\u00E9partement : ',
                          placeholder = '75, 77, 78, ...')
                # ,
                # actionButton(ns('carte'), 'Actualiser la carte')
      )
    ),
    col_10(
      plotly::plotlyOutput(ns('graph_annee'), height = "200px"),
      shiny::plotOutput(ns('graph_dpart'), height = "600px", )


      # fluidRow(
      # ),
      # fluidRow(
      #   col_6(
      #   ),
      #   col_6(
      #   )
      # )
    )
  )
}

#' prenom_carte_graphique Server Function
#'
#' @noRd
#' @import patchwork
mod_prenom_carte_graphique_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent( input$prenom, {r$graphique$prenom <- toupper(input$prenom)} )
  observeEvent( input$annee, {r$graphique$annee <- input$annee[1]:input$annee[2]} )
  observeEvent( input$departement, {r$graphique$departement <- parseur(input$departement) %||% fc(c(1:95, 971:974))} )
  observeEvent( input$genre, {r$graphique$genre <- switch(as.integer(input$genre), 'M', 'F', c('M', 'F'))})

  output$graph_annee <- plotly::renderPlotly({
    graph_annee(dt = r$prenom_insee, candidat = r$graphique$prenom, sexe = r$graphique$genre, departement = r$graphique$depatement) %>% plotly::ggplotly()
  })

  output$graph_dpart <- shiny::renderPlot({
    p1 <- graph_departement(dt = r$prenom_insee, relatif = FALSE, candidat = r$graphique$prenom, annee = r$graphique$annee, sexe = r$graphique$genre, dpt = r$graphique$departement, map = departement_simplifie)
    p2 <- graph_departement(dt = r$prenom_insee, relatif = TRUE, candidat = r$graphique$prenom, annee = r$graphique$annee, sexe = r$graphique$genre, dpt = r$graphique$departement, map = departement_simplifie)

    p1 - p2
  })

}

## To be copied in the UI
# mod_prenom_carte_graphique_ui("prenom_carte_graphique_ui_1")

## To be copied in the server
# callModule(mod_prenom_carte_graphique_server, "prenom_carte_graphique_ui_1")

