#' prenom_tableau UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prenom_tableau_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_2(
      wellPanel(style = "max-height: 800px",
                radioButtons(ns('finesse'), 'Finesse du tableau',
                             choices = purrr::set_names(c('fin','annee', 'depart', 'rien'),
                                                        c('detail maximaux', 'par ann\u00E9e',
                                                          'par d\u00E9partements', 'tout en m\u00EAme temps')),
                             selected = 'rien'),
                radioButtons(ns('genre'), 'Genre du pr\u00E9nom',
                             choices = purrr::set_names(1:3, c('Gar\u00E7ons', 'Filles', 'Les deux')),
                             selected = '2'),
                sliderInput(ns('annee'), "Ann\u00E9e",
                            min = 1900, max = annee_max,
                            value = c(1900, annee_max),
                            step = 1),
                textInput(ns('departement'), 'D\u00E9partement(s) : ',
                          placeholder = '75, 77, 78, ...',
                          value = NULL)
      )
    ),
    col_10(
      DT::dataTableOutput(ns('table_prenom'))
    )
  )
}

#' prenom_tableau Server Function
#'
#' @noRd
mod_prenom_tableau_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent( input$finesse, {
    r$tableau$finesse <- input$finesse
    r$tableau$variables <- switch(input$finesse, 'fin' = c('annee', 'dpt'), 'annee' = 'annee', 'depart' = 'dpt', 'rien' = character(0))
    } )
  observeEvent( input$annee, {r$tableau$annee <- input$annee[1]:input$annee[2]} )
  observeEvent( input$departement, {r$tableau$departement <- parseur(input$departement) %|||% fc(c(1:95, 971:974))} )
  observeEvent( input$genre, {r$tableau$genre <- switch(as.integer(input$genre %||% 2), 'M', 'F', c('M', 'F'))})

  output$table_prenom <- DT::renderDataTable({
    table_mef(dt = r$prenom_insee, sexe = r$tableau$genre, annee = r$tableau$annee, departement = r$tableau$departement, variables = r$tableau$variables)
  }, rownames= FALSE)

}

## To be copied in the UI
# mod_prenom_tableau_ui("prenom_tableau_ui_1")

## To be copied in the server
# callModule(mod_prenom_tableau_server, "prenom_tableau_ui_1")

