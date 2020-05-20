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
      wellPanel(style = "overflow-y:scroll; max-height: 800px",
                radioButtons(ns('finesse'), 'Finesse du tableau',
                             choices = setNames(c('fin','annee', 'depart', 'rien'),
                                                c('detail maximaux', 'par année',
                                                  'par départements', 'tout en même temps')),
                             selected = 'rien'),
                sliderInput(ns('annee'), "Année",
                            min = 1900, max = 2017,
                            value = c(1900,2017),
                            step = 1),
                textInput(ns('departement'), 'Departement : ',
                          placeholder = '75, 77, 78, ...',
                          value = NULL)
      )
    ),
    col_10(
      dataTableOutput(ns('table_prenom'))
    )
  )
}

#' prenom_tableau Server Function
#'
#' @noRd
mod_prenom_tableau_server <- function(input, output, session){
  ns <- session$ns

}

## To be copied in the UI
# mod_prenom_tableau_ui("prenom_tableau_ui_1")

## To be copied in the server
# callModule(mod_prenom_tableau_server, "prenom_tableau_ui_1")

