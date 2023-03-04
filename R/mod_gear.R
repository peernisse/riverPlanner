#' gear UI Function
#'
#' @description Gear module UI.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gear_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'display: grid; text-align: center;
             justify-content: space-around; margin: 5px;padding: 20px;',
             #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
             h3('Customize Equipment Checklists')
      )
    ),
    fluidRow(
      column(width = 12, style = 'display: grid; text-align: center;
             justify-content: space-around; margin: 5px;padding: 20px;',
             #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
             h3('Download Equipment Checklists')
      )
    )
  )
}

#' gear Server Functions
#'
#' @noRd
mod_gear_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_gear_ui("gear_1")

## To be copied in the server
# mod_gear_server("gear_1")
