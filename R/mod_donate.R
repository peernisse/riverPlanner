#' donate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_donate_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('donate'), label = "Support", class = "cta-btn align-middle")
  )
}

#' donate Server Functions
#'
#' @noRd
mod_donate_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$donate, {
      showModal(
        modalDialog(
          "Yeah this works OK."
        )
      )
    })

  })
}

## To be copied in the UI
# mod_donate_ui("donate_1")

## To be copied in the server
# mod_donate_server("donate_1")
