#' travel UI Function
#'
#' @description Travel planner module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_travel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'display: grid; text-align: center;
             justify-content: space-around; margin: 5px; padding: 20px;',
         #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
         h3('Crew Details'),
         actionButton(ns('viewCrew'), label = 'View Crew Members', class = 'btn btn-success',
           style = 'margin:5px;'),
         actionButton(ns('addCrew'), label = 'Add Crew Member', class = 'btn btn-success',
           style = 'margin:5px;')
      )
    ),
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
         #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
         h3('Vehicles')
      )
    ),
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
         #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
         h3('Shuttle Plan')
      )
    )
  )
}

#' travel Server Functions
#'
#' @noRd
mod_travel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #####OBSERVERS#####

    observeEvent(input$addCrew, {
      showModal(
        customModalDialog(

          p(
            "Put a crew details form here"
          ),
          title = 'Adding Crew Member Details |',
          session = session,
          size = 'l',
          footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: row; justify-content: space-around;',
            actionButton(ns('confirmModalAddCrew'), label = 'Confirm', class = 'btn btn-success'),
            actionButton(ns('cancelModalAddCrew'), label = 'Cancel', class = 'btn btn-default',
                         class = 'riv', class = 'getstarted')
          )
        )
      )
    })


    observeEvent(input$confirmModalAddCrew, {
      showNotification("Button Works!")
    })

    observeEvent(input$cancelModalAddCrew, {
      removeModal()
    })

    #----------------------------------------

  })
}

## To be copied in the UI
# mod_travel_ui("travel_1")

## To be copied in the server
# mod_travel_server("travel_1")
