#' gear UI Function
#' @description Gear module UI.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_gear_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3('Select Equipment Checklists'),
        div(class = 'section-title',
          p('Select the categories of checklists to use for this trip.
          Click "+" to add the category to the trip. These will then be
          available below to customize and assign items to people, if desired.
          Finally, preview and export the trip checklist which will combine
          everything into one HTML checklist file to use on your mobile device.'
          )
        ),
        div(id = ns("checklistSelect"), class = "accordion",
          accInner(ns, parentId = "checklistSelect", buttonId = 'selectChecklists',
           buttonTitle = 'View Checklists', collapseId = 'collapseChecklists',
           show = FALSE, body = p('Nothing here yet...'), bgColor = TRUE)
        )
      )
    ),
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3('Customize Trip Checklists'),
        div(class = 'section-title',
          p('Edit the checklists you have selected. When they are ready,
            export a combined trip checklist to use on your mobile device.')
        ),
        div(id = ns("checklistCustomize"), class = "accordion",
          accInner(ns, parentId = "checklistCustomize", buttonId = 'customizeChecklists',
           buttonTitle = 'Selected Checklists', collapseId = 'collapseSelectedChecklists',
           show = FALSE, body = p('Nothing here yet...'), bgColor = TRUE)
        ),
        br(),
        actionButton(ns('checklistExport'), label = 'Export Equipment Checklist',
                     class = 'btn btn-success', style = 'margin:5px;')
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
