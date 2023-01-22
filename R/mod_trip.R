#' trip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trip_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'display: grid; text-align: center; justify-content: space-around; margin: 5px;',
        h3('New Trip'),
        customTextInput(inputId = ns('tripName'), label = 'Trip Name', labelColor = '#162118', placeholder = 'Enter Trip Name'),

        customSelectInput(inputId = ns('noAdults'), label = 'Adults 12+', labelColor = '#162118',
                          choices = c('No. People Age 12+', '0', seq(1:30)),
                          selected = 'No. People Age 12+'
        ),
        customSelectInput(inputId = ns('noKids'), label = 'Kids <12', labelColor = '#162118',
                          choices = c('No. People Age <12', '0', seq(1:30)),
                          selected = 'No. People Age <12'
        ),
        customTextAreaInput(inputId = ns('tripDesc'), label = 'Trip Description', labelColor = '#162118'),

        actionButton(ns('createTrip'), class = 'btn btn-success', label = 'Create Trip')
      )
    ),
    br(),
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;',
        h3('My Trips')#,
        # editMealModal(),
        # a(class = "btn btn-primary", `data-bs-toggle` = "modal", href = "#exampleModalToggle",
        #   role = "button", "Open first modal")
      )
    )
  )
}

#' trip Server Functions
#'
#' @noRd
mod_trip_server <- function(id, data){
  moduleServer( id, function(input, output, session){

    # Reactive Objects
    ns <- session$ns
    LOCAL <- data

    #####REACTIVE BUTTON OBSERVERS#####
    observeEvent(input$createTrip,{

      if(is.null(input$tripName) | input$tripName == '') {
        showNotification('Please enter trip name', type = 'error', duration = 5)
        return(NULL)
      }

      if(is.null(input$noAdults) | input$noAdults == 'No. People Age 12+') {
        showNotification('Please enter number of adults', type = 'error', duration = 5)
        return(NULL)
      }

      # if(input$noAdults != 'No. People Age 12+' & is.na(as.numeric(input$noAdults)) == TRUE) {
      #   showNotification('Number of adults must be just a number', type = 'error', duration = 5)
      #   return(NULL)
      # }

      # if(is.null(input$noKids) | input$noKids == 'No. People Age <12') {
      #   showNotification('Please enter number of kids', type = 'error', duration = 5)
      #   return(NULL)
      # }

      # if(input$noKids != 'No. People Age <12' & is.na(as.numeric(input$noKids)) == TRUE) {
      #   showNotification('Number of kids must be just a number', type = 'error', duration = 5)
      #   return(NULL)
      # }

      LOCAL$tripName <- input$tripName
      LOCAL$tripDesc <- input$tripDesc
      LOCAL$noAdults <- as.numeric(input$noAdults)
      LOCAL$noKids <- ifelse(input$noKids == '' | input$noKids == 'No. People Age <12',0,as.numeric(input$noKids))
      LOCAL$noPeople <- as.numeric(LOCAL$noAdults) + as.numeric(LOCAL$noKids)
      LOCAL$noPeopleCalc <- ceiling(as.numeric(LOCAL$noAdults) + (as.numeric(LOCAL$noKids) * .65))

      showNotification(paste('Trip',LOCAL$tripName,'created for', LOCAL$noPeople,'people!'),
                       type = 'default', duration = 5
      )

    })

    #####OBSERVERS#####



    #####UI OUTPUTS#####


    #####RETURN LOCAL DATA OBJECT#####
    return(LOCAL)

  })
}

## To be copied in the UI
# mod_trip_ui("trip_1")

## To be copied in the server
# mod_trip_server("trip_1")
