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
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
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
      column(width = 12, style = 'display: grid; text-align: center; justify-content: space-around; margin: 5px;',
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3('My Trips'),
        uiOutput(ns('test'))

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

    #####VECTOR OF CREATED OBSERVERS FOR EDIT MEAL MODAL#####
    createdObservers <- c()
    tripDelete <- c()
    tripLoad <- c()

    #####OBSERVERS#####

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

    # Observe Load or Delete Trip -----

    observe({
      tripIDs <- LOCAL$LU_TRIPS %>%
                     filter(USERNAME == LOCAL$userName) %>%
                     pull(TRIP_ID) %>% unique()

      # Observe load trip buttons
      loadTripIDs <- paste0('load-tripID-',tripIDs)
      loadTripIDs <- loadTripIDs[!loadTripIDs %in% createdObservers]
      if(length(loadTripIDs) > 0){
        map(loadTripIDs, ~ observeEvent(input[[.x]], {
            tripLoad <<- gsub('load-tripID-','',.x)
            tripName <- LOCAL$LU_TRIPS %>%
              filter(TRIP_ID == gsub('load-tripID-','',.x)) %>%
              pull(TRIPNAME) %>%
              unique(.)

            showModal(
              customModalDialog(
                p("This will clear any unsaved work and load trip ",strong(tripName)," Are you sure?"),
                title = 'Warning!',
                session = session,
                footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',
                  actionButton(ns('confirmModalLoadTrip'), label = 'Confirm', class = 'btn btn-success'),
                  actionButton(ns('cancelModalLoadTrip'), label = 'Cancel', class = 'btn btn-default',
                               class = 'riv', class = 'getstarted')
                )
              )
            )
          }, ignoreInit = TRUE) # end observeEvent
        )
        createdObservers <<- c(createdObservers,loadTripIDs)
      } # end if

      # Observe delete trip buttons

      delTripIDs <- paste0('kill-tripID-',tripIDs)
      delTripIDs <- delTripIDs[!delTripIDs %in% createdObservers]
      if(length(delTripIDs) > 0){
        map(delTripIDs, ~ observeEvent(input[[.x]], {
            tripDelete <<- gsub('kill-tripID-','',.x)
            showModal(
              customModalDialog(
                p('This will permanently delete this trip. Are you sure?'),
                title = 'Warning!',
                session = session,
                footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',
                  actionButton(ns('confirmModalDelTrip'), label = 'Confirm', class = 'btn btn-success'),
                  actionButton(ns('cancelModalDelTrip'), label = 'Cancel', class = 'btn btn-default',
                               class = 'riv', class = 'getstarted')
                )
              )
            )
          }, ignoreInit = TRUE) # end observeEvent
        )
        createdObservers <<- c(createdObservers,delTripIDs)
      } # end if
    }) # end observe

    # Observe trip load or delete modal confirm buttons -----

    # observe cancel trip load
    observeEvent(input$cancelModalLoadTrip, {removeModal()})

    # Observe cancel trip delete
    observeEvent(input$cancelModalDelTrip, {removeModal()})

    # Observe confirm trip load
    observeEvent(input$confirmModalLoadTrip,{
      req(length(tripLoad) >0)

      withProgress(message = 'Loading', detail = 'Loading trip...', {
        map(1:5, ~ incProgress(.x/10))

        loadTrip(session = session, id = tripLoad, data = LOCAL)

        tripLoad <<-c()
        removeModal()
        map(6:10, ~ incProgress(.x/10))
      })

    })

    # Observe confirm trip delete
    observeEvent(input$confirmModalDelTrip, {
      req(length(tripDelete) >0)

      withProgress(message = 'Deleting', detail = 'Deleting trip...', {
        map(1:5, ~ incProgress(.x/10))
        dbDelete(id = tripDelete, to = 'LU_TRIPS', data = LOCAL, level = 'trip')
        LOCAL$LU_TRIPS <- LOCAL$LU_TRIPS %>% filter(!TRIP_ID %in% tripDelete)
        tripDelete <<-c()
        removeModal()
        map(6:10, ~ incProgress(.x/10))
      })
    })


    #####UI OUTPUTS#####

    # Trip cards
    output$test <- makeTripCards(input, output, session, data = LOCAL)


    #####RETURN LOCAL DATA OBJECT#####
    return(LOCAL)

  })
}

## To be copied in the UI
# mod_trip_ui("trip_1")

## To be copied in the server
# mod_trip_server("trip_1")
