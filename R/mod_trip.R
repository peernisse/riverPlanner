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
        customTextInput(inputId = ns('tripName'), label = 'Trip Name', labelColor = '#232b2b', placeholder = 'Enter Trip Name'),

        customSelectInput(inputId = ns('noAdults'), label = 'Adults 12+', labelColor = '#232b2b',
                          choices = c('No. People Age 12+', '0', seq(1:30)),
                          selected = 'No. People Age 12+'
        ),
        customSelectInput(inputId = ns('noKids'), label = 'Kids <12', labelColor = '#232b2b',
                          choices = c('No. People Age <12', '0', seq(1:30)),
                          selected = 'No. People Age <12'
        ),
        customTextAreaInput(inputId = ns('tripDesc'), label = 'Trip Description', labelColor = '#232b2b'),

        actionButton(ns('saveTrip'), class = 'btn btn-success', label = 'Save Trip')
      )
    ),
    br(),
    fluidRow(
      column(width = 12, style = '/*display: grid;*/ text-align: center; justify-content: space-around; margin: 5px;',
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3('My Trips'),
        #uiOutput(ns('test')),
        div(id = ns("tripSelect"), class = "accordion",
            accInner(ns, parentId = "tripSelect", buttonId = 'savedTrips', buttonTitle = 'View My Trips',
              collapseId = 'collapseTrips', body = uiOutput(ns('trips')))
        ),
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;')
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
    tripSave <- c()
    tripCopy <- c()
    tripLoad <- c()
    tripDelete <- c()


    #####OBSERVERS#####

    observeEvent(input$saveTrip,{
#TODO Make this 'Save Trip' and add checks to warn for if a trip is currently loaded

#browser()
      if(is.null(input$tripName) | input$tripName == '') {
        showNotification('Please enter trip name', type = 'error', duration = 5)
        return(NULL)
      }

      if(is.null(input$noAdults) | input$noAdults == 'No. People Age 12+') {
        showNotification('Please enter number of adults', type = 'error', duration = 5)
        return(NULL)
      }

      if(is.null(input$noKids) | input$noKids == 'No. People Age <12') {
        showNotification('Please enter number of kids', type = 'error', duration = 5)
        return(NULL)
      }

      # Check that trip name does not start with 'Copy Of'
      if(startsWith(input$tripName, 'Copy Of') ==  TRUE){
        showNotification('Please change the trip name before saving', type = 'error', duration = 10)
        return(NULL)
      }

      if(nrow(LOCAL$myMeals) == 0 | is.null(LOCAL$myMeals)){
        LOCAL$tripName <- input$tripName
        LOCAL$tripDesc <- input$tripDesc
        LOCAL$noAdults <- as.numeric(input$noAdults)
        LOCAL$noKids <- ifelse(input$noKids == '' | input$noKids == 'No. People Age <12',0,as.numeric(input$noKids))
        LOCAL$noPeople <- as.numeric(LOCAL$noAdults) + as.numeric(LOCAL$noKids)
        LOCAL$noPeopleCalc <- ceiling(as.numeric(LOCAL$noAdults) + (as.numeric(LOCAL$noKids) * .65))

        showNotification(paste0('Trip ',LOCAL$tripName,' started for ',LOCAL$noPeople,' people!
          Start adding trip data to enable saving to the database.'),
          type = 'message', duration = 10)
        return(NULL)
      }

      # If save is clicked on a new trip with a meals but have not
      # saved from the edit meal modal yet there is no 'TRIPNAME' column in myMeals yet

      if(nrow(LOCAL$myMeals) > 0 & is.null(LOCAL$myMeals$TRIPNAME)){

        # Get Trip Info

        trip <- LOCAL$myMeals %>%
          mutate(
            TRIP_ID = LOCAL$tripID,
            TRIPNAME = ifelse(length(LOCAL$tripName) > 0, LOCAL$tripName, as.character(Sys.Date())),
            TRIP_DESC = ifelse(length(LOCAL$tripDesc) > 0, LOCAL$tripDesc, 'unknown'),
            USERNAME = LOCAL$userName,
            UPTIME = Sys.Date(),
            UPUSER = LOCAL$userName
          ) %>%
          select(., names(LOCAL$LU_TRIPS))

        # Save to database

        withProgress(message = 'Trip Info', detail = 'saving to database...', {
          map(1:5, ~ incProgress(.x/10))
            dbUpdate(trip, 'LU_TRIPS', data = LOCAL)
          map(6:10, ~ incProgress(.x/10))
        })
        showNotification(paste0('Trip ',LOCAL$tripName,' saved to database!'),
                         type = 'message', duration = 10)
        return(NULL)
      }

      # For copying a trip or changing name - Check if input name matches myMeals
      if(nrow(LOCAL$myMeals) > 0 & input$tripName != LOCAL$myMeals$TRIPNAME[1]){

        showModal(
          customModalDialog(
            p(paste0("The Trip Name entered: '",input$tripName,"' does not match the loaded trip name '",LOCAL$myMeals$TRIPNAME[1],"'. Are you changing the name?")),
            title = 'Warning!',
            session = session,
            size = 'l',
            footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: row; justify-content: center;',
                              actionButton(ns('confirmModalSaveTrip'), label = 'Confirm', class = 'btn btn-success'),
                              actionButton(ns('cancelModalSaveTrip'), label = 'Cancel', class = 'btn btn-default',
                                           class = 'riv', class = 'getstarted')
            )
          )
        )
        return(NULL)

      } else

      # Check if the input field NoAdults and NoKids matches LOCAL
      if(!as.numeric(input$noAdults) %in% LOCAL$noAdults |
         !as.numeric(input$noKids) %in% LOCAL$noKids){

        showModal(
          customModalDialog(
            p(
              paste0("The trip size entered: ",input$noAdults," Adults | ",input$noKids,
                     " Kids; does not match the loaded trip size: ",LOCAL$noAdults,
                     " Adults | ",LOCAL$noKids," Kids; Are you changing the trip size?
            NOTE: Confirming this change will re-calculate all meal ingredient quantities for the new trip size."
              )
            ),
            title = 'Warning!',
            session = session,
            size = 'l',
            footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: row; justify-content: center;',
                              actionButton(ns('confirmModalSaveTrip'), label = 'Confirm', class = 'btn btn-success'),
                              actionButton(ns('cancelModalSaveTrip'), label = 'Cancel', class = 'btn btn-default',
                                           class = 'riv', class = 'getstarted')
            )
          )
        )
        return(NULL)

      } else {

        withProgress(message = 'Saving', detail = 'Saving trip...', {
          map(1:5, ~ incProgress(.x/10))

          saveTrip(input, output, session = session, data = LOCAL)

          tripSave <<-c()
          removeModal()
          map(6:10, ~ incProgress(.x/10))
        })

      }



      # if(input$noAdults != 'No. People Age 12+' & is.na(as.numeric(input$noAdults)) == TRUE) {
      #   showNotification('Number of adults must be just a number', type = 'error', duration = 5)
      #   return(NULL)
      # }



      # if(input$noKids != 'No. People Age <12' & is.na(as.numeric(input$noKids)) == TRUE) {
      #   showNotification('Number of kids must be just a number', type = 'error', duration = 5)
      #   return(NULL)
      # }



      # showNotification(paste('Trip',LOCAL$tripName,'created for', LOCAL$noPeople,'people!'),
      #                  type = 'default', duration = 5
      # )

    })

    # Observe Load Copy or Delete Trip -----

    observe({
      tripIDs <- LOCAL$LU_TRIPS %>%
                     filter(USERNAME == LOCAL$userName) %>%
                     pull(TRIP_ID) %>% unique()

      # Observe Copy Trip buttons
      copyTripIDs <- paste0('copy-tripID-',tripIDs)
      copyTripIDs <- copyTripIDs[!copyTripIDs %in% createdObservers]

      if(length(copyTripIDs) > 0){
        map(copyTripIDs, ~ observeEvent(input[[.x]], {
            tripCopy <<- gsub('copy-tripID-','',.x)
            tripName <- LOCAL$LU_TRIPS %>%
              filter(TRIP_ID == gsub('copy-tripID-','',.x)) %>%
              pull(TRIPNAME) %>%
              unique(.)

            showModal(
              customModalDialog(
                p("This will clear any unsaved work and create trip ",strong(paste0('Copy Of ',tripName)),
                  ". You can then rename and modify the trip. Are you sure?"),
                title = 'Warning!',
                session = session,
                size = 'l',
                footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: row;  justify-content: center;',
                                  actionButton(ns('confirmModalCopyTrip'), label = 'Confirm', class = 'btn btn-success'),
                                  actionButton(ns('cancelModalCopyTrip'), label = 'Cancel', class = 'btn btn-default',
                                               class = 'riv', class = 'getstarted')
                )
              )
            )
          }, ignoreInit = TRUE) # end observeEvent
        )
        createdObservers <<- c(createdObservers,copyTripIDs)
      } # end if



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
                size = 'l',
                footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: row;  justify-content: center;',
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
                size = 'l',
                footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: row;  justify-content: center;',
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

    #Observe cancel trip save -----
    observeEvent(input$cancelModalSaveTrip, {removeModal()})

    # Observe cancel trip copy -----
    observeEvent(input$cancelModalCopyTrip, {removeModal()})

    # observe cancel trip load -----
    observeEvent(input$cancelModalLoadTrip, {removeModal()})

    # Observe cancel trip delete -----
    observeEvent(input$cancelModalDelTrip, {removeModal()})

    # Observe confirm trip save -----

    observeEvent(input$confirmModalSaveTrip,{
      #req(length(tripSave) > 0)

      withProgress(message = 'Saving', detail = 'Saving trip...', {
        map(1:5, ~ incProgress(.x/10))

        saveTrip(input, output, session = session, data = LOCAL)

        tripSave <<-c()
        removeModal()
        map(6:10, ~ incProgress(.x/10))
      })

    })


    # Observe confirm trip copy -----
    observeEvent(input$confirmModalCopyTrip,{
      req(length(tripCopy) > 0)

      withProgress(message = 'Copying', detail = 'Copying trip...', {
        map(1:5, ~ incProgress(.x/10))

        copyTrip(session = session, id = tripCopy, data = LOCAL)

        tripCopy <<-c()
        removeModal()
        map(6:10, ~ incProgress(.x/10))
      })

      showNotification('Trip Copied', type = 'message', duration = 5)

    })


    # Observe confirm trip load -----
    observeEvent(input$confirmModalLoadTrip,{
      req(length(tripLoad) > 0)

      withProgress(message = 'Loading', detail = 'Loading trip...', {
        map(1:5, ~ incProgress(.x/10))

        loadTrip(session = session, id = tripLoad, data = LOCAL)

        tripLoad <<-c()
        removeModal()
        map(6:10, ~ incProgress(.x/10))
      })

      showNotification('Trip Loaded', type = 'message', duration = 5)

    })

    # Observe confirm trip delete -----
    observeEvent(input$confirmModalDelTrip, {
      req(length(tripDelete) > 0)
      withProgress(message = 'Deleting', detail = 'Deleting trip...', {
        map(1:5, ~ incProgress(.x/10))
          dbDelete(session = session, input = input, output = output, id = tripDelete,
            to = 'LU_TRIPS', data = LOCAL, level = 'trip')

        LOCAL$LU_TRIPS <- LOCAL$LU_TRIPS %>% filter(!TRIP_ID %in% tripDelete)
        tripDelete <<-c()
        removeModal()
        map(6:10, ~ incProgress(.x/10))
      })

      showNotification('Trip Removed', type = 'message', duration = 5)

    })


    #####UI OUTPUTS#####

    # Trip cards
    output$trips <- makeTripCards(input, output, session, data = LOCAL)


    #####RETURN LOCAL DATA OBJECT#####
    return(LOCAL)

  })
}

## To be copied in the UI
# mod_trip_ui("trip_1")

## To be copied in the server
# mod_trip_server("trip_1")
