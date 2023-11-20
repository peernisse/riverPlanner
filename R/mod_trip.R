#' trip UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny NS tagList
#' @noRd
mod_trip_ui <- function(id){
  ns <- NS(id)
  tagList(
    alerts(),
    tags$section(id="trip", class="shiny section-bg",
        div(class="section-title",
            h2('Trip Details'),
            helpText('This is dev test'),
            p('Enter your trip details. Your trip plan will be saved as you
              work on it to come back to, and if you ever want to use it again.'),
            tags$a(href="https://river-planner.com", target="_blank","< Click Here to goto Instructions >")
        ),
        fluidRow(
            column(width = 12, style = 'display: grid; text-align: center;
                 justify-content: space-around; margin: 5px; padding: 20px;',
                   # hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),


                   #h3('New Trip'),
                   h3(withSpinner(type = 4, color = '#5cb874', uiOutput(ns('sectionTitleTrip')))),
                   customTextInput(inputId = ns('tripName'), label = 'Trip Name', labelColor = '#232b2b', placeholder = 'Enter Trip Name'),

                   customSelectInput(inputId = ns('noAdults'), label = 'Adults 12+', labelColor = '#232b2b',
                                     choices = c('No. People Age 12+', '0', seq(1:30)),
                                     selected = 'No. People Age 12+'
                   ),
                   customSelectInput(inputId = ns('noKids'), label = 'Kids <12', labelColor = '#232b2b',
                                     choices = c('No. People Age <12', '0', seq(1:30)),
                                     selected = 'No. People Age <12'
                   ),
                   customTextAreaInput(inputId = ns('tripDesc'), label = 'Trip Description',
                                       resize = 'horizontal', width = 'auto', labelColor = '#232b2b'),

                   actionButton(ns('saveTrip'), class = 'btn btn-success', label = 'Save Trip'),
                   # hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;')

            )
        ),
        br(),
        fluidRow(
            column(width = 12, style = '/*display: grid;*/ text-align: center;
                 justify-content: space-around; margin: 5px; padding: 20px;',
                   #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
                   h3('My Trips'),
                   #uiOutput(ns('test')),
                   div(id = ns("tripSelect"), class = "accordion",
                       accInner(ns, parentId = "tripSelect", buttonId = 'savedTrips',
                                buttonTitle = 'View My Trips', collapseId = 'collapseTrips',
                                show = TRUE, body = uiOutput(ns('trips')), bgColor = TRUE)
                   ),
                   #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;')
            )
        ),
        uiOutput(ns('js'))
    ) # end section
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
#TODO make this a function
    observeEvent(input$saveTrip,{

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

            if(nrow(LOCAL$myMeals) == 0 | is.null(LOCAL$myMeals) | length(LOCAL$tripID) == 0){
            LOCAL$tripID <- getMaxTripID() + 1

            LOCAL$tripName <- input$tripName
            LOCAL$tripDesc <- input$tripDesc
            LOCAL$noAdults <- as.numeric(input$noAdults)
            LOCAL$noKids <- ifelse(input$noKids == '' | input$noKids == 'No. People Age <12',0,as.numeric(input$noKids))
            LOCAL$noPeople <- as.numeric(LOCAL$noAdults) + as.numeric(LOCAL$noKids)
            LOCAL$noPeopleCalc <- ceiling(as.numeric(LOCAL$noAdults) + (as.numeric(LOCAL$noKids) * .65))

            # Save trip record to DB even without any meals added 7/9/2023

            tripStart <- data.frame(
                TRIP_ID = LOCAL$tripID,
                TRIPNAME = ifelse(length(LOCAL$tripName) > 0, LOCAL$tripName, as.character(Sys.Date())),
                TRIP_DESC = ifelse(length(LOCAL$tripDesc) > 0, LOCAL$tripDesc, 'unknown'),
                USER_ID = LOCAL$userID,
                USERNAME = LOCAL$userName,
                UPTIME = Sys.Date(),
                UPUSER = LOCAL$userName
            )

            # Save to database

            withProgress(message = 'Trip Info', detail = 'saving to database...', {
                map(1:5, ~ incProgress(.x/10))
                dbUpdate(from = tripStart, to = 'LU_TRIPS', data = LOCAL)
                map(6:10, ~ incProgress(.x/10))
            })

            showNotification(paste0('Trip ',LOCAL$tripName,' saved to database!'),
                             type = 'message', duration = 10)

            # showNotification(paste0('Trip ',LOCAL$tripName,' started for ',LOCAL$noPeople,' people!
            #   Start adding trip data to enable saving to the database.'),
            #   type = 'message', duration = 10)

            return(NULL)
            }

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
                p(
                  paste0("The Trip Name entered: '",input$tripName,
                  "' does not match the loaded trip name '",LOCAL$myMeals$TRIPNAME[1],
                  "'. Are you changing the name?")
                ),
                title = 'Warning!',
                session = session,
                size = 'xl',
                footer = fluidRow(class = 'modal-footer-row',
                  actionButton(ns('confirmModalSaveTrip'), label = 'Confirm',
                               class = 'btn btn-success'), class = 'riv',
                  actionButton(ns('cancelModalSaveTrip'), label = 'Cancel',
                               class = 'btn btn-default', class = 'riv', class = 'getstarted')
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
                size = 'xl',
                footer = fluidRow(class = 'modal-footer-row',
                  actionButton(ns('confirmModalSaveTrip'), label = 'Confirm',
                               class = 'btn btn-success', class = 'riv'),
                  actionButton(ns('cancelModalSaveTrip'), label = 'Cancel',
                               class = 'btn btn-default', class = 'riv', class = 'getstarted')
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
    })

    # Observe Load Copy or Delete Trip -----

    observe({

      tripIDs <- LOCAL$LU_TRIPS %>%
        filter(USER_ID == LOCAL$userID) %>%
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
                title = 'Heads Up!',
                session = session,
                size = 'xl',
                footer = fluidRow(class = 'modal-footer-row',
                  actionButton(ns('confirmModalCopyTrip'), label = 'Confirm',
                               class = 'btn btn-success', class = 'riv'),
                  actionButton(ns('cancelModalCopyTrip'), label = 'Cancel',
                               class = 'btn btn-default', class = 'riv', class = 'getstarted')
                )
              )
            )
          }, ignoreInit = TRUE) # end observeEvent
        )

        createdObservers <<- c(createdObservers,copyTripIDs)

      } # end if

      ## Observe load trip buttons ----

      loadTripIDs <- paste0('load-tripID-',tripIDs)
      loadTripIDs <- loadTripIDs[!loadTripIDs %in% createdObservers]

      if(length(loadTripIDs) > 0){
        map(loadTripIDs, ~ observeEvent(input[[.x]], {

            if(length(LOCAL$tripID) > 0 && gsub('load-tripID-','',.x) == LOCAL$tripID){
                showNotification(paste('Trip:', LOCAL$tripName,'is already loaded!'), type = 'error')
                return(NULL)
            }

            tripLoad <<- gsub('load-tripID-','',.x)
            tripName <- LOCAL$LU_TRIPS %>%
              filter(TRIP_ID == gsub('load-tripID-','',.x)) %>%
              pull(TRIPNAME) %>%
              unique(.)

            showModal(
              customModalDialog(
                p("This will clear any unsaved work and load trip ",strong(tripName),
                  " Are you sure?"),
                title = 'Heads Up!',
                session = session,
                size = 'xl',
                footer = fluidRow(class = 'modal-footer-row',
                  actionButton(ns('confirmModalLoadTrip'), label = 'Confirm',
                               class = 'btn btn-success', class = 'riv'),
                  actionButton(ns('cancelModalLoadTrip'), label = 'Cancel',
                               class = 'btn btn-default', class = 'riv', class = 'getstarted')
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
            tripName <- LOCAL$LU_TRIPS %>%
            filter(TRIP_ID == gsub('kill-tripID-','',.x)) %>%
            pull(TRIPNAME) %>%
            unique(.)

            showModal(
              customModalDialog(
                p('This will permanently delete trip', strong(tripName),'. Are you sure?'),
                title = 'Warning!',
                session = session,
                size = 'xl',
                footer = fluidRow(class = 'modal-footer-row',
                  actionButton(ns('confirmModalDelTrip'), label = 'Confirm',
                               class = 'btn btn-success', class = 'riv'),
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

        copyTrip(session, id = tripCopy, data = LOCAL)

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

      showNotification(paste0('Your Trip \"',LOCAL$tripName,
        '\" is now Loaded! Manage your trip components from the menus below.'),
        type = 'message', duration = 10)

    })

    # Observe confirm trip delete -----
    observeEvent(input$confirmModalDelTrip, {

      req(length(tripDelete) > 0)
      withProgress(message = 'Deleting', detail = 'Deleting trip...', {
        map(1:5, ~ incProgress(.x/10))
          deleteTrip(session, id = tripDelete, data = LOCAL)
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

    # Trip details title
    output$sectionTitleTrip <- renderUI({
      if(length(LOCAL$tripName) > 0){
        paste('Trip',LOCAL$tripName, 'Currently Loaded')
      } else{'Create New Trip'}
    })

    #####RETURN LOCAL DATA OBJECT#####
    return(LOCAL)

  })
}
