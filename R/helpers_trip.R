# HELPER FUNCTIONS FOR MODULE TRIP

# UI FUNCTIONS ----

#' alertIcon
#' @description Creates dissmisible alert with bootstrap primar, warning, danger etc
#' @param icon character bi class icon base name c('info', 'exclamation-triangle')
#' @param type character bootstrap alert type primary, warning, danger etc...
#' @param style character additional style arguments for parent box
#' @param dismissible Logical whether close X button should display in upper right
#' @param ... character R code for HTML for box contents
#' @noRd
alertIcon <- function(icon, type, style, dismissible = TRUE, ...){

    path <- switch(icon,
        'info' = "M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16zm.93-9.412-1 4.705c-.07.34.029.533.304.533.194 0 .487-.07.686-.246l-.088.416c-.287.346-.92.598-1.465.598-.703 0-1.002-.422-.808-1.319l.738-3.468c.064-.293.006-.399-.287-.47l-.451-.081.082-.381 2.29-.287zM8 5.5a1 1 0 1 1 0-2 1 1 0 0 1 0 2z",
        'exclamation-triangle' = "M8.982 1.566a1.13 1.13 0 0 0-1.96 0L.165 13.233c-.457.778.091 1.767.98 1.767h13.713c.889 0 1.438-.99.98-1.767L8.982 1.566zM8 5c.535 0 .954.462.9.995l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 5.995A.905.905 0 0 1 8 5zm.002 6a1 1 0 1 1 0 2 1 1 0 0 1 0-2z"

    )

    btnDisplay <- NULL
    if(dismissible == FALSE) btnDisplay <- 'none'

    div(class = paste0('alert alert-', type, ' alert-dismissible d-flex align-items-center'),
        role = "alert", style = style,
        tags$button(type = "button", class = "btn-close",
            style = paste0('display: ', btnDisplay,';'), `data-bs-dismiss` = "alert"),
        tags$svg(xmlns = "http://www.w3.org/2000/svg",
            width = "24",
            height = "24",
            fill = "currentColor",
            class = paste0('bi bi-', path, '-fill flex-shrink-0 me-2'),
            viewbox = "0 0 16 16",
            role = "img",
            `aria-label` = "Warning:",
            tags$path(d = path)
        ),
        div(...)
    )
}

# TRIP FUNCTIONS -----

#' Copy Trip
#' @description Copies trip meal info from LU_TRIPS to LOCAL$myMeals,
#' creates new trip ID, updates trip info in LOCAL.
#' @param session The shiny session object.
#' @param id The trip ID of the trip to copy.
#' @param data The LOCAL reactive data object.
#' @noRd
copyTrip <- function(session, id, data){
    ns <- session$ns
    LOCAL <- data

    # Validate User ID exists and id is not blank
    req(!is.null(LOCAL$userName) & LOCAL$userName != '' & length(LOCAL$userName) > 0)
    req(!is.null(id) & id != '' & length(id) > 0)

    # Get new trip ID

    newID <- getMaxTripID() + 1

    # Modify the trip dataframe with new ID and name change

    trip <- LOCAL$LU_TRIPS %>%
        filter(TRIP_ID == id)

    # Load trip info

    LOCAL$loadTripMode <- TRUE

    LOCAL$tripName <- paste0('Copy Of ',unique(trip$TRIPNAME))
    LOCAL$tripID <- newID
    LOCAL$tripDesc <- unique(trip$TRIP_DESC)
    LOCAL$noAdults <- trip$NO_ADULTS[which.max(trip$NO_ADULTS)]
    LOCAL$noKids <- trip$NO_KIDS[which.max(trip$NO_ADULTS)]
    LOCAL$noPeople <- LOCAL$noAdults + LOCAL$noKids
    LOCAL$noPeopleCalc <- trip$NO_PEOPLE_CALC[which.max(trip$NO_PEOPLE_CALC)]

    # Modify
    trip <- trip %>%
        mutate(
            TRIP_ID = LOCAL$tripID,
            TRIPNAME = LOCAL$tripName,
            NO_ADULTS = LOCAL$noAdults,
            NO_KIDS = LOCAL$noKids,
            TRIP_DESC = LOCAL$tripDesc,
            UPTIME = Sys.Date()
        )

    LOCAL$myMeals <- as.data.frame(trip)

    LOCAL$LU_TRIPS <- bind_rows(LOCAL$LU_TRIPS, trip)

    # Update trip input fields
    updateTextInput(session = session, 'tripName', value = LOCAL$tripName)
    updateTextInput(session = session, 'noAdults', value = LOCAL$noAdults)
    updateTextInput(session = session, 'noKids', value = LOCAL$noKids)
    updateTextInput(session = session, 'tripDesc', value = LOCAL$tripDesc)

    # Copy trip gear list to new trip
    newGearList <- LOCAL$XREF_GEAR %>%
        filter(TRIP_ID == id) %>%
        mutate(TRIP_ID = newID, UPTIME = Sys.Date())

    # Save trip to database

    con <- rivConnect()
    dbUpdate(con = con, from = trip, 'LU_TRIPS', data = LOCAL)
    dbUpdate(con = con, from = newGearList, 'XREF_GEAR', data = LOCAL)
    dbDisconnect(con)
}

#' Load Trip
#' @description Loads trip meal info from LU_TRIPS to LOCAL$myMeals, updates trip info in LOCAL
#' @param session The shiny session object
#' @param id The trip ID
#' @param data The LOCAL reactive data object
#' @noRd
loadTrip <- function(session, id, data){
    ns <- session$ns
    LOCAL <- data

    # Validate User ID exists and id is not blank
    req(!is.null(LOCAL$userName) & LOCAL$userName != '' & length(LOCAL$userName) > 0)
    req(!is.null(id) & id != '' & length(id) > 0)

    # Load trip info
    LOCAL$loadTripMode <- TRUE

    trip <- LOCAL$LU_TRIPS %>%
        filter(TRIP_ID == id)

    LOCAL$tripName <- unique(trip$TRIPNAME)
    LOCAL$tripID <- unique(trip$TRIP_ID)
    LOCAL$tripDesc <- unique(trip$TRIP_DESC)
    LOCAL$noAdults <- trip$NO_ADULTS[which.max(trip$NO_ADULTS)]
    LOCAL$noKids <- trip$NO_KIDS[which.max(trip$NO_ADULTS)]
    LOCAL$noPeople <- LOCAL$noAdults + LOCAL$noKids
    LOCAL$noPeopleCalc <- trip$NO_PEOPLE_CALC[which.max(trip$NO_ADULTS)]
    LOCAL$myMeals <- as.data.frame(trip)

    # Update trip input fields
    updateTextInput(session = session, 'tripName', value = LOCAL$tripName)
    updateTextInput(session = session, 'noAdults', value = LOCAL$noAdults)
    updateTextInput(session = session, 'noKids', value = LOCAL$noKids)
    updateTextInput(session = session, 'tripDesc', value = LOCAL$tripDesc)

    # Case if the trip gets loaded but only the default startup meal is there, now remove it

    if(0 %in% LOCAL$myMeals$MEAL_ID){LOCAL$myMeals <- filter(LOCAL$myMeals, MEAL_ID != 0)}

    # NOTE: 3/17/2024 The trip gear list gets loaded automatically
    # because the gear cards populate based on LOCAL$tripID and grab
    # any thing in xref_gear with that trip ID
}

#' deleteTripResponse
#' @description Clears UI inputs. Initiates deleting the trip from the databse.
#' @param session The shiny session object
#' @param id The trip ID
#' @param data The LOCAL reactive data object
#' @noRd
deleteTrip <- function(session, id, data){
    LOCAL <- data

    # Case no trip is loaded

    if(length(LOCAL$tripID) == 0){LOCAL$tripID <- id}

    # TODO Case trip being deleted is not loaded but a different trip is loaded
    # this seems to not be a problem for now 3/15/2023

    # Case trip being deleted is loaded

    if(length(LOCAL$tripID) > 0 & id == LOCAL$tripID){

        # Clear trip info from LOCAL

        LOCAL$tripID <- 0
        LOCAL$tripName <- character()
        LOCAL$tripDesc <- character()
        LOCAL$noAdults <- 1
        LOCAL$noKids <- 0
        LOCAL$noPeople <- 1
        LOCAL$noPeopleCalc <- 1
        LOCAL$myMeals <- data.frame()

        # Clear user inputs

        updateTextInput(session = session, 'tripName', value = '')
        updateTextInput(session = session, 'noAdults', value = 'No. People Age 12+')
        updateTextInput(session = session, 'noKids', value = 'No. People Age <12')
        updateTextInput(session = session, 'tripDesc', value = '')
    }

    #Delete records from xref_trips and lu_trips

    toDelete <- LOCAL$XREF_TRIPS %>%
        filter(TRIP_ID == id)

    if(nrow(toDelete) > 0){
        con <- rivConnect()

        # Kill xref trips

        map(1:nrow(toDelete), ~ deleteXrefTrips(con = con, from = toDelete, data = LOCAL, row = .x))

        # Kill xref_gear

        toDeleteGear <- LOCAL$XREF_GEAR %>%
            filter(TRIP_ID == id)
        if(nrow(toDeleteGear) > 0) {
            map(1:nrow(toDeleteGear), ~ deleteXrefGear(con = con, from = toDeleteGear, data = LOCAL, row = .x))
        }

        # Kill lu trips

        deleteLuTrips(con = con, id = id, data = LOCAL)

        # Refresh LOCAL trip tables

        refreshLOCAL(con = con, data = LOCAL, tables = c('LU_TRIPS','XREF_TRIPS', 'XREF_GEAR'))

        dbDisconnect(con)
    }
}
