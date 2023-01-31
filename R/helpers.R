#Helper functions and dev items

#Test data

meals <- data.frame(
    MEAL_ID = c(1,2,3,4,5,6),
    MEAL_TYPE = c('Breakfast','Breakfast','Lunch','Lunch','Dinner','Dinner'),
    MEAL = c('Lox and Bagels','Eggs to Order', 'Pita Sandwiches', 'Chicken Cesar Wraps', 'Lasagna','Steak Dinner'),
    DESCRIPTION = c('Delicious lox and bagels.',
                    'Eggs with potatoes and fruit',
                    'Pita with cold cuts and cookies',
                    'Chicken cesar salad in tortillas',
                    'Dutch overn lasagna with salad and breadsticks.',
                    'Steak with potatoes and salad.')
)



#####MEAL MODULE OBSERVER RESPONSE ACTIONS#####

#' The action to take when a delete meal card button is pressed
#' @description Delete the selected meal from the LOCAL$myMeals dataframe.
#' THis causes the UI to re-draw and that menu card is gone from the menu
#' @param id The delete button ID for that menu card
#' @param data The LOCAL data object#'
#' @noRd
delMealResponse <- function(id, data){

    LOCAL <- data
    mealName <- LOCAL$myMeals %>%
        filter(MEAL_UNIQUE_ID %in% gsub('del-','',id)) %>%
        select(MEAL_NAME) %>%
        unique()

    # Remove the meal from myMeals
    LOCAL$myMeals <- subset(LOCAL$myMeals, !MEAL_UNIQUE_ID %in% gsub('del-','',id))

    # Remove the meal from LU_TRIPS if it is there

    withProgress(message = 'Deleting', detail = paste0('Deleting ', mealName, ' from trip...'), {
        map(1:5, ~ incProgress(.x/10))
            dbDelete(id = gsub('del-','',id), to = 'LU_TRIPS', data = LOCAL, level = 'meal')
        map(6:10, ~ incProgress(.x/10))
    })
}

#' The action to take when the preview meal button is pressed
#' @description Opens a modal to view the meal ingredients, tools and instructions
#' @param session The shiny session object
#' @param id The view button ID for that meal
#' @param data The LOCAL data object
#' @noRd
viewMeal <- function(session, id, data){
    ns <- session$ns
    LOCAL <- data

    viewMealDF <- LOCAL$ALL_DATA %>%
        filter(MEAL_VIEW_ID == id)

    ttl <- unique(viewMealDF$MEAL_NAME)
    desc <- unique(viewMealDF$MEAL_DESCRIPTION)
    ings <- unique(viewMealDF$INGREDIENT)
    tools <- viewMealDF$TOOLS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()
    inst <- viewMealDF$INSTRUCTIONS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()

    showModal(
        customModalDialog(
            p(desc),
            p(style = 'font-style: italic;', '--This meal can be edited once added to the trip menu--'),
            fluidRow(
                column(width = 3,
                       h5('Ingredients'),
                       tags$ul(
                           map(ings, ~ tags$li(.x))
                       )
                ),
                column(width = 3,
                       h5('Tools'),
                       tags$ul(
                           map(tools, ~ tags$li(.x))
                       )
                ),
                column(width = 6,
                       h5('Instructions'),
                       tags$ol(
                           map(inst, ~ tags$li(.x))
                       )
                )
            ),
            session = session,
            title = ttl,
            size = 'xl',
            easyClose = FALSE,
            fade = TRUE,
            footer = NULL
        )
    )
}


#' The action to take when editing a meal and change number of adults or kids
#' @description This runs when editing a meal and change the number of adults or kids.
#' The function is to update all ingredient quantities based on the new number of people.
#' @param input,output,session The shiny app session objects
#' @param data The LOCAL data object containing the editMealDF object for the currently being editied meal
#' @noRd
editMealAdjPeople <- function(input, output, session, data){
    ns <- session$ns
    LOCAL<- data
#browser()
    noAdultsID <- paste0('editMeal-noAdults-',LOCAL$editMealMealUniqueID)
    noKidsID <- paste0('editMeal-noKids-',LOCAL$editMealMealUniqueID)
    noPeopleCalcID <- paste0('editMeal-noPeopleCalc-',LOCAL$editMealMealUniqueID)

    LOCAL$editMealDF$NO_ADULTS <- input[[noAdultsID]]
    LOCAL$editMealDF$NO_KIDS <- input[[noKidsID]]
    LOCAL$editMealDF$NO_PEOPLE_CALC <- ceiling(as.numeric(input[[noAdultsID]]) + (as.numeric(input[[noKidsID]]) * 0.65))
    LOCAL$editMealDF$QTY <- ceiling(LOCAL$editMealDF$NO_PEOPLE_CALC * LOCAL$editMealDF$SERVING_SIZE_FACTOR)

}


#' Action to take when manually adjust an ingredient quantity in editMeal modal
#' @description When ingredient quantity is adjusted backcalculate the multiplier
#' and update this in the editMeal dataframe and the UI
#' @param input,output,session The shiny app session objects
#' @param id The UI input ID of the edited ingredient quantity field
#' @param data The LOCAL data object containing the editMealDF object for the currently being edited meal
#' @noRd

editMealAdjQty <- function(input, output, session, id, data){
    ns <- session$ns
    LOCAL<- data
    .x <- id

    row <- which(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID == gsub('ing-qty-','',.x))

    if(LOCAL$editMealDF$QTY[row] == as.numeric(input[[.x]]) | is.na(as.numeric(input[[.x]]))) {return(NULL)}

    LOCAL$editMealDF$QTY[row] <- isolate(input[[.x]])

    LOCAL$editMealDF$SERVING_SIZE_FACTOR[row] <- isolate({
        round(
            as.numeric(input[[.x]]) / as.numeric(LOCAL$editMealDF$NO_PEOPLE_CALC[row]), 3
        )
    })

}

#' Action to take when delete ingredient button is pressed on editMeal modal
#' @description When delete ingredient is pressed remove item from LOCAL$editMealDF
#' and rebuild the modal. If you delete all ingredients, the meal rebuilds from the database with all ingredients
#' @param input,output,session The shiny app session objects
#' @param id The UI input ID of the deleted ingredient delete button
#' @param data The LOCAL data object containing the editMealDF object for the currently being edited meal
#' @noRd
editMealDelIng <- function(input, output, session, id, data){
    ns <- session$ns
    LOCAL<- data
    .x <- id
#browser()
    # Check if ingredient is in the df else stop
    if(!gsub('del-ing-','',.x) %in% LOCAL$editMealDF$INGREDIENT_UNIQUE_ID) {return(NULL)}

    # Check if ingredient is the last one and warn
    if(nrow(LOCAL$editMealDF) <= 1){
        showNotification('Can\'t remove last ingredient.
                         Select other ingredients before removing this one.',
                         type = 'error', duration = 10)
        return(NULL)
    }

    # Subset out the deleted ingredient
    if(nrow(LOCAL$editMealDF) > 1) {
        killRow <- which(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID == gsub('del-ing-','',.x))
        LOCAL$editMealDF <- LOCAL$editMealDF[-killRow,]
    }
}

#' Action to take when the addIngredient button is pressed in editMeal modal
#'
#'
#'
#' @noRd
editMealAddIng <- function(input, output, session, data){
    #browser()
    ns <- session$ns
    LOCAL <- data

    if(is.null(input[['selectIngredient']])) {return(NULL)}

    if(input[['selectIngredient']] == 'Select ingredient or start typing to search...' | input[['selectIngredient']] %in% LOCAL$editMealDF$INGREDIENT) {

        showNotification(paste(input[['selectIngredient']],'already exists in this meal'),
                         type = 'error', duration = 5)
        updateSelectInput(session,'selectIngredient', selected = 'Select ingredient or start typing to search...')
        return(NULL)

    }

    ingName <- input[['selectIngredient']]
    ingID <- LOCAL$LU_INGREDIENTS[which(LOCAL$LU_INGREDIENTS$INGREDIENT ==
                                            input[['selectIngredient']]),'INGREDIENT_ID'] %>%
        pull()

    LOCAL$editMealDF$QTY <- as.character(LOCAL$editMealDF$QTY)


    newRecord <- LOCAL$LU_INGREDIENTS %>% filter(INGREDIENT_ID == ingID) %>%
        select(-c(UPTIME,UPUSER)) %>%
        mutate(
            MEAL_ID = LOCAL$editMealDF$MEAL_ID[1],
            MEAL_NAME = LOCAL$editMealDF$MEAL_NAME[1],
            MEAL_TYPE = LOCAL$editMealDF$MEAL_TYPE[1],
            MEAL_DESCRIPTION = LOCAL$editMealDF$MEAL_DESCRIPTION[1],
            TOOLS = LOCAL$editMealDF$TOOLS[1],
            INSTRUCTIONS = LOCAL$editMealDF$INSTRUCTIONS[1],
            MEAL_ADD_ID = LOCAL$editMealDF$MEAL_ADD_ID[1],
            MEAL_DEL_ID = LOCAL$editMealDF$MEAL_DEL_ID[1],
            MEAL_VIEW_ID = LOCAL$editMealDF$MEAL_VIEW_ID[1],
            MEAL_EDIT_ID = LOCAL$editMealDF$MEAL_EDIT_ID[1],
            MEAL_UNIQUE_ID = LOCAL$editMealDF$MEAL_UNIQUE_ID[1],
            INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID),
            RIVER_DAY = LOCAL$editMealDF$RIVER_DAY[1],
            NO_ADULTS = LOCAL$editMealDF$NO_ADULTS[1],
            NO_KIDS = LOCAL$editMealDF$NO_KIDS[1],
            NO_PEOPLE_CALC = LOCAL$editMealDF$NO_PEOPLE_CALC[1],
            QTY = as.character(ceiling(as.numeric(NO_PEOPLE_CALC) * SERVING_SIZE_FACTOR)),
            MEAL_NOTES = LOCAL$editMealDF$MEAL_NOTES[1]
        )

    LOCAL$editMealDF <- bind_rows(LOCAL$editMealDF, newRecord)

    showNotification(paste(input[['selectIngredient']],'added to this meal.'),
                     type = 'message', duration = 5)

    updateSelectInput(session,'selectIngredient', selected = 'Select ingredient or start typing to search...')



}


#' Action to take when the addIngredient button is pressed in createMeal modal
#'
#'
#'
#' @noRd
createMealAddIng <- function(input, output, session, data){
    #browser()
    ns <- session$ns
    LOCAL <- data

    if(is.null(input[['selectIngredient']])) {return(NULL)}

    if(input[['selectIngredient']] == 'Select ingredient or start typing to search...' | input[['selectIngredient']] %in% LOCAL$editMealDF$INGREDIENT) {

        showNotification(paste(input[['selectIngredient']],'already exists in this meal'),
                         type = 'error', duration = 5)
        updateSelectInput(session,'selectIngredient', selected = 'Select ingredient or start typing to search...')
        return(NULL)

    }

    ingName <- input[['selectIngredient']]
    ingID <- LOCAL$LU_INGREDIENTS[which(LOCAL$LU_INGREDIENTS$INGREDIENT ==
                                            input[['selectIngredient']]),'INGREDIENT_ID'] %>%
        pull()

    LOCAL$editMealDF$QTY <- as.character(LOCAL$editMealDF$QTY)


    newRecord <- LOCAL$LU_INGREDIENTS %>% filter(INGREDIENT_ID == ingID) %>%
        select(-c(UPTIME,UPUSER)) %>%
        mutate(
            MEAL_ID = LOCAL$editMealDF$MEAL_ID[1],
            MEAL_NAME = LOCAL$editMealDF$MEAL_NAME[1],
            MEAL_TYPE = LOCAL$editMealDF$MEAL_TYPE[1],
            MEAL_DESCRIPTION = LOCAL$editMealDF$MEAL_DESCRIPTION[1],
            TOOLS = LOCAL$editMealDF$TOOLS[1],
            INSTRUCTIONS = LOCAL$editMealDF$INSTRUCTIONS[1],
            MEAL_ADD_ID = LOCAL$editMealDF$MEAL_ADD_ID[1],
            MEAL_DEL_ID = LOCAL$editMealDF$MEAL_DEL_ID[1],
            MEAL_VIEW_ID = LOCAL$editMealDF$MEAL_VIEW_ID[1],
            MEAL_EDIT_ID = LOCAL$editMealDF$MEAL_EDIT_ID[1],
            MEAL_UNIQUE_ID = LOCAL$editMealDF$MEAL_UNIQUE_ID[1],
            INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID),
            RIVER_DAY = LOCAL$editMealDF$RIVER_DAY[1],
            NO_ADULTS = LOCAL$editMealDF$NO_ADULTS[1],
            NO_KIDS = LOCAL$editMealDF$NO_KIDS[1],
            NO_PEOPLE_CALC = LOCAL$editMealDF$NO_PEOPLE_CALC[1],
            QTY = as.character(ceiling(as.numeric(NO_PEOPLE_CALC) * SERVING_SIZE_FACTOR)),
            MEAL_NOTES = LOCAL$editMealDF$MEAL_NOTES[1]
        )

    LOCAL$editMealDF <- bind_rows(LOCAL$editMealDF, newRecord)

    updateSelectInput(session,'selectIngredient', selected = 'Select ingredient or start typing to search...')

}

# TRIP FUNCTIONS -----

#' Load Trip
#' @description Loads trip meal info from LU_TRIPS to LOCAL$myMeals, updates trip info in LOCAL
#' @param id The trip ID
#' @noRd
loadTrip <- function(session, id, data){
    ns <- session$ns
    LOCAL <- data
    # DEV ---
    #id <- 11
    # ---

    # Validate User ID exists and id is not blank
    req(!is.null(LOCAL$userName) & LOCAL$userName != '' & length(LOCAL$userName) > 0)
    req(!is.null(id) & id != '' & length(id) > 0)

    # Load trip info-----
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
}

# DATABASE CRUD -----

#' dbUpdate
#' @description Replaces or appends records in a database table
#' @param from The updated dataframe with the new/revised records
#' @param to The name of the database table to update
#' @param data The session reactive data object LOCAL
#' @importFrom googlesheets4 read_sheet range_delete sheet_append gs4_auth gs4_deauth
#' @noRd
dbUpdate <- function(from, to, data = LOCAL){
    LOCAL <- data

    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    #TODO add if statements to use this function for other tables
    #TODO make the pulling ID check a range_read so not reading entire table

    check <- read_sheet(url, sheet = to) %>%

        mutate(check = paste0(USERNAME,'_',TRIP_ID,'_',MEAL_UNIQUE_ID)) %>%
        select(check)

    try <- from %>%
        mutate(check = paste0(USERNAME,'_',TRIP_ID,'_',MEAL_UNIQUE_ID)) %>%
        select(check)

    if(unique(try$check) %in% unique(check$check) == TRUE) {


        rows<- which(check$check %in% unique(try$check))
        range_delete(url, to, range = paste0(min(rows)+1,':',max(rows) +1))
        sheet_append(url, from, sheet = to)
        LOCAL[[to]] <- read_sheet(url, sheet = to)

    } else

    if(unique(try$check) %in% unique(check$check) == FALSE) {
        sheet_append(url, from, sheet = to)
        LOCAL[[to]] <- read_sheet(url, sheet = to)
    }

    gs4_deauth()

}

#' dbDelete
#' @description Deletes records in a database table based on ID
#' @param id The ID to find records to delete
#' @param to The name of the database table to update
#' @param data The session reactive data object LOCAL
#' @noRd
dbDelete <- function(id, to, data = LOCAL, level){
    LOCAL <- data
#browser()
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    # Check if requested records exist in the db table
    #TODO add if statements to use this function for other tables

    if(level == 'meal'){
        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USERNAME,'_',TRIP_ID,'_',MEAL_UNIQUE_ID)) %>%
            select(check)
        try <- paste0(LOCAL$userName,'_',LOCAL$tripID,'_',id)
    }

    if(level == 'trip'){
        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USERNAME,'_',TRIP_ID)) %>%
            select(check)
        try <- paste0(LOCAL$userName,'_',id)
    }

    #TODO make the pulling ID check a range_read so not reading entire table

    if(!try %in% check$check){return(NULL)}

    if(try %in% check$check){
        rows <- which(check$check %in% try)
        range_delete(url, to, range = paste0(min(rows)+1,':',max(rows) +1))
    }

    gs4_deauth()
}


#
