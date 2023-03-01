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
delMealResponse <- function(session, input, output, id, data){
    LOCAL <- data
    mealName <- LOCAL$myMeals %>%
        filter(MEAL_UNIQUE_ID %in% gsub('del-','',id)) %>%
        select(MEAL_NAME) %>%
        unique()

    # Remove the meal from myMeals. This was running twice so if else stops the second run

    if(nrow(mealName) > 0){
        LOCAL$myMeals <- subset(LOCAL$myMeals, !MEAL_UNIQUE_ID %in% gsub('del-','',id))
    } else {return(NULL)}


    # Remove the meal from LU_TRIPS if it is there

    withProgress(message = 'Deleting', detail = paste0('Deleting ', mealName, ' from trip...'), {
        map(1:5, ~ incProgress(.x/10))
            dbDelete(session = session, input = input, output = output,
                id = gsub('del-','',id), to = 'LU_TRIPS', data = LOCAL, level = 'meal')
        map(6:10, ~ incProgress(.x/10))
    })

    # Notify meal removed
    showNotification(paste0(mealName, ' removed from menu!'),
                     type = 'message', duration = 10)
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

#' Action to take when delete ingredient button is pressed on createMeal modal
#' @description When delete ingredient is pressed remove item from LOCAL$createMealDF
#' and rebuild the modal. If you delete all ingredients, the meal rebuilds from the database with all ingredients
#' @param input,output,session The shiny app session objects
#' @param id The UI input ID of the deleted ingredient delete button
#' @param data The LOCAL data object containing the createMealDF object for the currently being edited meal
#' @noRd
createMealDelIng <- function(input, output, session, id, data){
    ns <- session$ns
    LOCAL<- data
    .x <- id

    # Check if ingredient is in the df else stop
    if(!gsub('del-ing-','',.x) %in% LOCAL$createMealDF$INGREDIENT_UNIQUE_ID) {return(NULL)}

    # Check if ingredient is the last one and warn
    if(nrow(LOCAL$createMealDF) <= 1){
        showNotification('Can\'t remove last ingredient.
                         Select other ingredients before removing this one.',
                         type = 'error', duration = 10)
        return(NULL)
    }

    # Subset out the deleted ingredient
    if(nrow(LOCAL$createMealDF) > 1) {
        killRow <- which(LOCAL$createMealDF$INGREDIENT_UNIQUE_ID == gsub('del-ing-','',.x))
        LOCAL$createMealDF <- LOCAL$createMealDF[-killRow,]
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

    req(length(ingID) == 1)

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

    updateSelectInput(session,'selectIngredient', selected = 'Start typing to search...')



}


#' Action to take when the addIngredient button is pressed in createMeal modal
#'
#'
#'
#' @noRd
createMealAddIng <- function(input, output, session, data){

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


#' Action to take when new ingredient button is clicked
#'
#'
#' @noRd
newIngredientResponse <- function(input, output, session, data){
    ns <- session$ns
    LOCAL <- data
#browser()
    if(input[['ing-new-ing']] == '') {
        showNotification('Ingredient name cannot be blank!', type = 'error', duration = 10)
        return(NULL)
    }

    if(input[['ing-new-ing']] %in% LOCAL$LU_INGREDIENTS$INGREDIENT){
        showNotification('An ingredient with this name already exists!',
                         type = 'error', duration = 10)
        return(NULL)
    }

    if(input[['ing-new-cat']] == 'Select category' | input[['ing-new-cat']] == '') {
        showNotification('Ingredient category cannot be blank!',
                         type = 'error', duration = 10)
        return(NULL)
    }

    if(input[['ing-new-desc']] == '') {
        showNotification('Ingredient description cannot be blank!',
                         type = 'error', duration = 10)
        return(NULL)
    }

    if(input[['ing-new-unit']] == '') {
        showNotification('Ingredient units cannot be blank!',
                         type = 'error', duration = 10)
        return(NULL)
    }

    if(input[['ing-new-ssf']] == '') {
        showNotification('Ingredient multiplier cannot be blank!
    Enter a quantity for this number of people to produce a multiplier.',
                         type = 'error', duration = 10)
        return(NULL)
    }


    ingName <- input[['ing-new-ing']]
    ingCat <- input[['ing-new-cat']]
    ingDesc <- input[['ing-new-desc']]
    ingUnit <- input[['ing-new-unit']]
    ingSSF <- as.numeric(input[['ing-new-ssf']])
    ingStorage <- input[['ing-new-storage']]
    ingQty <- input[['ing-new-qty']]

    # Get latest DB ing ID
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    newIngId <- max(gs4ColIndex(url,'INGREDIENT_ID','LU_INGREDIENTS')) + 1

    gs4_deauth()

    newRecord <- data.frame(
        INGREDIENT_ID = newIngId,
        INGREDIENT_CATEGORY = ingCat,
        INGREDIENT = ingName,
        INGREDIENT_DESCRIPTION = ingDesc,
        SERVING_SIZE_DESCRIPTION = ingUnit,
        SERVING_SIZE_FACTOR = ingSSF,
        STORAGE_DESCRIPTION = ingStorage,
        UPTIME = Sys.Date(),
        UPUSER = LOCAL$userName,
        USER_ID = LOCAL$userID
    )

    # Append to LU_INGREDIENTS

    LOCAL$LU_INGREDIENTS <- bind_rows(LOCAL$LU_INGREDIENTS, newRecord) %>%
        arrange(INGREDIENT)

    # Append to database LU_INGREDIENTS
    dbUpdate(LOCAL$LU_INGREDIENTS,'LU_INGREDIENTS', data = LOCAL)


    # Notify ingredient added

    showNotification(
        paste(ingName, "was added to the 'Add Ingredient' dropdown list above.
        You can use the dropdown to add",ingName,"to this meal."),
        type = 'message', duration = 10
    )
}


#' createMealModalSave
#'
#'
#' @noRd
createMealModalSave <- function(input, output, session, data){
    ns <- session$ns
    LOCAL <- data

    # Validate a new ingredient is not sitting in input
    if(input[['ing-new-ing']] != ''){
        showNotification('New Ingredient form has data!
            Do you need to finish creating a new ingredient?',
            type = 'error', duration = 10
        )
        return(NULL)
    }

    # Validate inputs are not blank
    if(input[['meal-new-name']] == ''){
        showNotification('Meal name cannot be blank!', type = 'error', duration = 10)
        return(NULL)
    }

    if(input[['meal-new-type']] == 'Choose meal type...' | input[['meal-new-type']] == ''){
        showNotification('Meal type cannot be blank!', type = 'error', duration = 10)
        return(NULL)
    }

    if(is.null(input[['meal-new-desc']]) | input[['meal-new-desc']] == ''){
        showNotification('Meal description cannot be blank!', type = 'error', duration = 10)
        return(NULL)
    }

    if(is.null(input[['meal-new-tools']]) | input[['meal-new-tools']] == ''){
        showNotification('Meal tools cannot be blank!', type = 'error', duration = 10)
        return(NULL)
    }

    if(is.null(input[['meal-new-inst']]) | input[['meal-new-inst']] == ''){
        showNotification('Meal instructions cannot be blank!', type = 'error', duration = 10)
        return(NULL)
    }

    # Get max meal_ids from database LU_MEALS, LU_INGREDIENT, XREF_INGREDIENT

    # url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    # authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    # gs4_auth(path = authPath)
    #
    #     # Get new meal ID again just in case. It gets done when ings are added also
    #     maxDbMealID <- googlesheets4::read_sheet(url, 'LU_MEAL', range = 'A:A') %>% max()
    #     maxDbXrefMealID <- googlesheets4::read_sheet(url, 'XREF_INGREDIENT', range = 'B:B') %>% max()
    #     maxDbMealID <- max(c(maxDbMealID,maxDbXrefMealID))
    #
    #     # maxDbIngID <- googlesheets4::read_sheet(url, 'LU_INGREDIENTS', range = 'A:A') %>% max()
    #     # maxDbXrefIngID <- googlesheets4::read_sheet(url, 'XREF_INGREDIENT', range = 'A:A') %>% max()
    #     # maxDbIngID <- max(c(maxDbIngID,maxDbXrefIngID))
    #
    # gs4_deauth()

    # Update LOCAL$LU_MEAL

    maxDbMealID <- getMaxMealID()


    newRecord <- data.frame(MEAL_ID = maxDbMealID + 1) %>%
        mutate(
            MEAL_NAME = LOCAL$createMealDF$MEAL_NAME[1],
            MEAL_TYPE = LOCAL$createMealDF$MEAL_TYPE[1],
            MEAL_DESCRIPTION = LOCAL$createMealDF$MEAL_DESCRIPTION[1],
            TOOLS = input[['meal-new-tools']],
            INSTRUCTIONS = input[['meal-new-inst']],
            UPTIME = Sys.Date(),
            UPUSER = LOCAL$userName,
            USER_ID = LOCAL$userID
        )

    LOCAL$LU_MEAL <- newRecord %>%
        bind_rows(LOCAL$LU_MEAL) %>%
        select(MEAL_ID, MEAL_NAME, MEAL_TYPE, MEAL_DESCRIPTION,
               TOOLS, INSTRUCTIONS, UPTIME, UPUSER, USER_ID)


    # Update LOCAL$LU_INGREDIENTS
        # (LOCAL$LU_INGREDIENTS already updated when creating ingredients)


    # Update XREF_INGREDIENT

    if(LOCAL$createMealDF$MEAL_ID %in% LOCAL$XREF_INGREDIENT$MEAL_ID){
        showNotification(paste('Something went wrong. Please refresh the app and try again.'),
                         type = 'error', duration = 10
        )
        return(NULL)
    }

    # Update LOCAL$XREF_INGREDIENT

    LOCAL$XREF_INGREDIENT <- LOCAL$createMealDF %>%
        select(INGREDIENT_ID, MEAL_ID, INGREDIENT, MEAL_NAME, UPTIME, UPUSER, USER_ID) %>%
        bind_rows(LOCAL$XREF_INGREDIENT)

    # Update LOCAL$ALL_DATA
    names <- names(LOCAL$ALL_DATA)

    LOCAL$ALL_DATA <- LOCAL$LU_MEAL %>%
        select(-c(UPTIME,UPUSER)) %>%
        left_join(LOCAL$XREF_INGREDIENT %>% select(MEAL_ID,INGREDIENT_ID), by = 'MEAL_ID') %>%
        left_join(LOCAL$LU_INGREDIENTS %>% select(-c(UPTIME,UPUSER)), by = 'INGREDIENT_ID') %>%
        mutate(
            MEAL_ADD_ID = paste0('add-',MEAL_ID),
            MEAL_DEL_ID = paste0('del-',MEAL_ID),
            MEAL_VIEW_ID = paste0('view-',MEAL_ID),
            MEAL_EDIT_ID = paste0('edit-',MEAL_ID),
            MEAL_UNIQUE_ID = '',
            INGREDIENT_UNIQUE_ID = '',
            RIVER_DAY = NA_real_,
            USERNAME = LOCAL$userName,
            USER_ID = LOCAL$userID
        ) %>%
        select(names)



    # Save the trip with all tables getting updated

    # Update DB LU_MEAL

    dbUpdate(LOCAL$LU_MEAL,'LU_MEAL', data = LOCAL)

    # Update DB LU_XREF_INGREDIENT
    #browser()
    dbUpdate(LOCAL$XREF_INGREDIENT,'XREF_INGREDIENT', data = LOCAL)

    # Clear createMealDF

    LOCAL$createMealDF <- data.frame()

    # Notify success

    showNotification(paste('Meal', input[['meal-new-name']],
        'created! You can find it in the Select Meals carousel!'),
        type = 'message', duration = 10
    )

    # Close modal
    removeModal()

}


# TRIP FUNCTIONS -----

#' Save Trip
#' @param input,output,session The shiny session objects
#' @param data List. The LOCAL reactiveValues object is intended to be passed to saveTrip
#'
#' @noRd
saveTrip <- function(input, output, session, data){
    ns <- session$ns
    LOCAL <- data

    # Validate User ID exists and there are data to save
    req(!is.null(LOCAL$userName) & LOCAL$userName != '' & length(LOCAL$userName) > 0)
    req(nrow(LOCAL$myMeals) > 0)
    #req(!is.null(id) & id != '' & length(id) > 0)

    # Determine if ingredient re-calculation needs to be done
    if(input$noAdults != LOCAL$noAdults | input$noKids != LOCAL$noKids){

        # Update calculations in LOCAL$myMeals
        LOCAL$myMeals$NO_ADULTS <- as.numeric(input$noAdults)
        LOCAL$myMeals$NO_KIDS <- as.numeric(input$noKids)
        LOCAL$myMeals$NO_PEOPLE_CALC <- ceiling(as.numeric(LOCAL$myMeals$NO_ADULTS) + (as.numeric(LOCAL$myMeals$NO_KIDS) * .65))
        LOCAL$myMeals$QTY <- ceiling(LOCAL$myMeals$NO_PEOPLE_CALC * LOCAL$myMeals$SERVING_SIZE_FACTOR)

        # Update calculations in LOCAL$LU_TRIPS placeholder
        # TODO this might not be needed so I did not code it. LU_TRIPS already gets replaced in dbUpdate()

    }

    # Update LOCAL reactive session trip values

    LOCAL$tripName <- input$tripName
    LOCAL$tripDesc <- input$tripDesc
    LOCAL$noAdults <- as.numeric(input$noAdults)
    LOCAL$noKids <- ifelse(input$noKids == '' | input$noKids == 'No. People Age <12',0,as.numeric(input$noKids))
    LOCAL$noPeople <- as.numeric(LOCAL$noAdults) + as.numeric(LOCAL$noKids)
    LOCAL$noPeopleCalc <- ceiling(as.numeric(LOCAL$noAdults) + (as.numeric(LOCAL$noKids) * .65))

    # Update LOCAL$myMeals trip info

    LOCAL$myMeals$TRIP_ID <- LOCAL$tripID
    LOCAL$myMeals$TRIPNAME <- LOCAL$tripName
    LOCAL$myMeals$UPTIME <- Sys.Date()
    LOCAL$myMeals$UPUSER <- LOCAL$userName
    LOCAL$myMeals$TRIP_DESC <- LOCAL$tripDesc
    LOCAL$myMeals$USERNAME <- LOCAL$userName

    # Save trip to database
    dbUpdate(LOCAL$myMeals, 'LU_TRIPS', data = LOCAL)

    # Notify save
    showNotification(paste0('Trip ', LOCAL$tripName,' saved!'),
                     type = 'message', duration = 10)
}

#' Copy Trip
#' @description Copies trip meal info from LU_TRIPS to LOCAL$myMeals, creates new trip ID, updates trip info in LOCAL
#' @param session The shiny session object
#' @param id The trip ID of the trip to copy
#' @param data The LOCAL reactive data object
#' @noRd
copyTrip <- function(session, id, data){
    ns <- session$ns
    LOCAL <- data
    # DEV ---
    #id <- 11
    # ---

    # Validate User ID exists and id is not blank
    req(!is.null(LOCAL$userName) & LOCAL$userName != '' & length(LOCAL$userName) > 0)
    req(!is.null(id) & id != '' & length(id) > 0)

    # Get new trip ID
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)
#TODO make this a range read of just the ID column
    newID <- read_sheet(url, sheet = 'LU_TRIPS') %>%
        pull(TRIP_ID) %>%
        max() + 1

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
    LOCAL$noPeopleCalc <- trip$NO_PEOPLE_CALC[which.max(trip$NO_ADULTS)]

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

    # Save trip to database
    dbUpdate(trip, 'LU_TRIPS', data = LOCAL)
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
    # DEV ---
    #id <- 11
    # ---

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
}

# DATABASE CRUD -----

#' gs4ColIndex
#' @description Return googlesheets column by name. Used to get row numbers
#' by condition to do a range read so not reading entire sheets
#' @param url String of connection to sheets URL being used
#' @param colNum The name of the column you want
#' @param sheetName The name of the sheet you want to get the column of
#' @returns One column dataframe from teh sheet
#' so we can specify to read just one GS column by name
#' @noRd
gs4ColIndex <- function(url, colName, sheetName){
    #colName <- 'QTY'
    #sheetName <- 'LU_TRIPS'

    names <- read_sheet(url, sheet = sheetName, range = '1:1') %>% names()
    colNum <- which(names == colName)

    if(length(colNum) == 0){
        showNotification(
            paste('No column of name',colName,'found in table',sheetName),
            duration = 10, type = 'error')
        return(NULL)
    }

    l1 <- LETTERS
    l2 <- paste0('A',LETTERS)
    l3 <- paste0('B',LETTERS)
    l4 <- paste0('C',LETTERS)

    gsIndex <- c(l1, l2, l3, l4)

    LU_GS_COLS <<- data.frame(
        COL_INDEX = seq(1:length(gsIndex)),
        GS_INDEX = gsIndex
    ) %>%
    mutate(
        GS_INDEX = paste0(GS_INDEX,':',GS_INDEX)
    )

    gsIndex <- LU_GS_COLS %>% filter(COL_INDEX == colNum) %>% pull(GS_INDEX)

    # Get the colun
    return(read_sheet(url, sheet = sheetName, range = gsIndex))

}

#' dbUpdate
#' @description Replaces or appends records in a database table
#' @param from The updated dataframe with the new/revised records
#' @param to The name of the database table to update
#' @param data The session reactive data object LOCAL
#' @importFrom googlesheets4 read_sheet range_delete sheet_append gs4_auth gs4_deauth
#' @noRd
dbUpdate <- function(from, to, data = LOCAL){
    LOCAL <- data
#browser()
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)
    #TODO make the pulling ID check a range_read so not reading entire table
    #TODO add if statements to use this function for other tables

    # case LU_USERS

    if(to == 'LU_USERS'){
        check <- read_sheet(url, sheet = to) %>%
            mutate(check = USERNAME) %>%
            select(check)

        try <- from %>%
            mutate(check = USERNAME) %>%
            select(check)

        if(unique(try$check) %in% unique(check$check) == FALSE){
            sheet_append(url, from, sheet = to)
            return(NULL)
        }

    }


    # Case LU_INGREDIENTS

    if(to == 'LU_INGREDIENTS'){
        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USER_ID,'_',INGREDIENT_ID)) %>%
            select(check)

        try <- from %>%
            mutate(check = paste0(USER_ID,'_',INGREDIENT_ID)) %>%
            select(check) %>%
            anti_join(check)

        from <- from %>%
            mutate(check = paste0(USER_ID,'_',INGREDIENT_ID)) %>%
            filter(check %in% try$check)
    }


    # Case LU_TRIPS

    if(to == 'LU_TRIPS'){

        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USERNAME,'_',TRIP_ID,'_',MEAL_UNIQUE_ID)) %>%
            select(check)

        try <- from %>%
            mutate(check = paste0(USERNAME,'_',TRIP_ID,'_',MEAL_UNIQUE_ID)) %>%
            select(check)
     }

    # Case LU_MEAL

    if(to == 'LU_MEAL'){

        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USER_ID,'_',MEAL_ID)) %>%
            select(check)

        try <- from %>%
            mutate(check = paste0(USER_ID,'_',MEAL_ID)) %>%
            select(check) %>%
            anti_join(check)

        from <- from %>%
            mutate(check = paste0(USER_ID,'_',MEAL_ID)) %>%
            filter(check %in% try$check)

    }

    # Case XREF_INGREDIENT

    if(to == 'XREF_INGREDIENT'){

        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USER_ID,'_',MEAL_ID, '_', INGREDIENT_ID)) %>%
            select(check)

        try <- from %>%
            mutate(check = paste0(USER_ID,'_',MEAL_ID, '_', INGREDIENT_ID)) %>%
            select(check) %>%
            anti_join(check)

        from <- from %>%
            mutate(check = paste0(USER_ID,'_',MEAL_ID, '_', INGREDIENT_ID)) %>%
            filter(check %in% try$check)

    }

    # Process to DB

    if(unique(try$check) %in% unique(check$check) == TRUE) {

        rows<- which(check$check %in% unique(try$check))
        #TODO this is really sketchy if rows are not all together it will delete wrong info
        range_delete(url, to, range = paste0(min(rows) + 1,':',max(rows) +1))
        localSheetNames <- names(eval(parse(text = paste0('LOCAL$',to))))
        from <- from %>% select(localSheetNames)
        sheet_append(url, from, sheet = to)
        LOCAL[[to]] <- read_sheet(url, sheet = to)

    } else

    if(unique(try$check) %in% unique(check$check) == FALSE) {
        localSheetNames <- names(eval(parse(text = paste0('LOCAL$',to))))
        from <- from %>% select(localSheetNames)

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
dbDelete <- function(session, input, output, id, to, data = LOCAL, level){
    ns <- session$ns
    LOCAL <- data

    # Remove trip from database

    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    # Check if requested records exist in the db table
    #TODO add if statements to use this function for other tables
    #TODO why does this run twice?

    if(level == 'meal'){

        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USERNAME,'_',TRIP_ID,'_',MEAL_UNIQUE_ID)) %>%
            select(check)
        try <- paste0(LOCAL$userName,'_',LOCAL$tripID,'_',id)
    }

    if(level == 'trip'){

        # Remove trip info and inputs if the trip is currently loaded

        if(id == LOCAL$tripID){

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

        # Get the trip IDs from LU_TRIPS in database and check if it is already there
        check <- read_sheet(url, sheet = to) %>%
            mutate(check = paste0(USERNAME,'_',TRIP_ID)) %>%
            select(check)
        try <- paste0(LOCAL$userName,'_',id)
    }

    #TODO make the pulling ID check a range_read so not reading entire table

    if(!try %in% check$check){return(NULL)}

    if(try %in% check$check){
        rows <- which(check$check %in% try)
        #TODO This is sketchy need to address this if the rows are not together will delete other stuff
        range_delete(url, to, range = paste0(min(rows)+1,':',max(rows) +1))
        LOCAL[[to]] <- read_sheet(url, sheet = to)

    }

    gs4_deauth()
}

# Export menu items -----

#' shopList
#' @description Makes the grouped ingredient shopping list for the trip
#' @param data The LOCAL reactive values data object, specifically the myMeals dataframe
#' @noRd
shopList <- function(data){
    LOCAL <- data
    req(nrow(LOCAL$myMeals) > 0)
    output <- LOCAL$myMeals %>%
        group_by(INGREDIENT, SERVING_SIZE_DESCRIPTION) %>%
        summarize(
            TOTAL = sum(QTY, na.rm = TRUE) %>% as.character(),
            MEAL_COUNT = length(INGREDIENT)
        ) %>%
        select(INGREDIENT, TOTAL, SERVING_SIZE_DESCRIPTION, MEAL_COUNT)
    names(output) <- c('Ingredient', 'Quantity', 'Units', 'Meal Count')
    return(output)
}

#' dailyMenu
#' @noRd
dailyMenu <- function(session, id, data){
    ns <- session$ns
    #browser()
    LOCAL <- data
    req(nrow(LOCAL$myMeals) > 0)

    #DEV###
    #id <- '158_Breakfast_1'
    #######

    meal <- LOCAL$myMeals %>%
        filter(MEAL_UNIQUE_ID == id)

    day <- unique(meal$RIVER_DAY)
    mtype <- unique(meal$MEAL_TYPE)
    ttl <- unique(meal$MEAL_NAME)
    noAdults <- unique(meal$NO_ADULTS)
    noKids <- unique(meal$NO_KIDS)

    header <- paste('Day',day,'|',mtype,'|',ttl,'|',noAdults,'Adults |', noKids,'Kids')

    ings <- meal %>%
        select(INGREDIENT, QTY, SERVING_SIZE_DESCRIPTION, STORAGE_DESCRIPTION) %>%
        arrange(INGREDIENT)

    #renderUI({
        div(
            h4(header, style = 'color: black; text-align: left;'),
            tags$table(class = "table table-striped",
               tags$thead(
                   tags$tr(
                       tags$th(scope = 'col', 'Ingredient'),
                       tags$th(scope = 'col', 'Quantity'),
                       tags$th(scope = 'col', 'Units'),
                       tags$th(scope = 'col', 'Storage'),
                    )
                ),
                tags$tbody(
                    map(1:nrow(ings), ~
                        tags$tr(
                            tags$td(ings[.x,1]),
                            tags$td(ings[.x,2]),
                            tags$td(ings[.x,3]),
                            tags$td(ings[.x,4])
                        )
                    )
                )
            ),
            br()
        )

    #})
}

#'getMaxMealID
#'
#'
#'@noRd
getMaxMealID <- function(){

    # Get max meal_ids from database LU_MEALS, LU_INGREDIENT, XREF_INGREDIENT

    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    # Get new meal ID again just in case. It gets done when ings are added also
    maxDbMealID <- googlesheets4::read_sheet(url, 'LU_MEAL', range = 'A:A') %>% max()
    maxDbXrefMealID <- googlesheets4::read_sheet(url, 'XREF_INGREDIENT', range = 'B:B') %>% max()
    maxDbMealID <- max(c(maxDbMealID,maxDbXrefMealID))

    # maxDbIngID <- googlesheets4::read_sheet(url, 'LU_INGREDIENTS', range = 'A:A') %>% max()
    # maxDbXrefIngID <- googlesheets4::read_sheet(url, 'XREF_INGREDIENT', range = 'A:A') %>% max()
    # maxDbIngID <- max(c(maxDbIngID,maxDbXrefIngID))

    gs4_deauth()

    return(maxDbMealID)


}


#
