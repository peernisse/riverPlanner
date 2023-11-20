#Helper functions and dev items

#####MEAL MODULE OBSERVER RESPONSE ACTIONS#####

#' The action to take when a delete meal card button is pressed
#' @description Delete the selected meal from the LOCAL$myMeals dataframe.
#' THis causes the UI to re-draw and that menu card is gone from the menu
#' @param id The delete button ID for that menu card
#' @param data The LOCAL data object#'
#' @noRd
#delMealResponse <- function(session, input, output, id, data){
delMealResponse <- function(id, data){

    LOCAL <- data
    mealName <- LOCAL$myMeals %>%
        filter(MEAL_UNIQUE_ID %in% gsub('del-','',id)) %>%
        select(MEAL_NAME) %>%
        unique()

    mealId <- LOCAL$myMeals %>%
        filter(MEAL_UNIQUE_ID %in% gsub('del-','',id)) %>%
        pull(MEAL_ID) %>%
        unique()

    # Remove the meal from myMeals. This was running twice so if else stops the second run

    if(nrow(mealName) > 0){
        LOCAL$myMeals <- subset(LOCAL$myMeals, !MEAL_UNIQUE_ID %in% gsub('del-','',id))
    } else {return(NULL)}

    # toKill <- LOCAL$XREF_TRIPS %>%
    #     filter(USER_ID == LOCAL$userID, MEAL_ID == mealId) %>%
    #     mutate(
    #         MEAL_TYPE = map(MEAL_TYPE_ID, ~ getMealType(.x)),
    #         mealDelId = paste0(MEAL_ID, '_',MEAL_TYPE, '_',RIVER_DAY)
    #     ) %>%
    #     select(TRIP_ID, MEAL_ID, mealDelId) %>%
    #     unique()

    # if(nrow(toKill) <= 1){
    #     xrefT <- data.frame(
    #         MEAL_ID = 0,
    #         RIVER_DAY = 1,
    #         INGREDIENT_ID = 1,
    #         TRIP_ID = LOCAL$tripID,
    #         MEAL_TYPE_ID = 1,
    #         MEAL_NOTES = 'startup',
    #         NO_ADULTS = LOCAL$noAdults,
    #         NO_KIDS = LOCAL$noKids,
    #         NO_PEOPLE_CALC = LOCAL$noPeopleCalc,
    #         SERVING_SIZE_FACTOR = 1,
    #         USER_ID = LOCAL$userID,
    #         UPTIME = Sys.Date(),
    #         UPUSER = LOCAL$userName
    #     )
    #     #add dummy record bc about to kill last of this meal in xref_trips
    #     #this is so the trip record is not abandoned in lu_trips
    #
    #     con <- rivConnect()
    #     upsertXrefTrips(con = con, from = xrefT, data = LOCAL, row = 1)
    #     dbDisconnect(con)
    # }

    #now remove the last legit record of this meal in xref trips
    # Remove the meal from XREF_TRIPS. delMealResponse() assumes trip is loaded

    withProgress(message = 'Deleting', detail = paste0('Deleting ', mealName, ' from menu...'), {
        map(1:5, ~ incProgress(.x/10))
            #delMeal(session, input, output, id = gsub('del-','',id), data = LOCAL)
            tripId <- isolate(LOCAL$tripID)
            delMeal(id = gsub('del-','',id), data = LOCAL, tripId = tripId)
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

    LOCAL$editMealDF$NO_ADULTS <- isolate(input[[noAdultsID]])
    LOCAL$editMealDF$NO_KIDS <- isolate(input[[noKidsID]])
    LOCAL$editMealDF$NO_PEOPLE_CALC <- ceiling(as.numeric(isolate(input[[noAdultsID]])) + (as.numeric(isolate(input[[noKidsID]])) * 0.65))

    # Set new quantities by case: THIS WAS THE CAUSE OF MUCH TURMOIL WITH QTY < .5 3/15/2023!!!

    LOCAL$editMealDF <- isolate(LOCAL$editMealDF) %>%
        mutate(
            QTY = case_when(
                NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                    round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                TRUE ~ ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
            )
        )

    #LOCAL$editMealDF$QTY <- ceiling(LOCAL$editMealDF$NO_PEOPLE_CALC * LOCAL$editMealDF$SERVING_SIZE_FACTOR)
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

    LOCAL$editMealDF$QTY <- LOCAL$editMealDF$QTY %>% as.numeric(.)

    LOCAL$editMealDF$SERVING_SIZE_FACTOR[row] <- isolate({
        round(
            as.numeric(input[[.x]]) / as.numeric(LOCAL$editMealDF$NO_PEOPLE_CALC[row]), 3
        )
    })
}

#' Action to take when delete ingredient button is pressed on editMeal modal
#' @description When delete ingredient is pressed remove item from LOCAL$editMealDF
#' and rebuild the modal.
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
                         Select other ingredients before removing this one!',
                         type = 'error', duration = 10)
        return(NULL)
    }

    # Subset out the deleted ingredient
    if(nrow(LOCAL$editMealDF) > 1) {
        killRow <- which(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID == gsub('del-ing-','',.x))
        ingName <- LOCAL$editMealDF$INGREDIENT[killRow]
        LOCAL$editMealDF <- LOCAL$editMealDF[-killRow,]

        showNotification(paste(ingName,'removed from this meal!'))
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

#' editMealAddIng
#' @description Action to take when the addIngredient button is pressed in editMeal modal
#' @param input,output,session Shiny objects
#' @param data The LOCAL reactive values data object
#' @noRd
editMealAddIng <- function(input, output, session, data){
    ns <- session$ns
    LOCAL <- data

    if(is.null(input[['selectIngredient']])) {return(NULL)}

    if(input[['selectIngredient']] == 'Start typing to search...'){
        showNotification("Please select an ingredient to add to this meal.",
                         type = 'warning', duration = 5)
        return(NULL)
    }

    if(input[['selectIngredient']] %in% LOCAL$editMealDF$INGREDIENT) {

        showNotification(paste(input[['selectIngredient']],'already exists in this meal!'),
                         type = 'error', duration = 5)
        updateSelectInput(session,'selectIngredient', selected = 'Start typing to search...')
        return(NULL)
    }

    ingName <- input[['selectIngredient']]
    ingID <- LOCAL$LU_INGREDIENTS[which(LOCAL$LU_INGREDIENTS$INGREDIENT ==
        input[['selectIngredient']]),'INGREDIENT_ID'] #%>%
        #pull()
    mTypeID <- LOCAL$editMealDF$MEAL_TYPE_ID[1]

    req(length(ingID) == 1)
#TODO see if this and QTY in newRecord can just stay numeric
    #LOCAL$editMealDF$QTY <- as.character(LOCAL$editMealDF$QTY)

    newRecord <- LOCAL$LU_INGREDIENTS %>% filter(INGREDIENT_ID == ingID) %>%
        select(-c(UPTIME,UPUSER)) %>%
        mutate(
            MEAL_ID = LOCAL$editMealDF$MEAL_ID[1],
            MEAL_NAME = LOCAL$editMealDF$MEAL_NAME[1],
            MEAL_TYPE = LOCAL$editMealDF$MEAL_TYPE[1],
            MEAL_TYPE_ID = mTypeID,
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
            MEAL_NOTES = LOCAL$editMealDF$MEAL_NOTES[1],
            USER_ID = LOCAL$userID,
            USERNAME = LOCAL$userName,
            TRIP_ID = LOCAL$tripID,
            TRIPNAME = LOCAL$tripName,
            TRIP_DESC = LOCAL$tripDesc,
            UPTIME = Sys.Date(),
            UPUSER = LOCAL$userName
        ) %>%
        mutate(
            QTY = case_when(
                NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                    round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                TRUE ~ ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
            )
        )

    LOCAL$editMealDF <- bind_rows(LOCAL$editMealDF, newRecord)

    showNotification(paste(input[['selectIngredient']],'added to this meal.'),
                     type = 'message', duration = 5)

    updateSelectInput(session,'selectIngredient', selected = 'Start typing to search...')
}


#' createMealAddIng
#' @description Action to take when the addIngredient button is pressed in createMeal modal
#' @param input,output,session Shiny objects
#' @param data The LOCAL reactive values data object
#'
#' @noRd
createMealAddIng <- function(input, output, session, data){

    #TODO This new record needs to be like the above -- DONE 3/18/2023
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
                input[['selectIngredient']]),'INGREDIENT_ID'] #%>%
    #pull()
    mTypeID <- LOCAL$editMealDF$MEAL_TYPE_ID[1]

    req(length(ingID) == 1)

    newRecord <- LOCAL$LU_INGREDIENTS %>% filter(INGREDIENT_ID == ingID) %>%
        select(-c(UPTIME,UPUSER)) %>%
        mutate(
            MEAL_ID = LOCAL$editMealDF$MEAL_ID[1],
            MEAL_NAME = LOCAL$editMealDF$MEAL_NAME[1],
            MEAL_TYPE = LOCAL$editMealDF$MEAL_TYPE[1],
            MEAL_TYPE_ID = mTypeID,
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
            #QTY = as.character(ceiling(as.numeric(NO_PEOPLE_CALC) * SERVING_SIZE_FACTOR)),
            MEAL_NOTES = LOCAL$editMealDF$MEAL_NOTES[1]
        ) %>%
        mutate(
            QTY = case_when(
                NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                    round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                TRUE ~ ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
            )
        )

    LOCAL$editMealDF <- bind_rows(LOCAL$editMealDF, newRecord)

    updateSelectInput(session,'selectIngredient', selected = 'Select ingredient or start typing to search...')
}


#' Action to take when create new ingredient button is clicked
#' @description Validates input fields and initiates adding the new ingredient to the database.
#' @param input,output,session The shiny session objects.
#' @param data The LOCAL reactive values data object.
#' @noRd
newIngredientResponse <- function(input, output, session, data){

    ns <- session$ns
    LOCAL <- data

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

    newIngId <- getMaxIngID() + 1

    # Create record to add

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
    dbUpdate(from = LOCAL$LU_INGREDIENTS,to = 'LU_INGREDIENTS', data = LOCAL)

    # Notify ingredient added

    showNotification(
        paste(ingName, "was added to the 'Add Ingredient' dropdown list above.
        You can use the dropdown to add",ingName,"to this meal."),
        type = 'message', duration = 10
    )
}

#' createMealModalSave
#' @param input,output,session The shiny session objects.
#' @param data The LOCAL reactive values data object.
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

    # Update LOCAL$LU_MEAL

    maxDbMealID <- getMaxMealID()

    # Create record to add

    newRecord <- data.frame(MEAL_ID = maxDbMealID + 1) %>%
        mutate(
            MEAL_NAME = LOCAL$createMealDF$MEAL_NAME[1],
            MEAL_TYPE = LOCAL$createMealDF$MEAL_TYPE[1],
            MEAL_DESCRIPTION = LOCAL$createMealDF$MEAL_DESCRIPTION[1],
            TOOLS = input[['meal-new-tools']],
            INSTRUCTIONS = input[['meal-new-inst']],
            UPTIME = Sys.Date(),
            UPUSER = LOCAL$userName,
            USER_ID = LOCAL$userID,
            MEAL_TYPE_ID = getMealTypeID(LOCAL$createMealDF$MEAL_TYPE[1])
        )

    LOCAL$LU_MEAL <- newRecord %>%
        bind_rows(LOCAL$LU_MEAL) %>%
        select(MEAL_ID, MEAL_NAME, MEAL_TYPE, MEAL_DESCRIPTION,
               TOOLS, INSTRUCTIONS, UPTIME, UPUSER, USER_ID, MEAL_TYPE_ID)


    # Update LOCAL$LU_INGREDIENTS
        # (LOCAL$LU_INGREDIENTS already updated when creating ingredients)

    # Update XREF_INGREDIENT

    if(unique(LOCAL$createMealDF$MEAL_ID) %in% LOCAL$XREF_INGREDIENT$MEAL_ID){
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

    # Save the trip with all tables getting updated -----

    # Update DB LU_MEAL

    dbUpdate(from = LOCAL$LU_MEAL, to = 'LU_MEAL', data = LOCAL)

    # Update DB LU_XREF_INGREDIENT

    dbUpdate(from = LOCAL$XREF_INGREDIENT, to = 'XREF_INGREDIENT', data = LOCAL)

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

    # Save trip to database

    con <- rivConnect()
        dbUpdate(con = con, from = trip, 'LU_TRIPS', data = LOCAL)
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

            # Kill lu trips

            deleteLuTrips(con = con, id = id, data = LOCAL)

            # Refresh LOCAL trip tables

            refreshLOCAL(con = con, data = LOCAL, tables = c('LU_TRIPS','XREF_TRIPS'))

        dbDisconnect(con)
    }
}

# DATABASE CRUD -----
# TODO 11/17/2023 This function can go away
#' gs4ColIndex
#' @description Return googlesheets column by name. Used to get row numbers
#' by condition to do a range read so not reading entire sheets
#' @param url String of connection to sheets URL being used
#' @param colName The name of the column you want
#' @param sheetName The name of the sheet you want to get the column of
#' @returns One column dataframe from the sheet
#' so we can specify to read just one GS column by name
#' @noRd
NULL

# gs4ColIndex <- function(url, colName, sheetName){
#     #colName <- 'QTY'
#     #sheetName <- 'LU_TRIPS'
#
#     names <- read_sheet(url, sheet = sheetName, range = '1:1') %>% names()
#     colNum <- which(names == colName)
#
#     if(length(colNum) == 0){
#         showNotification(
#             paste('No column of name',colName,'found in table',sheetName),
#             duration = 10, type = 'error')
#         return(NULL)
#     }
#
#     l1 <- LETTERS
#     l2 <- paste0('A',LETTERS)
#     l3 <- paste0('B',LETTERS)
#     l4 <- paste0('C',LETTERS)
#
#     gsIndex <- c(l1, l2, l3, l4)
#
#     LU_GS_COLS <<- data.frame(
#         COL_INDEX = seq(1:length(gsIndex)),
#         GS_INDEX = gsIndex
#     ) %>%
#     mutate(
#         GS_INDEX = paste0(GS_INDEX,':',GS_INDEX)
#     )
#
#     gsIndex <- LU_GS_COLS %>% filter(COL_INDEX == colNum) %>% pull(GS_INDEX)
#
#     # Get the column
#     return(read_sheet(url, sheet = sheetName, range = gsIndex))
#
# }

#' dbUpdate
#' @description Case conditions and set up to guide insert or
#' update of the database tables. Guides the delete, upsert... functions.
#' @param con database connection that exists in the parent. Placeholder arg.
#' @param from DF of data going to data base
#' @param to The database table name where the update should happen. DB table should match the DF in `from`
#' @param data RV. The LOCAL rv data object
#' @importFrom dbplyr sql_query_append
dbUpdate <- function(con = NULL, from, to, data = LOCAL){
    LOCAL <- data

    # Open DB connection

    con <- rivConnect()

    # Case LU_USERS

    # if(to == 'LU_USERS' || to == 'lu_users'){
    #     to <- stringr::str_to_lower(to)
    #     check <- dbGetQuery(con, paste0('select distinct username from ',to,';'))
    #
    #     try <- from %>%
    #         mutate(check = USERNAME) %>%
    #         select(check)
    #
    #     if(unique(try$check) %in% unique(check$check) == FALSE){
    #
    #         qry <- paste0("INSERT INTO lu_users (`USER_ID`, `USERNAME`, `EMAIL`, `UPTIME`, `UPUSER`) values('",
    #                       from[1,1],"','",from[1,2],"','",from[1,3],"','",Sys.time(),"','",from[1,5],"');")
    #
    #         dbExecute(con, "start transaction;")
    #         dbExecute(con,qry)
    #         dbExecute(con,"commit;")
    #         dbDisconnect(con)
    #     }
    # }

    # Case LU_INGREDIENTS

    if(to == 'LU_INGREDIENTS'){

        dbExecute(con, "start transaction;")
            dbIngIDs <- dbGetQuery(con, "SELECT INGREDIENT_ID FROM lu_ingredients;")
        dbExecute(con,"commit;")

        ingsToAdd <- LOCAL$LU_INGREDIENTS %>%
            filter(
                INGREDIENT_ID %in% setdiff(LOCAL$LU_INGREDIENTS$INGREDIENT_ID, dbIngIDs$INGREDIENT_ID)
            )

        map(1:nrow(ingsToAdd), ~ upsertLuIngredients(con = con, from = ingsToAdd, data = LOCAL, row = .x))

        refreshLOCAL(con = con, data = LOCAL, tables = c('LU_INGREDIENTS'))
    }

    # Case LU_MEAL

    if(to == 'LU_MEAL'){

        dbExecute(con, "start transaction;")
            dbMealIDs <- dbGetQuery(con, "SELECT MEAL_ID FROM lu_meal;")
        dbExecute(con,"commit;")

        mealsToAdd <- LOCAL$LU_MEAL %>%
            filter(
                MEAL_ID %in% setdiff(LOCAL$LU_MEAL$MEAL_ID, dbMealIDs$MEAL_ID)
            )

        map(1:nrow(mealsToAdd), ~ upsertLuMeal(con = con, from = mealsToAdd, data = LOCAL, row = .x))

        refreshLOCAL(con = con, data = LOCAL, tables = c('LU_MEAL'))
    }

    # Case XREF_INGREDIENT

    if(to == 'XREF_INGREDIENT'){

        dbExecute(con, "start transaction;")
            dbXrefIngIDs <- dbGetQuery(con, "SELECT MEAL_ID FROM xref_ingredient;") %>% unique()
        dbExecute(con,"commit;")

        xrefIngsToAdd <- LOCAL$XREF_INGREDIENT %>%
            filter(
                MEAL_ID %in% setdiff(LOCAL$XREF_INGREDIENT$MEAL_ID %>% unique(), dbXrefIngIDs$MEAL_ID)
            )

        map(1:nrow(xrefIngsToAdd), ~ upsertXrefIng(con = con, from = xrefIngsToAdd, data = LOCAL, row = .x))

        refreshLOCAL(con = con, data = LOCAL, tables = c('XREF_INGREDIENT'))
    }

    # Case LU_TRIPS

    if(to == 'LU_TRIPS'){

        # Update LU_TRIPS -----

        ## Case first save of trip no meals yet

        if(nrow(LOCAL$myMeals) == 0){
            luT <- from
        }

        ## Case meals have been added

        if(nrow(LOCAL$myMeals) > 0){
            luT <- LOCAL$myMeals %>% select(names(LOCAL$LU_TRIPS_DB)) %>% unique()
        }

        if(LOCAL$tripID %in% dbGetQuery(con, 'select trip_id from lu_trips;')$trip_id){
            dbExecute(con, "start transaction;")
            dbExecute(con,
                paste0("update lu_trips set `trip_id` = '",luT$TRIP_ID[1],"',`TRIPNAME` = '",
                luT$TRIPNAME[1],"',`TRIP_DESC` = '",luT$TRIP_DESC[1],
                "',USER_ID = '",luT$USER_ID[1],"', UPTIME = '",Sys.Date(),
                "', UPUSER = '",LOCAL$userName,"' where trip_id = '",luT$TRIP_ID[1],"';")
            )
            dbExecute(con,"commit;")
        }

        if(!LOCAL$tripID %in% dbGetQuery(con, 'select trip_id from lu_trips;')$trip_id){
            dbExecute(con, "start transaction;")
            dbExecute(con,
                paste0("insert into lu_trips (`TRIP_ID`,`TRIPNAME`,
                    `TRIP_DESC`,`USER_ID`,`UPTIME`,`UPUSER`)
                    values ('",luT$TRIP_ID[1],"','",luT$TRIPNAME[1],"','",luT$TRIP_DESC[1],
                       "','",luT$USER_ID[1],"','",Sys.time(),"','",LOCAL$userName,"');")
            )
            dbExecute(con,"commit;")
        }

        # Update XREF_TRIPS -----

        ## Case first save of trip no meals yet

        if(nrow(LOCAL$myMeals) == 0){

            xrefT <- data.frame(
                MEAL_ID = 0,
                RIVER_DAY = 1,
                INGREDIENT_ID = 1,
                TRIP_ID = LOCAL$tripID,
                MEAL_TYPE_ID = 1,
                MEAL_NOTES = 'startup',
                NO_ADULTS = LOCAL$noAdults,
                NO_KIDS = LOCAL$noKids,
                NO_PEOPLE_CALC = LOCAL$noPeopleCalc,
                SERVING_SIZE_FACTOR = 1,
                USER_ID = LOCAL$userID,
                UPTIME = Sys.Date(),
                UPUSER = LOCAL$userName
            )
        }

        ## Case meals have been added

        if(nrow(LOCAL$myMeals) > 0){
            xrefT <- LOCAL$myMeals %>% select(names(LOCAL$XREF_TRIPS)) %>%
                mutate(UPTIME = Sys.Date(), UPUSER = LOCAL$userName)
        }

        dbExecute(con, "start transaction;")
            dbXrefT <- dbGetQuery(con, paste0("SELECT * FROM xref_trips WHERE USER_ID = ",
                LOCAL$userID," AND TRIP_ID = ", LOCAL$tripID,";"))
        dbExecute(con,"commit;")

        # Check for records that have been deleted locally and delete from DB

        toDelete <- anti_join(dbXrefT,xrefT,
            by = c('MEAL_ID','RIVER_DAY','INGREDIENT_ID','TRIP_ID','MEAL_TYPE_ID')
        )

        if(nrow(toDelete) > 0){
            map(1:nrow(toDelete), ~ deleteXrefTrips(con = con, from = toDelete, data = LOCAL, row = .x))
        }

        # Upsert DB from local DF

        map(1:nrow(xrefT), ~ upsertXrefTrips(con = con, from = xrefT, data = LOCAL, row = .x))

        # Refresh LOCAL tables -----
        refreshLOCAL(con = con, data = LOCAL, tables = c('LU_TRIPS', 'XREF_TRIPS'))
    }

    dbDisconnect(con)
}

# ROOT DELETE MEAL RECORDS FUNCTIONS ----







