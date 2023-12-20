# HELPER FUNCTIONS FOR mod_root_meal_edit

#' Ingredient info inputs for ROOT meal edit modal
#' @description Makes pre-filled input fields which serve to edit the data
#' @param input,output,session The shiny app session objects
#' @param data The meal dataframe being viewed/edited.
#' @param userID The session user ID from LOCAL$userID. Needed to determine
#' which records should be displayed as editable.
#' @noRd
rootEditMealIngredientInputs <- function(input, output, session,data, userID){
    ns <- session$ns
    recordUserID <- data$USER_ID
    ingUniqueID <- unique(data$INGREDIENT_UNIQUE_ID)
    ing <- unique(data$INGREDIENT)
    desc <- unique(data$INGREDIENT_DESCRIPTION)
    unit <- unique(data$SERVING_SIZE_DESCRIPTION)
    ssf <- unique(data$SERVING_SIZE_FACTOR)

    # if(!recordUserID %in% userID){
    #     disabled <- 'disabled'
    #     editMultiplierDiv <- NULL
    #     multiplierBorderColor <- NULL
    # }

    if(recordUserID %in% userID){

        # TODO 9/17/2023 The qty does not populate when editing if there is a trip loaded
        disabled <- NULL
        noPeopleCalc <- round(1/ ssf, 2)

        multiplierBorderColor <- 'border-color: #5cb874; border-width: 4px; border-style: solid; border-radius: .3rem;'

        editMultiplierDiv <- div(class = "input-group mt-1", style = 'border-color: #5cb874;
            border-width: 4px; border-style: solid; border-radius: .3rem;',
                tags$span(class = "input-group-text",
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;',
                    'Quantity'),
                tags$input(id = ns(paste0('ing-new-qty-', ingUniqueID)),
                    placeholder = 'Qty. of Ing.', type = "text",
                    `aria-label` = "Quantity", class = "form-control",
                    value = 1
                ),
                tags$span(class = "input-group-text",
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;',
                    'Serves'
                ),
                tags$input(id = ns(paste0('ing-new-hypPeople', ingUniqueID)),
                    placeholder = 'Qty. of Ing.', type = "text",
                    `aria-label` = "Quantity", class = "form-control",
                    value = round(noPeopleCalc)
                )
            )

        ## Listeners for ing-new-qty and ing-new-hypPeople ----

        ### Observe new ingredient quantity input and update multiplier -----

        observeEvent(input[[paste0('ing-new-qty-', ingUniqueID)]],{

            #req(input[['ing-new-qty']] != '')
            req(
                round(as.numeric(input[[paste0('ing-new-qty-', ingUniqueID)]])/
                    as.numeric(input[[paste0('ing-new-hypPeople', ingUniqueID)]]), 3) !=
                    as.numeric(input[[paste0('ing-ssf-',ingUniqueID)]]) ||
                    is.na(as.numeric(input[[paste0('ing-ssf-',ingUniqueID)]]))
            )
            #req(LOCAL$rootEditMealModalSwitch == TRUE)
            #req(nrow(LOCAL$rootEditIngs) > 0)
            noPeopleCalc <- as.numeric(input[[paste0('ing-new-hypPeople', ingUniqueID)]])

            updateTextInput(session, inputId = paste0('ing-ssf-',ingUniqueID),
                value = round(as.numeric(input[[paste0('ing-new-qty-', ingUniqueID)]]) /
                    noPeopleCalc,3)
            )
        }, ignoreInit = TRUE)

        ### Observe new hypothetical NoPeople input and update multiplier -----

        observeEvent(input[[paste0('ing-new-hypPeople', ingUniqueID)]],{
            #req(LOCAL$rootEditMealModalSwitch == TRUE)
            #req(nrow(LOCAL$rootEditIngs) > 0)
            noPeopleCalc <- as.numeric(input[[paste0('ing-new-hypPeople', ingUniqueID)]])
            ingQty <- if(is.na(as.numeric(input[[paste0('ing-new-qty-', ingUniqueID)]]))) {1}
                else {as.numeric(input[[paste0('ing-new-qty-', ingUniqueID)]])}

            updateTextInput(session, inputId = paste0('ing-ssf-',ingUniqueID),
                            value = round(ingQty / noPeopleCalc,3)
            )

            updateTextInput(session, inputId = paste0('ing-new-qty-', ingUniqueID),
                value = ingQty)

        }, ignoreInit = TRUE)

        ## Return editable UI inputs ----
        return(
            tagList(
                div(class = "input-group", style ='margin-top:12px;',
                    tags$span(class = "input-group-text", ing,
                        style = 'background-color: #5CB874; border-color: #5CB874; color: #fff;'),
                    tags$input(id = ns(paste0('ing-ing-',ingUniqueID)), value = ing, type = "text",
                        disabled = disabled, `aria-label` = "Ingredient", class = "form-control"),
                    tags$button(id = ns(paste0('del-ing-',ingUniqueID)), class = "btn btn-danger action-button shiny-bound-input", type = "button", icon('trash'))

                ),
                div(class = "input-group",
                    tags$span(class = "input-group-text", "Description",
                        style = 'background-color: #5CB874; border-color: #5CB874; color: #fff;'),
                    tags$input(id = ns(paste0('ing-desc-',ingUniqueID)), value = desc, type = "text",
                        disabled = disabled, `aria-label` = "Description", class = "form-control")
                ),
                div(class = "input-group",
                    tags$span(class = "input-group-text", 'Units',
                        style = 'background-color: #5CB874; border-color: #5CB874; color: #fff;'),
                    tags$input(id = ns(paste0('ing-unit-',ingUniqueID)), value = unit, type = "text",
                        disabled = disabled, `aria-label` = "Units", class = "form-control")
                ),
                div(class = "input-group",
                    tags$span(class = "input-group-text", style = multiplierBorderColor, 'Multiplier'),
                    tags$input(id = ns(paste0('ing-ssf-',ingUniqueID)), value = ssf, type = "text",
                        disabled = 'disabled', `aria-label` = "Multiplier", class = "form-control"
                    )
                ),
                editMultiplierDiv
            )
        )
    } else {
        disabled <- 'disabled'
        editMultiplierDiv <- NULL
        multiplierBorderColor <- NULL
        return(
            tagList(
                div(class = "input-group", style ='margin-top:12px;',
                    tags$span(class = "input-group-text", ing,
                        style = 'background-color: #162118; border-color: #162118; color: #fff;'),
                    tags$input(id = ns(paste0('ing-ing-',ingUniqueID)), value = ing, type = "text",
                        disabled = disabled, `aria-label` = "Ingredient", class = "form-control"),
                    tags$button(id = ns(paste0('del-ing-',ingUniqueID)), class = "btn btn-danger action-button shiny-bound-input", type = "button", icon('trash'))

                ),
                div(class = "input-group",
                    tags$span(class = "input-group-text", "Description"),
                    tags$input(id = ns(paste0('ing-desc-',ingUniqueID)), value = desc, type = "text",
                        disabled = disabled, `aria-label` = "Description", class = "form-control")
                ),
                div(class = "input-group",
                    tags$span(class = "input-group-text", 'Units'),
                    tags$input(id = ns(paste0('ing-unit-',ingUniqueID)), value = unit, type = "text",
                        disabled = disabled, `aria-label` = "Units", class = "form-control")
                ),
                div(class = "input-group",
                    tags$span(class = "input-group-text", style = multiplierBorderColor, 'Multiplier'),
                    tags$input(id = ns(paste0('ing-ssf-',ingUniqueID)), value = ssf, type = "text",
                        disabled = 'disabled', `aria-label` = "Multiplier", class = "form-control"
                    )
                ),
                editMultiplierDiv
            )
        )
    }
}

#' Action to take when delete ingredient button is pressed on editMeal modal
#' @description When delete ingredient is pressed remove item from LOCAL$rootEditIngs
#' and rebuild the modal.
#' @param session The shiny app session objects
#' @param id The UI input ID of the deleted ingredient delete button
#' @param data The LOCAL data object containing the rootEditIngs object for the currently being edited meal
#' @noRd
rootEditMealDelIng <- function(session, id, data){
    ns <- session$ns
    LOCAL<- data
    ingredientUniqueID <- gsub('del-ing-','', id)
    if(!ingredientUniqueID %in% LOCAL$rootEditIngs$INGREDIENT_UNIQUE_ID) {return(NULL)}
    killRow <- which(LOCAL$rootEditIngs$INGREDIENT_UNIQUE_ID == ingredientUniqueID)
    ingName <- LOCAL$rootEditIngs$INGREDIENT[killRow]
    LOCAL$rootEditIngs <- LOCAL$rootEditIngs[-killRow, ]
    showNotification(paste(ingName,'removed from this meal!'))
}

# TODO 10/14/2023 THis function can go away

#' Action to take when edit ingredient button is pressed on editMeal modal
#' @description When edit ingredient is pressed remove item from LOCAL$rootEditIngs
#' and rebuild the modal.
#' @param input The shiny input object
#' @param session The shiny app session objects
#' @param id The UI input ID of the deleted ingredient edit button
#' @param data The LOCAL data object containing the rootEditIngs object for the currently being edited meal
#' @noRd
NULL
# rootEditMealEditIng <- function(input, session, id, data){
#
#     ns <- session$ns
#     LOCAL<- data
#     ingredientUniqueID <- gsub('edit-ing-','', id)
#
#     if(!ingredientUniqueID %in% LOCAL$rootEditIngs$INGREDIENT_UNIQUE_ID) {return(NULL)}
#
#     ingName <- input[[paste0('ing-ing-',ingredientUniqueID)]]
#     ingDesc <- input[[paste0('ing-desc-',ingredientUniqueID)]]
#     ingUnit <- input[[paste0('ing-unit-',ingredientUniqueID)]]
#     ingRow <- which(LOCAL$rootEditIngs$INGREDIENT_UNIQUE_ID == ingredientUniqueID)
#
#     if(!ingName == LOCAL$rootEditIngs$INGREDIENT[[ingRow]]){
#         LOCAL$rootEditIngs$INGREDIENT[[ingRow]] <- ingName
#     }
#
#     if(!ingDesc == LOCAL$rootEditIngs$INGREDIENT_DESCRIPTION[[ingRow]]){
#         LOCAL$rootEditIngs$INGREDIENT_DESCRIPTION[[ingRow]] <- ingDesc
#     }
#
#     if(!ingUnit == LOCAL$rootEditIngs$SERVING_SIZE_DESCRIPTION[[ingRow]]){
#         LOCAL$rootEditIngs$SERVING_SIZE_DESCRIPTION[[ingRow]] <- ingUnit
#     }
#
#     showNotification('ROOT INSTANCE Ingredient details updated!
#         Changes will save to database when you click `Save` below.',
#         type = 'message'
#     )
# }

#' rootEditMealAddIng
#' @description Action to take when the addIngredient button is pressed in rooteditMeal modal
#' @param input,output,session Shiny objects
#' @param data The LOCAL reactive values data object
#' @noRd
rootEditMealAddIng <- function(input, output, session, data){
    ns <- session$ns
    LOCAL <- data

    if(is.null(input[['selectIngredient']])) {return(NULL)}

    if(input[['selectIngredient']] == 'Start typing to search...'){
        showNotification("Please select an ingredient to add to this meal.",
            type = 'warning', duration = 5)
        return(NULL)
    }

    if(input[['selectIngredient']] %in% LOCAL$rootEditIngs$INGREDIENT) {
        showNotification(paste(input[['selectIngredient']],'already exists in this meal!'),
            type = 'error', duration = 5)
        updateSelectInput(session,'selectIngredient', selected = 'Start typing to search...')
        return(NULL)
    }

    ingName <- input[['selectIngredient']]
    ingID <- LOCAL$LU_INGREDIENTS[which(LOCAL$LU_INGREDIENTS$INGREDIENT ==
        input[['selectIngredient']]),'INGREDIENT_ID']

    req(length(ingID) == 1)

    ## Append LOCAL$rootEditIngs ----

    newRecord <- LOCAL$LU_INGREDIENTS %>%
        filter(INGREDIENT_ID == ingID) %>%
        mutate(INGREDIENT_UNIQUE_ID = paste0(INGREDIENT_ID, '_', USER_ID))

    LOCAL$rootEditIngs <- LOCAL$rootEditIngs %>% bind_rows(., newRecord)

    ## Clean up ----

    updateSelectInput(session,'selectIngredient', selected = 'Start typing to search...')

    showNotification(paste(input[['selectIngredient']],'added to this meal.'),
                     type = 'message', duration = 5)

}

#' rootDeleteMeal
#' @description Totally delete a users meal from the database and any
#' trips etc.
#' @param session The shiny session object
#' @param rvObj RV. The LOCAL RV object
#' @param mealId The meal ID of the meal to be deleted
#'
#' @noRd
rootDeleteMeal <- function(session, rvObj, mealId){
    LOCAL <- rvObj
    userId <- LOCAL$userID
    mealName <- LOCAL$LU_MEAL %>%
        filter(USER_ID == LOCAL$userID, MEAL_ID == mealId) %>%
        pull(MEAL_NAME)

    req(
        LOCAL$LU_MEAL %>% filter(MEAL_ID == mealId) %>%
            pull(USER_ID) == userId
    )

    withProgress(message = 'Deleting Meal',
        detail = paste0('Deleting ', mealName, ' from database...'), {

        # Delete from xref trips if there ----

        incProgress(.10)

        if(mealId %in% LOCAL$XREF_TRIPS$MEAL_ID){
            #create id df of this meal as mealid_mealtype_riverday
            ## Delete from DB ----

            toKill <- LOCAL$XREF_TRIPS %>%
                filter(USER_ID == LOCAL$userID, MEAL_ID == mealId) %>%
                mutate(
                    MEAL_TYPE = map(MEAL_TYPE_ID, ~ getMealType(.x)),
                    mealDelId = paste0(MEAL_ID, '_',MEAL_TYPE, '_',RIVER_DAY)
                ) %>%
                select(TRIP_ID, MEAL_ID, mealDelId) %>%
                unique()

            #if(nrow(toKill) <= 1){
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
            #         upsertXrefTrips(con = con, from = xrefT, data = LOCAL, row = 1)
            #     dbDisconnect(con)

                #now remove the last legit record of this meal in xref trips
                # map2(toKill$mealDelId, toKill$TRIP_ID, ~ delMeal(
                #         id = .x, data = LOCAL, tripId = .y
                #     )
                # )

        #}

            map2(toKill$mealDelId, toKill$TRIP_ID, ~ delMeal(
                    id = .x, data = LOCAL, tripId = .y
                )
            )

            ## Delete from LOCAL$LU_TRIPS ----

            #mealId %in% LOCAL$LU_TRIPS$MEAL_ID

            LOCAL$LU_TRIPS <- LOCAL$LU_TRIPS %>%
                anti_join(., toKill,
                    by = c('TRIP_ID' = 'TRIP_ID', 'MEAL_ID' = 'MEAL_ID')
                )

            ## Delete from LOCAL$myMeals ----

            LOCAL$myMeals <- LOCAL$myMeals %>%
                anti_join(., toKill,
                    by = c('TRIP_ID' = 'TRIP_ID', 'MEAL_ID' = 'MEAL_ID')
                )

            ## Delete from LOCAL$ALL_DATA ----

            # LOCAL$ALL_DATA <- LOCAL$ALL_DATA %>%
            #     anti_join(., toKill,
            #               by = c('MEAL_ID' = 'MEAL_ID')
            #     )
        }

        incProgress(.33)

        # Delete from xref ingredient ---

        if(mealId %in% LOCAL$XREF_INGREDIENT$MEAL_ID){
            con <- rivConnect()
            deleteXrefIng(con, mealId = mealId, userId = userId)
            dbDisconnect(con)
        }

        incProgress(.50)

        ## Delete from lu_meal ----

        if(mealId %in% LOCAL$LU_MEAL$MEAL_ID){
            con <- rivConnect()
            deleteLuMeal(con, mealId = mealId, userId = userId)
            dbDisconnect(con)

        }

        incProgress(.75)

        # Refresh LOCAL ----

        con <- rivConnect()
        refreshLOCAL(con = con, data = LOCAL,
            tables = c('XREF_TRIPS', 'XREF_INGREDIENT', 'LU_MEAL')
        )

        incProgress(.99)

        dbDisconnect(con)
    })


    showNotification(paste(mealName,'removed from the database!'),
        type = 'message', duration = 10
    )

    removeModal()

}








