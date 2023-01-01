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
    LOCAL$myMeals <- subset(LOCAL$myMeals, !MEAL_UNIQUE_ID %in% gsub('del-','',id))
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
    updateTextInput(session, noPeopleCalcID,  value = unique(LOCAL$editMealDF$NO_PEOPLE_CALC))

    # Loop the ingredient qty IDs and update from the existing SSF val and the new people val
    ingredientUniqueIDs <- unique(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID)
    qtyIDs <- paste0('ing-qty-',ingredientUniqueIDs)
    ssfIDs <- paste0('ing-ssf-',ingredientUniqueIDs)

    for(i in 1:length(qtyIDs)){
        qtyID <- qtyIDs[i]
        ingredientUniqueID <- gsub('ing-qty-','',qtyID)
        qty <- LOCAL$editMealDF %>% filter(INGREDIENT_UNIQUE_ID == ingredientUniqueID) %>% pull(QTY)

        updateTextInput(session, qtyID, value =  qty)

    }
}


#' Action to take when manually adjust an ingredient quantity in editMeal modal
#' @description When ingredient quantity is adjusted backcalculate the multiplier
#' and update this in the editMeal dataframe and the UI
#' @param input,output,session The shiny app session objects
#' @param id The UI input ID of the edited ingredient quantity field
#' @param data The LOCAL data object containing the editMealDF object for the currently being editied meal
#' @noRd
editMealAdjQty <- function(input, output, session, id, data){
    ns <- session$ns
    LOCAL<- data
    .x <- id

    noPeopleCalcID <- strsplit(.x,"_") %>% unlist() %>% .[1:3] %>% paste0(.,collapse = "_")
    noPeopleCalcID <- gsub('ing-qty-','editMeal-noPeopleCalc-',noPeopleCalcID)
    qtyID <- .x
    ssfID <- gsub('ing-qty-','ing-ssf-',.x)
    noPeopleCalc <- LOCAL$editMealDF$NO_PEOPLE_CALC %>% unique(.)
    mult <- input[[ssfID]]
    qty <- input[[qtyID]]

    if(ceiling(as.numeric(noPeopleCalc) * as.numeric(mult)) == as.numeric(qty)){
        return(NULL)
    } else

    if(ceiling(as.numeric(noPeopleCalc) * as.numeric(mult)) != as.numeric(qty)){
        #TODO figure out why this goes 3 times
        ingredientUniqueID <- gsub('ing-qty-','',.x)

        # Backcalculate SSF from qty input

        newMult <- round(as.numeric(qty) / as.numeric(noPeopleCalc), 3)
        row <- which(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID == ingredientUniqueID)
        isolate({
            LOCAL$editMealDF$SERVING_SIZE_FACTOR[row] <- newMult
        })
        updateTextInput(session,ssfID, value = round(as.numeric(qty) / as.numeric(noPeopleCalc), 3))
    }
}
