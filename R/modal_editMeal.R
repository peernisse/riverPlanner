#' Edit meal modal
#' @description A large modal with to edit the ingredients of a meal and save
#' to the user's trip and trip history
#'
#' @noRd
editMealModal <- function(input, output, session, mealUniqueID, data){
    req(length(mealUniqueID) == 1)
    ns <- session$ns
    LOCAL <- data
#browser()

    LOCAL$editMealDF <- LOCAL$myMeals %>%
        filter(MEAL_UNIQUE_ID == mealUniqueID)

    # Turn on modal observers -----
    LOCAL$editMealModalSwitch <- TRUE

    # Set meal unique ID and ingredient uniqe IDs in LOCAL to use for calculating input IDs from the modal -----
    LOCAL$editMealMealUniqueID <- mealUniqueID


    # Set up adult and kid IDs to make adjustment within editMeal modal
    #TODO this can probably go away
    #LOCAL$openModalnoAdults <- unique(LOCAL$editMealDF$NO_ADULTS)
    #LOCAL$openModalnoKids <- unique(LOCAL$editMealDF$NO_KIDS)


    # Set up the input IDs for SSF and qty for observers to make reactive adjustments

    ingredientUniqueIDs <- unique(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID)
    #LOCAL$openModalssfIDs <- paste0('ing-ssf-',ingredientUniqueIDs)
    #LOCAL$openModalqtyIDs <- paste0('ing-qty-',ingredientUniqueIDs)
    #LOCAL$openModalmealUniqueIDs <- ingredientUniqueIDs

    noAdultsID <- paste0('editMeal-noAdults-',mealUniqueID)
    noKidsID <- paste0('editMeal-noKids-',mealUniqueID)
    noPeopleCalcID <- paste0('editMeal-noPeopleCalc-',mealUniqueID)

    ttl <- paste(unique(LOCAL$editMealDF$MEAL_NAME),"|",unique(LOCAL$editMealDF$MEAL_TYPE),
                 "| River Day", unique(LOCAL$editMealDF$RIVER_DAY),"|",
                 unique(LOCAL$editMealDF$NO_ADULTS) + unique(LOCAL$editMealDF$NO_KIDS),'People')
    desc <- unique(LOCAL$editMealDF$MEAL_DESCRIPTION)
    ings <- unique(LOCAL$editMealDF$INGREDIENT)
    tools <- LOCAL$editMealDF$TOOLS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()
    inst <- LOCAL$editMealDF$INSTRUCTIONS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()

    rows <- c(1:nrow(LOCAL$editMealDF))

    # Modal HTML -----

    customModalDialog(
        h5(desc),
        p('If there will be a change of people during the trip, you can change the people inputs here
           and they will just affect this meal on this day. You will need to do this for each meal that
          has a change of people from the original numbers set up in `Trip Info`.'),
        p('Each ingredient quantity can be edited and the multiplier will update for this meal only. This change
          is not global and must be done again when using the same ingredient elsewhere, if desired.'),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5('Meal Trip Info'),
                customTextInput(inputId = ns('editMeal-tripName'), label = 'Trip Name',
                               labelColor = '#162118', value = LOCAL$tripName, disabled = 'disabled'),
                customSelectInput(inputId = ns(noAdultsID), label = 'Adults', labelColor = '#5cb874',
                                 choices = c('No. People Age 12+', '0', seq(1:30)), disabled = NULL,
                                 selected = unique(LOCAL$editMealDF$NO_ADULTS)
                ),
                customSelectInput(inputId = ns(noKidsID), label = 'Kids', labelColor = '#5cb874',
                                  choices = c('No. People Age <12', '0', seq(1:30)), disabled = NULL,
                                  selected = unique(LOCAL$editMealDF$NO_KIDS)
                ),
                customTextInput(inputId = ns(noPeopleCalcID), label = 'Total People to Calc. (Kids as 2/3)',
                                labelColor = '#162118', value = unique(LOCAL$editMealDF$NO_PEOPLE_CALC),
                                disabled = 'disabled')
            )
        ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5('Ingredients/Quantities'),
                p('--If you don\'t like the calculated quantity for an ingredient, you can adjust it
                and will adjust the multiplier for that ingredient for this meal.--',style = 'font-style: italic;'),
                p('--Click TRASH to remove an ingredient from this meal.--',style = 'font-style: italic;'),
                p('--To ADD a different ingredient, use the ingredient picker below.--', style = 'font-style: italic;'),

                map(rows, ~ editMealIngredientInputs(input, output, session,data = LOCAL$editMealDF[.x,]))
            )
        ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5(paste('Add Ingredient to',ttl)),
                p('--Start typing a word to filter the dropdown. Click + to add ingredient to this meal.--',
                style = 'font-style: italic;'),
                selectIngredients(input, output, session,data = LOCAL$LU_INGREDIENTS)
             )
           ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5(paste('Create New Ingredient')),
                p('--Your new ingredient will appear in the Add Ingredient dropdown.--',
                style = 'font-style: italic;'),
                createIngredients(session)
            )
           ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5('Meal Notes'),
                customTextAreaInput(inputId = ns(paste0('notes-',mealUniqueID)),
                                   label = 'Meal Notes', labelColor = '#162118')
            )
        ),

        fluidRow(style = 'margin-top:20px;',
          column(width = 12,
            h5('Tools'),
            tags$ul(
              map(tools, ~ tags$li(.x))
            )
          )
        ),
        fluidRow(style = 'margin-top:20px;',
          column(width = 12,
            h5('Instructions'),
            tags$ol(
              map(inst, ~ tags$li(.x))
            )
          )
        ),
        session = session,
        title = h4(ttl, style = 'color: #5cb874;'),
        size = 'xl',
        easyClose = FALSE,
        fade = TRUE,
        footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',

            actionButton(ns('updateMeal'), label = 'Update'),
            actionButton(ns('editMealModalClose_2'), label = 'Cancel'),

            modalButton('Cancel')


        )


    )


}
