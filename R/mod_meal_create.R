#' meal_create UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @noRd
mod_meal_create_ui <- function(id,session){
    ns <- NS(id)
    tagList(
        customModalDialog(
            # Instructions -----
            h5('New Meal Details'),
            p('--Fill in your new meal details and add ingredients for your new meal.
              The ingredient picker and new ingredient creation tool are available below.--',
              style = 'font-style: italic;'),

            # Create Meal Data Entry -----

            div(class = "input-group", class = 'create-meal',
                tags$span(class = "input-group-text", class = 'create-meal',
                    'Meal Name',
                    #style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;'
                ),
                tags$input(id = ns('meal-new-name'),
                    placeholder = 'New Meal Name', type = "text",
                    `aria-label` = "New Meal Name", class = "form-control", class = 'create-meal')
            ),

            div(class = "input-group", class = 'create-meal',
                tags$label(class = "input-group-text", class = 'create-meal',
                    `for` = ns('meal-new-type'),
                    #style = "background-color: #ed7000; border-color: #ed7000; color: #fff;",
                    'Meal Type'
                ),
                tags$select(id = ns('meal-new-type'), class = "form-select", class = 'create-meal',
                    tags$option(selected = "selected", 'Choose meal type...'),
                        map(c('Breakfast','Lunch','Dinner', 'Dessert','Appetizer','Cocktail'),
                            ~ tags$option(value = .x,.x))
                )
            ),
            customTextAreaInput(inputId = ns('meal-new-desc'), label = 'Meal Description',
              labelColor = '#5cb874', width = '100%'
            ),

            fluidRow(style = 'margin-top:20px;',
                column(width = 12,
                    h5('Add Ingredients'),
                    uiOutput(ns('modalIngs'))
                )
            ),
            fluidRow(style = 'margin-top:20px;',
                column(width = 12,
                    uiOutput(ns('ttl2')),
                    p('--Start typing a word to filter the dropdown. Click + to add ingredient to this meal.--',
                        style = 'font-style: italic;'),
                    uiOutput(ns('modalSelIng'))
                )
            ),

            h5('Create New Ingredient', style = 'margin-top:20px;'),
            collapseInstructions(nmsp = ns, id = 'mealCreateInst-1',
                ttl = '<Open Create Ingredient Instructions>', icon = 'circle-info',
                    p('Below the input form in `Orange` allows you to create a new ingredient
                        for your profile to use again and again.'),
                    p('Use the `Multiplier Calculator` to determine the best multiplier
                        for your ingredient and units.'),
                    p('What you choose for Units is up to you. It may make sense to have
                        units of "ounces", or it might make sense to use units such as
                        "12 ounce can" or "Standard box of Zattaran\'s rice".'),
                    p('-- Your new ingredient will be saved in the database when you click ` + `. --',
                        style = 'font-style: italic;'),
                    p('--Your new ingredient will appear in the Add Ingredient dropdown above.--',
                        style = 'font-style: italic;'),
            ),
            fluidRow(style = 'margin-top:20px;',
                column(width = 12,
                    uiOutput(ns('modalNewIng')),
                    br()
                )
            ),

            fluidRow(
                column(width = 12,
                    h5('Tools Needed'),
                    p("Enter the general cooking tools needed, separated by semi-colons ';'",
                        style = 'font-style: italic;'),
                    customTextAreaInput(inputId = ns('meal-new-tools'), label = 'Tools',
                        labelColor = '#5cb874', width = '100%',
                        placeholder = "Separate tools with ';'"),
                )
            ),

            fluidRow(
                column(width = 12,
                h5('Instructions'),
                    p("Enter the general cooking steps needed, separated by semi-colons ';'",
                        style = 'font-style: italic;'),
                customTextAreaInput(inputId = ns('meal-new-inst'), label = 'Instructions',
                    labelColor = '#5cb874', width = '100%', placeholder = "Separate steps with ';'"),
                )
            ),

            # customModalDialog arguments -----

            session = session,
            title = uiOutput(ns('ttl')),
            size = 'fs',
            easyClose = FALSE,
            fade = FALSE,
            footer = fluidRow(class = 'modal-footer-row',
              actionButton(ns('createMeal'), label = 'Save', class = 'btn btn-success', class = 'riv'),
              actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
                           class = 'riv', class = 'getstarted')
            ) # end footer
        ) # End customModalDialog
    ) # end tagList
}

#' meal_create Server Functions
#' @description Server function for create meal modal.
#' @noRd
mod_meal_create_server <- function(id, data = LOCAL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    LOCAL <- data
    luMtypes <- c('Choose meal type...','Breakfast','Lunch','Dinner',
                  'Dessert','Appetizer','Cocktail')
    # Observer list

    createdObservers <- c()

    # Observe add ingredient button -----

    observeEvent(input[['addIngredient']], {

      if(input[['selectIngredient']] %in% LOCAL$createMealDF$INGREDIENT){
        showNotification('This ingredient already exists for this meal!', type = 'error', duration = 10)
        return(NULL)
      }

      if(input[['meal-new-name']] == ''){
        showNotification('Meal name cannot be blank!', type = 'error', duration = 10)
        return(NULL)
      }

      if(input[['meal-new-type']] == 'Choose meal type...' | input[['meal-new-type']] == ''){
        showNotification('Meal type cannot be blank!', type = 'error', duration = 10)
        return(NULL)
      }

      if(input[['selectIngredient']] == 'Start typing to search...' | input[['selectIngredient']] == '') {
        showNotification('Please select an ingredient!', type = 'error', duration = 10)
        return(NULL)
      }

      # Create a record from LOCAL$ALL_DATA headers -----
      ingRow <- which(LOCAL$LU_INGREDIENTS$INGREDIENT == input[['selectIngredient']])

      # Get DB max meal ID
      maxDbMealID <- getMaxMealID()

      newRecord <- data.frame(
        MEAL_ID = maxDbMealID + 1,
        MEAL_NAME = input[['meal-new-name']],
        MEAL_TYPE = input[['meal-new-type']],
        MEAL_DESCRIPTION = input[['meal-new-desc']],
        INGREDIENT_ID = LOCAL$LU_INGREDIENTS$INGREDIENT_ID[ingRow],
        INGREDIENT_CATEGORY = LOCAL$LU_INGREDIENTS$INGREDIENT_CATEGORY[ingRow],
        INGREDIENT = LOCAL$LU_INGREDIENTS$INGREDIENT[ingRow],
        INGREDIENT_DESCRIPTION = LOCAL$LU_INGREDIENTS$INGREDIENT_DESCRIPTION[ingRow],
        SERVING_SIZE_DESCRIPTION = LOCAL$LU_INGREDIENTS$SERVING_SIZE_DESCRIPTION[ingRow],
        SERVING_SIZE_FACTOR = LOCAL$LU_INGREDIENTS$SERVING_SIZE_FACTOR[ingRow],
        STORAGE_DESCRIPTION = LOCAL$LU_INGREDIENTS$STORAGE_DESCRIPTION[ingRow],
        UPTIME = isolate(Sys.Date()),
        UPUSER = LOCAL$userName,
        USER_ID = LOCAL$userID
      ) %>%
      mutate(
        MEAL_UNIQUE_ID = paste0(MEAL_ID,'_',MEAL_TYPE,'_','0'),
        INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID)
      )

      # Add ingredient to createMealDF
      if(nrow(LOCAL$createMealDF) == 0) {
        LOCAL$createMealDF <- LOCAL$ALL_DATA[0,] %>%
          bind_rows(newRecord)
      } else {
        LOCAL$createMealDF <- LOCAL$createMealDF %>%
          bind_rows(newRecord)
      }

      # Reset select ingredient picker
      showNotification(paste(input[['selectIngredient']],'added to this meal.'),
                       type = 'message', duration = 5)

      updateSelectInput(session,'selectIngredient', selected = 'Start typing to search...')


    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # Observe new ingredient button -----

    observeEvent(input[['newIngredient']], {
      withProgress(message = 'New Ingredient Creation',
        detail = 'saving new ingredient to database...', {
          setProgress(0.5)
            newIngredientResponse(input, output, session, data = LOCAL)
          setProgress(0.9)
      })
    })

    # Observe new ingredient quantity input and update multiplier -----

    observeEvent(input[['ing-new-qty']],{
      noPeopleCalc <- as.numeric(input[['ing-new-hypPeople']])
          updateTextInput(session, inputId = 'ing-new-ssf',
            value = round(as.numeric(input[['ing-new-qty']]) / noPeopleCalc,3)
          )
    }, ignoreInit = TRUE)

    # Observe new hypothetical NoPeople input and update multiplier -----

    observeEvent(input[['ing-new-hypPeople']],{
      noPeopleCalc <- as.numeric(input[['ing-new-hypPeople']])
      ingQty <- if(is.na(as.numeric(input[['ing-new-qty']]))) {1} else {as.numeric(input[['ing-new-qty']])}
          updateTextInput(session, inputId = 'ing-new-ssf',
                          value = round(ingQty / noPeopleCalc,3)
          )

          updateTextInput(session, inputId = 'ing-new-qty',
                          value = ingQty
          )
    }, ignoreInit = TRUE)

    # Observe delete ingredient buttons -----

    observe({
      ingredientUniqueIDs <- unique(LOCAL$createMealDF$INGREDIENT_UNIQUE_ID)
      delIngIDs <- paste0('del-ing-',ingredientUniqueIDs)
      delIngIDs <- delIngIDs[!delIngIDs %in% createdObservers]
      if(length(delIngIDs) > 0){
        map(delIngIDs, ~ observeEvent(input[[.x]], {
          createMealDelIng(input, output, session, id = .x, data = LOCAL)
        }, ignoreInit = TRUE, ignoreNULL = TRUE, autoDestroy = TRUE)
        )
        createdObservers <<- c(createdObservers,delIngIDs)
      }
    })

    # Observe SAVE createMeal button -----

    observeEvent(input$createMeal, {

      shinyjs::disable('createMeal')

      withProgress(message = 'New Meal Creation', detail = 'saving to database...', {
        setProgress(0.5)
          createMealModalSave(input, output, session, data = LOCAL)
        setProgress(0.9)
      })

      shinyjs::enable('createMeal')

    })

    # Cancel create meal modal button upper right corner -----

    observeEvent(input$editMealModalClose_1,{
      LOCAL$editMealModalSwitch <- FALSE
      LOCAL$createMealDF <- data.frame()
      removeModal()
    })

    # Cancel create meal modal button footer -----

    observeEvent(input$editMealModalClose_2,{
      LOCAL$editMealModalSwitch <- FALSE
      LOCAL$createMealDF <- data.frame()
      removeModal()
    })


    # UI OUTPUTS -----

    # Create meal modal dynamic Ingredient list -----

    output$modalIngs <- renderUI({
      if(nrow(LOCAL$createMealDF) == 0){
        p('Ingredients will appear here as added.')
      } else {
        rows <- c(1:nrow(LOCAL$createMealDF))
        map(rows, ~ editMealIngredientInputs(input, output, session,
          data = isolate(LOCAL$createMealDF[.x,]), displayQty = FALSE)
        )
      }
    })

    output$modalSelIng <- renderUI({
      #  if(nrow(LOCAL$editMealDF) == 0){return(NULL)}
      selectIngredients(input, output, session,data = LOCAL$LU_INGREDIENTS)
    })

    output$modalNewIng <- renderUI({
      #  if(nrow(LOCAL$editMealDF) == 0){return(NULL)}
      createIngredients(input, output, session, data = isolate(LOCAL))
    })

    # Dynamic Title -----

    observe({
      output$ttl <- renderUI(h4(paste('Creating New Meal |', input[['meal-new-name']]), style = 'color: #5cb874;'))
      output$ttl2 <- renderUI(h5(paste('Add Ingredients |', input[['meal-new-name']])))
      output$mealTypes <- renderText(luMtypes)
    })


    #####RETURN LOCAL DATA OBJECT#####
    return(LOCAL)

    #####END MODULE SERVER#####

  })
}
