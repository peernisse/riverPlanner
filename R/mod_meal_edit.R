#' meal_edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_meal_edit_ui <- function(id, session){
  ns <- NS(id)
  tagList(
    customModalDialog(

      h5(textOutput(ns('desc'))),
      p('If there will be a change of people during the trip, you can change the people inputs here
           and they will just affect this meal on this day. You will need to do this for each meal that
          has a change of people from the original numbers set up in `Trip Info`.'),
      p('Each ingredient quantity can be edited and the multiplier will update for this meal only. This change
          is not global and must be done again when using the same ingredient elsewhere, if desired.'),

      uiOutput(ns('mealTripInfo')),

      fluidRow(style = 'margin-top:20px;',
               column(width = 12,
                      h5('Ingredients/Quantities'),
                      p('--If you don\'t like the calculated quantity for an ingredient, you can adjust it
                and will adjust the multiplier for that ingredient for this meal.--',style = 'font-style: italic;'),
                      p('--Click TRASH to remove an ingredient from this meal.--',style = 'font-style: italic;'),
                      p('--To ADD a different ingredient, use the ingredient picker below.--', style = 'font-style: italic;'),
                      uiOutput(ns('modalIngs'))
               )
      ),
      fluidRow(style = 'margin-top:20px;',
               column(width = 12,
                      h5(textOutput(ns('modalTitle2'))),
                      p('--Start typing a word to filter the dropdown. Click + to add ingredient to this meal.--',
                        style = 'font-style: italic;'),
                      uiOutput(ns('modalSelIng'))
               )
      ),
      fluidRow(style = 'margin-top:20px;',
         column(width = 12,
                h5('Create New Ingredient'), #TODO make an i icon with info popover
                icon(name = 'info'),
                p('--Your new ingredient will appear in the Add Ingredient dropdown above.--',
                  style = 'font-style: italic;'),
                uiOutput(ns('modalNewIng'))
         )
      ),

      fluidRow(style = 'margin-top:20px;',
        column(width = 12,
           h5('Meal Notes'),
           uiOutput(ns('notes'))
        )
      ),

      fluidRow(style = 'margin-top:20px;',
               column(width = 12,
                      h5('Tools'),
                      uiOutput(ns('tools'))
               )
      ),
      fluidRow(style = 'margin-top:20px;',
               column(width = 12,
                      h5('Instructions'),
                      uiOutput(ns('inst'))
               )
      ),
      session = session,
      title = h4(textOutput(ns('modalTitle')), style = 'color: #5cb874'),
      size = 'fs',
      easyClose = FALSE,
      fade = FALSE,
      footer = fluidRow(class = 'modal-footer-row',
        actionButton(ns('updateMeal'), label = 'Save', class = 'btn btn-success', class = 'riv'),
        actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
          class = 'riv', class = 'getstarted')
      )
    )
  )
}

#' meal_edit Server Functions
#'
#' @noRd
mod_meal_edit_server <- function(id, data = LOCAL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    LOCAL <- data
    createdObservers <- c()

    observe({
      #browser()

      if(length(LOCAL$tripID) > 0){

        mealUniqueID <- LOCAL$editMealMealUniqueID
        req(!length(mealUniqueID) > 1)
        req(LOCAL$editMealModalSwitch == TRUE)

        #TODO figure out why this goes 3 times and blinks

        if(nrow(LOCAL$editMealDF) == 0 | !mealUniqueID %in% LOCAL$editMealDF$MEAL_UNIQUE_ID){
          LOCAL$editMealDF <- LOCAL$myMeals %>% filter(MEAL_UNIQUE_ID == mealUniqueID)
        } else {
          LOCAL$editMealDF <- isolate(LOCAL$editMealDF)
        }
      }
    })

    # Reactive updates in edit meal modal -----

    observe({

      if(LOCAL$editMealModalSwitch == TRUE) {

        # Get input IDs to observe -----

        noAdultsID <- paste0('editMeal-noAdults-',LOCAL$editMealMealUniqueID)
        noKidsID <- paste0('editMeal-noKids-',LOCAL$editMealMealUniqueID)
        noPeopleCalcID <- paste0('editMeal-noPeopleCalc-',LOCAL$editMealMealUniqueID)
        ###########HERE----------------------------------------------------------------
        #TODO try setting these 4 things in LOCAL and get rid of this outer observe,
        #just start from the if(LOCAL$editMealModalSwitch == TRUE)

        ingredientUniqueIDs <- unique(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID)

        #####################------------------------------------------

        #This can maybe go away
        #ssfIDs <- paste0('ing-ssf-',ingredientUniqueIDs)

        #####RENDER MODAL UI ELEMENTS#####
        # Modal title -----
        output$modalTitle <- renderText({
          paste(unique(LOCAL$editMealDF$MEAL_NAME),"|",unique(LOCAL$editMealDF$MEAL_TYPE),
                "| River Day", unique(LOCAL$editMealDF$RIVER_DAY),"|",
                as.numeric(unique(LOCAL$editMealDF$NO_ADULTS)) + as.numeric(unique(LOCAL$editMealDF$NO_KIDS)),
                'People'
          )
        })

        output$modalTitle2 <- renderText({
          paste('Add Ingredients to',unique(LOCAL$editMealDF$MEAL_NAME),"|",unique(LOCAL$editMealDF$MEAL_TYPE),
                "| River Day", unique(LOCAL$editMealDF$RIVER_DAY),"|",
                as.numeric(unique(LOCAL$editMealDF$NO_ADULTS)) + as.numeric(unique(LOCAL$editMealDF$NO_KIDS)),
                'People'
          )
        })

        # Modal Meal Trip Info -----
        #TODO these all run twice

        output$mealTripInfo <- renderUI({
          if(nrow(LOCAL$editMealDF) == 0){return(NULL)}
          editMealTripInfoInputs(input, output, session, data = isolate(LOCAL))
        })

        # Modal Ingredient list -----

        output$modalIngs <- renderUI({
          if(nrow(LOCAL$editMealDF) == 0){return(NULL)}
          rows <- c(1:nrow(LOCAL$editMealDF))
          map(rows, ~ editMealIngredientInputs(input, output, session,data = isolate(LOCAL$editMealDF[.x,])))
        })

        # Modal editMeal select ingredients dropdown -----

        output$modalSelIng <- renderUI({

          #  if(nrow(LOCAL$editMealDF) == 0){return(NULL)}
          selectIngredients(input, output, session,data = LOCAL$LU_INGREDIENTS)
        })

        # Modal New Ingredient form -----

        output$modalNewIng <- renderUI({
          #  if(nrow(LOCAL$editMealDF) == 0){return(NULL)}
          createIngredients(input, output, session, data = isolate(LOCAL))
        })

        #####OBSERVE MODAL INPUTS#####


        # Observe change in NoAdults -----

        observeEvent(input[[noAdultsID]],{
          editMealAdjPeople(input, output, session, data = isolate(LOCAL))
        }, ignoreInit = TRUE)

        # Observe change in NoKids -----

        observeEvent(input[[noKidsID]], {
          editMealAdjPeople(input, output, session, data = isolate(LOCAL))
        }, ignoreInit = TRUE)


        # Observe manual change in ingredient quantities -----

        qtyIDs <- paste0('ing-qty-',ingredientUniqueIDs)
        qtyIDs <- qtyIDs[!qtyIDs %in% createdObservers]
        if(length(qtyIDs) > 0){
          map(qtyIDs, ~ observeEvent(input[[.x]], {
            editMealAdjQty(input, output, session, id = .x, data = LOCAL)
          }, ignoreInit = TRUE)
          )
          createdObservers <<- c(createdObservers,qtyIDs)
        }

        # Observe delete ingredient buttons -----

        delIngIDs <- paste0('del-ing-',ingredientUniqueIDs)
        delIngIDs <- delIngIDs[!delIngIDs %in% createdObservers]
        if(length(delIngIDs) > 0){

          map(delIngIDs, ~ observeEvent(input[[.x]], {
            editMealDelIng(input, output, session, id = .x, data = LOCAL)
          }, ignoreInit = TRUE, ignoreNULL = TRUE, autoDestroy = TRUE)
          )
          createdObservers <<- c(createdObservers,delIngIDs)
        }


        # End of when modal is open code -----

      } # end if
    }) # end observe
# ------------------------------------------------------------------------

    # Observe add ingredient button -----

    observeEvent(input[['addIngredient']], {
      editMealAddIng(input, output, session, data = LOCAL)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Observe new ingredient quantity input and update multiplier -----

    observeEvent(input[['ing-new-qty']],{

      req(LOCAL$editMealModalSwitch == TRUE)
      req(nrow(LOCAL$editMealDF) > 0)
      noPeopleCalc <- as.numeric(input[['ing-new-hypPeople']])

      updateTextInput(session, inputId = 'ing-new-ssf',
                      value = round(as.numeric(input[['ing-new-qty']]) / noPeopleCalc,3)
      )
    }, ignoreInit = TRUE)

    # Observe new hypothetical NoPeople input and update multiplier -----

    observeEvent(input[['ing-new-hypPeople']],{

      req(LOCAL$editMealModalSwitch == TRUE)
      req(nrow(LOCAL$editMealDF) > 0)
      noPeopleCalc <- as.numeric(input[['ing-new-hypPeople']])
      ingQty <- if(is.na(as.numeric(input[['ing-new-qty']]))) {1} else {as.numeric(input[['ing-new-qty']])}

      updateTextInput(session, inputId = 'ing-new-ssf',
                      value = round(ingQty / noPeopleCalc,3)
      )

      updateTextInput(session, inputId = 'ing-new-qty',
                      value = ingQty
      )
    }, ignoreInit = TRUE)

    # Observe new ingredient button -----

    observeEvent(input[['newIngredient']], {
      withProgress(message = 'New Ingredient', detail = 'saving to database...', {
        map(1:5, ~ incProgress(.x/10))
        newIngredientResponse(input, output, session, data = LOCAL)
        map(6:10, ~ incProgress(.x/10))
      })
    })


     # Update meal button observe -----

    observeEvent(input$updateMeal,{
      # TODO finish this and make all this editMealModal stuff a new module
      #browser()
      # Validate
      req(nrow(LOCAL$editMealDF) > 0)
      mealUniqueID <- LOCAL$editMealDF$MEAL_UNIQUE_ID %>% unique()
      req(mealUniqueID %in% LOCAL$myMeals$MEAL_UNIQUE_ID)



      #TODO Validate if info exists in create ingredient form, prompt to enter that and stop
      # Validate a new ingredient is not sitting in input
      if(input[['ing-new-ing']] != ''){
        showNotification('New Ingredient form has data!
            Do you need to finish creating a new ingredient?',
                         type = 'error', duration = 10
        )
        return(NULL)
      }

      # Update LOCAL$myMeals
      #TODO figure this data types discrepancy out better
      LOCAL$editMealDF$NO_ADULTS <- as.numeric(LOCAL$editMealDF$NO_ADULTS)
      LOCAL$editMealDF$NO_KIDS <- as.numeric(LOCAL$editMealDF$NO_KIDS)
      LOCAL$editMealDF$NO_PEOPLE_CALC <- as.numeric(LOCAL$editMealDF$NO_PEOPLE_CALC)
      LOCAL$editMealDF$QTY <- as.numeric(LOCAL$editMealDF$QTY)
      LOCAL$editMealDF$MEAL_NOTES <- input[['meal-notes']]

      rows <- which(LOCAL$myMeals$MEAL_UNIQUE_ID == mealUniqueID)
      LOCAL$myMeals <- LOCAL$myMeals[-rows,] %>% bind_rows(LOCAL$editMealDF)

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

      # Update LOCAL$LU_TRIPS
      # LOCAL$LU_TRIPS <- LOCAL$LU_TRIPS %>%
      #   filter(!TRIP_ID %in% trip$TRIP_ID) %>%
      #   bind_rows(trip)

      # Update DB

      withProgress(message = 'Trip Info', detail = 'saving to database...', {
        map(1:5, ~ incProgress(.x/10))
        dbUpdate(trip, 'LU_TRIPS', data = LOCAL)
        map(6:10, ~ incProgress(.x/10))
      })

      # Notify
      showNotification(paste(LOCAL$editMealDF[1,'MEAL_NAME'], 'saved to menu!'),type = 'message')

      # Clean up and close modal
      LOCAL$editMealModalSwitch <- FALSE
      LOCAL$editMealDF <- data.frame()
      removeModal()

    })

    # Cancel edit meal modal button upper right corner -----

    observeEvent(input$editMealModalClose_1,{
      LOCAL$editMealModalSwitch <- FALSE
      LOCAL$editMealDF <- data.frame()
      removeModal()
    })

    # Cancel edit meal modal button footer -----

    observeEvent(input$editMealModalClose_2,{
      LOCAL$editMealModalSwitch <- FALSE
      LOCAL$editMealDF <- data.frame()
      removeModal()
    })


    # Static UI outputs -----
    observe({
      #browser()
      # Description
      output$desc <- renderText({unique(LOCAL$editMealDF$MEAL_DESCRIPTION)})

      # Meal notes

      output$notes <- renderUI({
        ns <- session$ns
          customTextAreaInput(inputId = ns('meal-notes'),
            label = 'Meal Notes',
            value = isolate(unique(LOCAL$editMealDF$MEAL_NOTES)),
            labelColor = '#162118', width = '100%', height = NULL, cols = NULL,
            rows = NULL, placeholder = NULL, resize = 'vertical',
            labelTextColor = '#fff', disabled = NULL)
      })

      # Tools
      tools <- LOCAL$editMealDF$TOOLS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()

      output$tools <- renderUI({
        tags$ul(
          map(tools, ~ tags$li(.x))
        )
      })

      # Instructions
      inst <- LOCAL$editMealDF$INSTRUCTIONS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()

      output$inst <- renderUI({
        tags$ol(
          map(inst, ~ tags$li(.x))
        )
      })
      #rows <- c(1:nrow(LOCAL$editMealDF))
    }) # end observe

    #####RETURN LOCAL DATA OBJECT#####
    return(LOCAL)

    #####END MODULE SERVER#####

  })
}

## To be copied in the UI
# mod_meal_edit_ui("meal_edit_1")

## To be copied in the server
# mod_meal_edit_server("meal_edit_1")
