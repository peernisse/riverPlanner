#' menu UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import bsplus
#' @importFrom shiny NS tagList
#' @noRd
mod_menu_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;',
         h3('Select Meals'),
         bs_accordion(id = ns('meals')) %>%
           bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
           #bs_append(title = NULL, content = NULL) %>%
           bs_append(title = "Breakfasts", content = uiOutput(ns('breakfast'))) %>%
           bs_append(title = "Lunches", content = uiOutput(ns('lunch'))) %>%
           bs_append(title = 'Dinners', content = uiOutput(ns('dinner'))) %>%
           bs_append(title = 'Appetizers', content = uiOutput(ns('appetizer'))) %>%
           bs_append(title = 'Desserts', content = uiOutput(ns('dessert')))
      )
    ),

    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;',
             h3('Trip Menu'),
             #uiOutput(ns('myMenu')),
             uiOutput(ns('dayMenu'))

      )
    )
  )
}

#' menu Server Functions
#' @param id Internal parameters for {shiny}.Must match id of mod_menu_ui
#' @param data The global reactiveValues data object returned from mod_data_server
#' @noRd
mod_menu_server <- function(id, data){
  moduleServer(id, function(input, output, session){

    # Reactive Objects
    ns <- session$ns
    LOCAL <- data

    #####REACTIVE BUTTON OBSERVERS#####
    # Add meal button IDs
    #TODO this may come into play when I add the create new meal part

    # View meal button IDs
    #TODO this may come into play when I add the create new meal part

    # Delete button IDs
    new_del_button <- reactiveVal(0)
    delMealButtonIDs <- c()

    # Edit button IDs
    new_edit_button <- reactiveVal(0)
    editMealButtonIDs <- c()

    # Add meal button observers -----

    #addButtonIDs <- reactive(LOCAL$ALL_DATA %>% pull(MEAL_ADD_ID) %>% unique(.))
    addButtonIDs <- reactive(LOCAL$ALL_DATA$MEAL_ADD_ID %>% unique(.))

    map(isolate(addButtonIDs()), ~ observeEvent(input[[.x]],{

      # Validate user has selected a river day

        if(is.na(as.numeric(input[[paste0('rd-',gsub('add-','',.x))]]))){
          showNotification('Select River Day!', type = 'error', duration = 10)
          return(NULL)
        }

      # Gather user inputs, calculate mealId and mealUniqueId and make new record to append to myMeals

      riverDay <- input[[paste0('rd-',gsub('add-','',.x))]] %>% as.numeric()
      mealType <- input[[paste0('mt-',gsub('add-','',.x))]]
      mealId <- gsub('add-','',.x)
      mealUniqueId <- paste0(mealId,'_',mealType,'_',riverDay)

      # TODO this assignment of people will need to be flexible for changing group size during the trip
      newRecord <- LOCAL$ALL_DATA %>% filter(MEAL_ADD_ID == .x) %>%
          mutate(RIVER_DAY = riverDay,
                 NO_ADULTS = LOCAL$noAdults,
                 NO_KIDS = LOCAL$noKids,
                 NO_PEOPLE_CALC = LOCAL$noPeopleCalc,
                 MEAL_TYPE = mealType,
                 MEAL_UNIQUE_ID = mealUniqueId,
                 INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID),
                 QTY = ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR),
                 MEAL_NOTES = ''
          )

      # Validate that that meal on that day does not already exist
        if(nrow(LOCAL$myMeals) > 0 & paste0(gsub('add-','',.x),'_',mealType,'_',riverDay) %in% LOCAL$myMeals$MEAL_UNIQUE_ID){
          showNotification('This Meal Exists on this Day Already!', type = 'error', duration = 5)
          return(NULL)

        }

      # Append new record to myMeals
#browser()
        LOCAL$myMeals <- LOCAL$myMeals %>% bind_rows(newRecord)

      # Notify meal added to myMenu

        showNotification(paste(unique(newRecord$MEAL_NAME),'added to trip menu below!'), type = 'default', duration = 5)

      # Reset input choices
        #TODO (maybe I will do this, maybe leave them)

      # Add a delete button listener for this menu card
        delMealButtonIDs <<- c(delMealButtonIDs,paste0('del-',mealUniqueId))

        #Increment the delete button counter
        isolate({
          val <- new_del_button()
          val <- val + 1
          new_del_button(val)
        })

      # Add an edit button listener for this menu card
        editMealButtonIDs <<- c(editMealButtonIDs,paste0('edit-',mealUniqueId))

        #Increment the edit button counter
        isolate({
          val <- new_edit_button()
          val <- val + 1
          new_edit_button(val)
        })


      })
    )

#--------------------------------------------------------

  # Create View meal button observer -----

  #viewButtonIDs <- reactive(LOCAL$ALL_DATA %>% pull(MEAL_VIEW_ID) %>% unique(.))
  viewButtonIDs <- reactive(LOCAL$ALL_DATA$MEAL_VIEW_ID %>% unique(.))
  map(isolate(viewButtonIDs()), ~ observeEvent(input[[.x]],{viewMeal(session, id = .x, data = LOCAL)}))

  # Create menu card delete and edit button observers -----

  observe({
    req(nrow(LOCAL$myMeals) > 0)

    # Delete button observers -----
    del_id <- delMealButtonIDs[new_del_button()]
    map(del_id, ~ observeEvent(input[[.x]],{delMealResponse(id = .x, data = LOCAL)},
                           ignoreInit = TRUE, once = TRUE)
    )

    # Edit button observers -----
    edit_id <- editMealButtonIDs[new_edit_button()]
    map(edit_id, ~
          observeEvent(input[[.x]],{
            showModal(
              editMealModal(input, output, session, mealUniqueID = gsub('edit-','',.x), data = LOCAL)
            )
          }, ignoreInit = TRUE)
    )
  })

  # Reactive updates in edit meal modal -----

  observe({

    if(LOCAL$editMealModalSwitch == TRUE) {

      # Get input IDs to observe -----

      noAdultsID <- paste0('editMeal-noAdults-',LOCAL$editMealMealUniqueID)
      noKidsID <- paste0('editMeal-noKids-',LOCAL$editMealMealUniqueID)
      noPeopleCalcID <- paste0('editMeal-noPeopleCalc-',LOCAL$editMealMealUniqueID)

      ingredientUniqueIDs <- unique(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID)
      qtyIDs <- paste0('ing-qty-',ingredientUniqueIDs)
      ssfIDs <- paste0('ing-ssf-',ingredientUniqueIDs)

      # Observe change in NoAdults -----

      observeEvent(input[[noAdultsID]],{
        editMealAdjPeople(input, output, session, data = LOCAL)
      }, ignoreInit = TRUE)

      # Observe change in NoKids -----

      observeEvent(input[[noKidsID]],{
        editMealAdjPeople(input, output, session, data = LOCAL)
      }, ignoreInit = TRUE)

      # Observe manual change in ingredient quantities -----

      map(qtyIDs, ~ observeEvent(input[[.x]],{
        editMealAdjQty(input, output, session, id = .x, data = LOCAL)
        }, ignoreInit = TRUE)
      )
    } # end if
  }) # end observe

#---------------------------------------------------------------

  # Update meal button observe

  observeEvent(input$updateMeal,{
# TODO finish this and make all this editMealModal stuff a new module

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

  #####UI OUTPUTS#####

  # Make meal card rows by meal type

  output$breakfast <- makeMealCards(input, output, session, mtype = 'Breakfast', LOCAL)
  output$lunch <- makeMealCards(input, output, session, mtype = 'Lunch', LOCAL)
  output$dinner <- makeMealCards(input, output, session, mtype = 'Dinner', LOCAL)
  output$appetizer <- makeMealCards(input, output, session, mtype = 'Appetizer', LOCAL)
  output$dessert <- makeMealCards(input, output, session, mtype = 'Dessert', LOCAL)


  # Make Menu card rows by day
  #TODO make this open the div for the menu day just updated

  output$dayMenu <- renderUI({makeDayBoxes(session, outer = 'dayBoxes', data = LOCAL$myMeals)})

  # End of menu module server -----

  })
}
