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
      column(width = 4,
        h1('Select Meals'),
        bs_accordion(id = ns('meals')) %>%
          bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
          bs_append(title = NULL, content = NULL) %>%
          bs_append(title = "Breakfasts", content = uiOutput(ns('breakfast'))) %>%
          bs_append(title = "Lunches", content = uiOutput(ns('lunch'))) %>%
          bs_append(title = 'Dinners', content = uiOutput(ns('dinner'))) %>%
          bs_append(title = 'Appetizers', content = uiOutput(ns('appetizer'))) %>%
          bs_append(title = 'Desserts', content = uiOutput(ns('dessert')))
      ),
      column(width = 8,
        h1('My Menu'),
        uiOutput(ns('myMenu'))
      ),


    )
  )
}

#' menu Server Functions
#' @param id Internal parameters for {shiny}.Must match id of mod_menu_ui
#' @param data,mdata The global reactiveValues data object returned from mod_data_server
#' @noRd
mod_menu_server <- function(id, data = mdata){
  moduleServer( id, function(input, output, session){

    # Reactive Objects
    ns <- session$ns
    LOCAL <- data

    #####REACTIVE OBSERVERS#####

    addButtonIDs <- reactive(LOCAL$ALL_DATA %>% pull(MEAL_ADD_ID) %>% unique(.))
    map(isolate(addButtonIDs()), ~ observeEvent(input[[.x]],{

      # Validate user has selected a river day

        if(is.na(as.numeric(input[[paste0('rd-',gsub('add-','',.x))]]))){
          showNotification('Select River Day!', type = 'error', duration = 10)
          return(NULL)
        }

      # Gather user inputs and make new record to append to myMeals

      riverDay <- input[[paste0('rd-',gsub('add-','',.x))]] %>% as.numeric()
      mealType <- input[[paste0('mt-',gsub('add-','',.x))]]
      newRecord <- LOCAL$ALL_DATA %>% filter(MEAL_ADD_ID == .x) %>%
          mutate(RIVER_DAY = riverDay,
                 MEAL_TYPE = mealType,
                 MEAL_UNIQUE_ID = paste0(MEAL_ID,'_',MEAL_TYPE,'_',RIVER_DAY))

      # Validate that that meal on that day does not already exist
        if(nrow(LOCAL$myMeals) > 0 & paste0(gsub('add-','',.x),'_',mealType,'_',riverDay) %in% LOCAL$myMeals$MEAL_UNIQUE_ID){
          showNotification('This Meal Exists on this Day Already!', type = 'error', duration = 5)
          return(NULL)

        }

      # Append new record to myMeals

        LOCAL$myMeals <- LOCAL$myMeals %>% bind_rows(newRecord)

      # Reset input choices (maybe I will do this, maybe leave them)
      # ...

      })
    )

    # Delete meal button
    # TODO THis needs to be on the myMeals menu item and delete just that specific meal

    delButtonIDs <- reactive(LOCAL$ALL_DATA %>% pull(MEAL_DEL_ID) %>% unique(.))
    map(isolate(delButtonIDs()), ~ observeEvent(input[[.x]],{
      LOCAL$myMeals <- subset(LOCAL$myMeals, !MEAL_DEL_ID %in% .x)
    }))

    #####UI OUTPUTS#####

    output$breakfast <- makeMealCards(input, output, session, mtype = 'Breakfast', LOCAL)
    output$lunch <- makeMealCards(input, output, session, mtype = 'Lunch', LOCAL)
    output$dinner <- makeMealCards(input, output, session, mtype = 'Dinner', LOCAL)
    output$appetizer <- makeMealCards(input, output, session, mtype = 'Appetizer', LOCAL)
    output$dessert <- makeMealCards(input, output, session, mtype = 'Dessert', LOCAL)

    # Make meal boxes

    mealBoxes <- reactive(makeMealBoxes(session, outer = 'mealBoxes', data = LOCAL$myMeals))

    output$myMenu <- renderUI({
      #browser()
      ns <- session$ns
      mealBoxes()

    })



  })
}
