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
        # bs_accordion(id = ns('myMeals')) %>%
        #   bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
        #   bs_append(title = 'Selected', content = tableOutput(outputId = ns('selMeals'))
        # ),
        uiOutput(ns('myMeals'))
      )
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
    buttonIDs <- reactiveValues() # Stores menu button ids when clicked. Represents MEAL_ID.

    # Make cards for Select Meals

    output$breakfast <- makeMealCards(input, output, session, mtype = 'Breakfast', LOCAL, buttonIDs)
    output$lunch <- makeMealCards(input, output, session, mtype = 'Lunch', LOCAL, buttonIDs)
    output$dinner <- makeMealCards(input, output, session, mtype = 'Dinner', LOCAL, buttonIDs)
    output$appetizer <- makeMealCards(input, output, session, mtype = 'Appetizer', LOCAL,  buttonIDs)
    output$dessert <- makeMealCards(input, output, session, mtype = 'Dessert', LOCAL, buttonIDs)

    # Make meal boxes

    output$myMeals <- renderUI({
      ns <- session$ns
      makeMealBoxes(session, outer = 'mealBoxes', data = LOCAL$myMeals)
    })

  })
}
