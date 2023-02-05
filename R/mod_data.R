#' data UI Function
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#'
#' @noRd
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    #No UI for data module. Unused.
  )
}

#' data Server Function
#' @description Handles database queries and updates.
#'
#' @importFrom googlesheets4 read_sheet gs4_auth
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate case_when select group_by summarize arrange pull left_join bind_rows bind_cols distinct
#'
#' @noRd
mod_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####GOOGLE SHEETS URL AND GET BASE DATA####
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    LU_USERS <- read_sheet(url, sheet = "LU_USERS")

    LU_TRIPS <- read_sheet(url, sheet = "LU_TRIPS") %>%
      replace(is.na(.),'')

    XREF_INGREDIENT <- read_sheet(url, sheet = "XREF_INGREDIENT")

    LU_MEAL_TYPE <- read_sheet(url, sheet = "LU_MEAL_TYPE") %>%
      mutate(
        MEAL_TYPE = factor(MEAL_TYPE, levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail', 'Snack'))
      )

    LU_MEAL <- read_sheet(url, sheet = 'LU_MEAL') %>%
      mutate(
        MEAL_TYPE = factor(MEAL_TYPE, levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail', 'Snack')),
        MEAL_ADD_ID = paste0('add-',MEAL_ID),
        MEAL_DEL_ID = paste0('del-',MEAL_ID),
        MEAL_VIEW_ID = paste0('view-',MEAL_ID),
        MEAL_EDIT_ID = paste0('edit-',MEAL_ID),
        MEAL_UNIQUE_ID = '',
        INGREDIENT_UNIQUE_ID = '',
        RIVER_DAY = NA_real_)

    LU_INGREDIENTS <- read_sheet(url, sheet = "LU_INGREDIENTS")

    ALL_DATA <- LU_MEAL %>%
      select(-c(UPTIME,UPUSER)) %>%
      left_join(XREF_INGREDIENT %>% select(MEAL_ID,INGREDIENT_ID), by = 'MEAL_ID') %>%
      left_join(LU_INGREDIENTS %>% select(-c(UPTIME,UPUSER)), by = 'INGREDIENT_ID')

    ####INSTANTIATE REACTIVE VALUES DATA OBJECT#####

    LOCAL <- reactiveValues(
      userName = as.character('dev_01'),
      LU_USERS = LU_USERS,
      LU_TRIPS = LU_TRIPS,
      XREF_INGREDIENT = XREF_INGREDIENT,
      LU_MEAL_TYPE = LU_MEAL_TYPE,
      LU_MEAL = LU_MEAL,
      LU_INGREDIENTS = LU_INGREDIENTS,
      tripID = ifelse(length(LU_TRIPS$TRIP_ID) == 0, 1, max(LU_TRIPS$TRIP_ID) + 1),
      tripName = character(),
      tripDesc = character(),
      loadTripMode = FALSE,
      noAdults = 1,
      noKids = 0,
      noPeople = 1,
      noPeopleCalc = 1,
      myMeals = data.frame(),
      editMealDF = data.frame(),
      editMealModalSwitch = FALSE,
      editMealMealUniqueID = NULL,
      createMealDF = data.frame(),
      ALL_DATA = ALL_DATA
    )

    rm(XREF_INGREDIENT,LU_USERS, LU_TRIPS, LU_MEAL_TYPE,LU_MEAL,LU_INGREDIENTS,ALL_DATA)
    gc()

    return(LOCAL)

  })
}

