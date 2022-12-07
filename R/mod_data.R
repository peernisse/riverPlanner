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
#' @importFrom dplyr filter mutate select group_by summarize pull left_join bind_rows bind_cols
#'
#' @noRd
mod_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####GOOGLE SHEETS URL AND GET BASE DATA TO GLOBAL####
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    googlesheets4::gs4_auth(path = './inst/app/www/.token/rivermenu-96e6b5c5652d.json')

    XREF_INGREDIENT <- googlesheets4::read_sheet(url, sheet = "XREF_INGREDIENT")

    LU_MEAL_TYPE <- googlesheets4::read_sheet(url, sheet = "LU_MEAL_TYPE")

    LU_MEAL <- googlesheets4::read_sheet(url, sheet = 'LU_MEAL') %>%
      mutate(MEAL_ADD_ID = paste0('add-',MEAL_ID),
             MEAL_DEL_ID = paste0('del-',MEAL_ID),
             MEAL_UNIQUE_ID = '',
             RIVER_DAY = NA_real_)

    LU_INGREDIENTS <- googlesheets4::read_sheet(url, sheet = "LU_INGREDIENTS")

    ALL_DATA <- LU_MEAL %>%
      select(-c(UPTIME,UPUSER)) %>%
      left_join(XREF_INGREDIENT %>% select(MEAL_ID,INGREDIENT_ID), by = 'MEAL_ID') %>%
      left_join(LU_INGREDIENTS %>% select(-c(UPTIME,UPUSER)), by = 'INGREDIENT_ID')

    ####INSTANTIATE REACTIVE VALUES DATA OBJECT#####

    LOCAL <- reactiveValues(
      XREF_INGREDIENT = XREF_INGREDIENT,
      LU_MEAL_TYPE = LU_MEAL_TYPE,
      LU_MEAL = LU_MEAL,
      LU_INGREDIENTS = LU_INGREDIENTS,
      myMeals = data.frame(),
      ALL_DATA = ALL_DATA
    )

    rm(XREF_INGREDIENT,LU_MEAL_TYPE,LU_MEAL,LU_INGREDIENTS,ALL_DATA)
    gc()

    return(LOCAL)

  })
}
