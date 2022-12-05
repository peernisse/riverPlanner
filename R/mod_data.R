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
#' @importFrom dplyr filter mutate select group_by summarize pull
#'
#' @noRd
mod_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####GOOGLE SHEETS URL####
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    googlesheets4::gs4_auth(path = './inst/app/www/.token/rivermenu-96e6b5c5652d.json')

    ####INSTANTIATE REACTIVE VALUES DATA OBJECT#####

    LOCAL <- reactiveValues(
      XREF_INGREDIENT = googlesheets4::read_sheet(url, sheet = "XREF_INGREDIENT"),
      LU_MEAL_TYPE = googlesheets4::read_sheet(url, sheet = "LU_MEAL_TYPE"),
      LU_MEAL = googlesheets4::read_sheet(url, sheet = 'LU_MEAL'),
      LU_INGREDIENTS = googlesheets4::read_sheet(url, sheet = "LU_INGREDIENTS"),
      myMeals = list()
    )

    return(LOCAL)
  })
}

