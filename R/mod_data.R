#' data UI Function
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect
#' @importFrom RMariaDB MariaDB
#'
#' @noRd
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$li(
      a(class = "nav-link scrollto", style = "font-size:16px; color: #5cb874",
        div(class = "form-group shiny-input-container", style = 'width: auto;',
          tags$input(id = ns("userName"), type = "text", class = "form-control",disabled = "disabled",
            style = 'width: auto; color: #5cb874; border-color: #5cb874;
              background-color: #fff; text-align: center;', value = NA
          )
        )
      )
    ),
    tags$li(
      #actionLink(inputId = ns('logOut'), label = "Log Out", class = "nav-link scrollto")
      a(class = "nav-link scrollto", style = "font-size:16px; color: #5cb874",
        tags$button(id = ns('logOut'),
          class = 'nav-link scrollto shiny-bound-input action-button btn btn-success',
          style = "margin:5px; color: #fff;", "Log Out"
        )
      )
    )
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

    #browser()
    ###DEV###
    #userName <- 'dev_01' # Will be coming in from Auth0
    #userID <- 4
    #TODO fix bug if you start new trip and first entry is a new meal with new ingredient
    #########

    # Get Auth0 username
    userName <- session$userData$auth0_info$name
    updateTextInput(session, inputId = 'userName', value = userName)

    ####GOOGLE SHEETS URL AND GET BASE DATA####
    url <- 'https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing'
    authPath <- './inst/app/www/.token/rivermenu-96e6b5c5652d.json'
    gs4_auth(path = authPath)

    LU_USERS <- read_sheet(url, sheet = "LU_USERS")

    # Establish UserID in DB if not there

    if(!userName %in% LU_USERS$USERNAME){
      newid <- max(LU_USERS$USER_ID) + 1
      if(length(grep('@', userName)) != 0){email <- userName} else {email <- ''}
      newrecord <- data.frame(
        USER_ID = as.numeric(newid),
        USERNAME = userName,
        EMAIL = email,
        UPTIME = Sys.time(),
        UPUSER = userName
      )

      dbUpdate(newrecord, 'LU_USERS', data = NULL)

      LU_USERS <- read_sheet(url, sheet = "LU_USERS")
      userID <- LU_USERS[which(LU_USERS$USERNAME == userName),'USER_ID'] %>% pull()

    } else {

      userID <- LU_USERS[which(LU_USERS$USERNAME == userName),'USER_ID'] %>% pull()

    }

    # Get remaining dataframes filtered by userID

    LU_TRIPS <- read_sheet(url, sheet = "LU_TRIPS") %>%
      filter(USER_ID %in% c(0,userID)) %>%
      replace(is.na(.),'')


    XREF_INGREDIENT <- read_sheet(url, sheet = "XREF_INGREDIENT") %>%
      filter(USER_ID %in% c(0,userID))

    LU_MEAL_TYPE <- read_sheet(url, sheet = "LU_MEAL_TYPE") %>%
      filter(USER_ID %in% c(0,userID)) %>%
      mutate(
        MEAL_TYPE = factor(MEAL_TYPE, levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail', 'Snack'))
      )

    LU_MEAL <- read_sheet(url, sheet = 'LU_MEAL') %>%
      filter(USER_ID %in% c(0,userID)) %>%
      mutate(
        MEAL_TYPE = factor(MEAL_TYPE, levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail', 'Snack')),
        MEAL_ADD_ID = paste0('add-',MEAL_ID),
        MEAL_DEL_ID = paste0('del-',MEAL_ID),
        MEAL_VIEW_ID = paste0('view-',MEAL_ID),
        MEAL_EDIT_ID = paste0('edit-',MEAL_ID),
        MEAL_UNIQUE_ID = '',
        INGREDIENT_UNIQUE_ID = '',
        RIVER_DAY = NA_real_)

    LU_INGREDIENTS <- read_sheet(url, sheet = "LU_INGREDIENTS") %>%
      filter(USER_ID %in% c(0,userID))

    gs4_deauth()


    ALL_DATA <- LU_MEAL %>%
      select(-c(UPTIME, UPUSER, USER_ID)) %>%
      left_join(XREF_INGREDIENT %>% select(MEAL_ID,INGREDIENT_ID), by = 'MEAL_ID') %>%
      left_join(LU_INGREDIENTS %>% select(-c(UPTIME,UPUSER, USER_ID)), by = 'INGREDIENT_ID') %>%
      mutate(
        USERNAME = userName,
        USER_ID = userID
      )

    ####INSTANTIATE REACTIVE VALUES DATA OBJECT#####

    LOCAL <- reactiveValues(
      userName = userName,
      userID = userID,
      LU_USERS = LU_USERS,
      LU_TRIPS = LU_TRIPS,
      XREF_INGREDIENT = XREF_INGREDIENT,
      LU_MEAL_TYPE = LU_MEAL_TYPE,
      LU_MEAL = LU_MEAL,
      LU_INGREDIENTS = LU_INGREDIENTS,
      tripID = character(),
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
      exportMenuModalSwitch = FALSE,
      ALL_DATA = ALL_DATA
    )

    rm(XREF_INGREDIENT,LU_USERS, LU_TRIPS, LU_MEAL_TYPE,LU_MEAL,LU_INGREDIENTS,ALL_DATA)
    gc()

    # UI Outputs -----
    #output$userName <- renderText({isolate(LOCAL$userName)})

    # output$userName <- renderUI({
    #
    #   tags$button(class = "getstarted",
    #     style = "background-color: #FFFFFF;",
    #     disabled = "disabled",
    #     LOCAL$userName
    #   )
    #
    # })

    #####OBSERVERS#####
    observeEvent(input$logOut, {logout()})

    # Return LOCAL reactive values object

    return(LOCAL)

  })
}

