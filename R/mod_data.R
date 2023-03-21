#' data UI Function
#' @description A shiny Module handles initial data load and setup.
#' Handles the username and logout button in the page header.#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @importFrom DBI dbConnect dbGetQuery dbExecute dbDisconnect dbIsValid
#' @importFrom RMariaDB MariaDB
#' @noRd
mod_data_ui <- function(id){
    ns <- NS(id)
    tagList(
        tags$li(
            actionLink(inputId = ns('feedback'), 'Feedback')
            #tags$a(id = ns('feedback'), class="nav-link", 'Feedback')
        ),
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

#' @importFrom auth0 logoutButton logout
#' @importFrom stringr str_to_lower
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate case_when select group_by ungroup
#' @importFrom dplyr summarize arrange pull left_join bind_rows bind_cols distinct
#' @noRd
mod_data_server <- function(id){
    moduleServer( id, function(input, output, session){
        ns <- session$ns
        # Get Auth0 username
        authPkg <- session$userData$auth0_info
        authType <- authPkg$sub
        if(grepl('google', authType)){
            userName <- paste0(authPkg$nickname,'@gmail.com')
            email <- userName
        } else if(grepl('auth0', authType)){
            userName <- authPkg$name
            email <- userName
        } else {
            stop('Unknown Auth Client')
            geterrmessage()
        }

        #userName <- session$userData$auth0_info$name


        # Connect database

        con <- rivConnect()

        # Get users table and userID if it is there

        dbExecute(con, "start transaction;")
            LU_USERS <- dbGetQuery(con,'select USER_ID, USERNAME from lu_users;')
        dbExecute(con,"commit;")

        userID <- LU_USERS[which(str_to_lower(LU_USERS$USERNAME) ==
                    str_to_lower(userName)),'USER_ID'] %>% unique()

        # Establish UserID in DB if not there

        if(!str_to_lower(userName) %in% str_to_lower(LU_USERS$USERNAME) ||
            length(userID) == 0){
                dbExecute(con, "start transaction;")
                    newid <- reserveNewUserID(con, userName)
                    LU_USERS <- dbGetQuery(con,
                        paste0(
                            "select * from lu_users where USER_ID = ",
                            newid, ";"
                        )
                    )
                dbExecute(con,"commit;")
                userID <- LU_USERS$USER_ID
        }

        # Dispay username in header

        updateTextInput(session, inputId = 'userName', value = userName)

        # Get remaining dataframes filtered by userID

        LU_MEAL <- dbGetQuery(con,
            paste0('select * from lu_meal where USER_ID in (0,',userID,');')
        )

        LU_INGREDIENTS <- dbGetQuery(con,
            paste0('select * from lu_ingredients where USER_ID in (0,',userID,');')
        )

        LU_MEAL_TYPE <- dbGetQuery(con,
            paste0('select * from lu_meal_type where USER_ID in (0,',userID,');')
        )

        LU_TRIPS_DB <- dbGetQuery(con,
            paste0('select * from lu_trips where USER_ID in (',userID,');')
        )

        XREF_INGREDIENT <- dbGetQuery(con,
            paste0('select * from xref_ingredient where USER_ID in (0,',userID,');')
        )

        XREF_TRIPS <- dbGetQuery(con,
            paste0('select * from xref_trips where USER_ID in (',userID,');')
        )

        dbDisconnect(con)

        # Create LU_TRIPS

        LU_TRIPS <- XREF_TRIPS %>%
            #select(-c(UPUSER, UPTIME)) %>%
            left_join(
                LU_MEAL %>% select(c(MEAL_ID,MEAL_NAME,MEAL_DESCRIPTION,TOOLS,INSTRUCTIONS,
                                     MEAL_ADD_ID,MEAL_DEL_ID,MEAL_VIEW_ID,MEAL_EDIT_ID)),
                by = c('MEAL_ID')
            ) %>%
            left_join(
                LU_MEAL_TYPE %>% select(MEAL_TYPE_ID, MEAL_TYPE), by = c('MEAL_TYPE_ID')
            ) %>%
            left_join(
                LU_INGREDIENTS %>% select(INGREDIENT_ID, INGREDIENT_CATEGORY,INGREDIENT,
                  INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION, STORAGE_DESCRIPTION),
                by = c('INGREDIENT_ID')
            ) %>%
            left_join(LU_USERS %>% select(USER_ID, USERNAME), by = c('USER_ID')) %>%
            left_join(LU_TRIPS_DB %>% select(TRIP_ID, TRIPNAME, TRIP_DESC), by = c('TRIP_ID')) %>%
            mutate(
                MEAL_UNIQUE_ID = paste0(MEAL_ID,'_',MEAL_TYPE,'_',RIVER_DAY),
                INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID),
                # QTY = round(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)

                #QTY = ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
                QTY = case_when(
                    NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                        round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                    TRUE ~ round(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
                )
            )

        # Create ALL_DATA

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
            LU_TRIPS_DB = LU_TRIPS_DB,
            XREF_TRIPS = XREF_TRIPS,
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

        rm(XREF_INGREDIENT,LU_USERS, XREF_TRIPS, LU_TRIPS, LU_TRIPS_DB, LU_MEAL_TYPE,LU_MEAL,LU_INGREDIENTS,ALL_DATA)
        gc()


        #####OBSERVERS#####
        # Observe Feedback link click

        observeEvent(input$feedback, {
            showModal(
                customModalDialog(
                    h4('Please Provide Feedback or Report a Bug'),
                    p('To report a bug: Provide as much detail about what you were trying to do and what happened,
                      along with any error messages that were displayed.'),
                    p('Feedback: We welcome all feedback or requests for additional features. Provide as much detail as possible.'),
                    p('Your responses will be stored in the database and reviewed periodically by the administrator/developer. You will recieve
                       an email when your reported issue has been addressed.'),
                    selectInput(inputId = ns('feedback-type'), label = 'Issue Type',
                                choices = c('Feature Request','Bug Report'), selected = 'Feature Request'
                    ),
                    textInput(inputId = ns('feedback-title'), label = 'Issue Title'
                    ),
                    textAreaInput(inputId = ns('feedback-desc'), label = 'Issue Description'
                    ),
                    session = session,
                    title = h4('River Planner Feedback', style = 'color: #5cb874'),
                    size = 'l',
                    easyClose = FALSE,
                    fade = FALSE,
                    footer = fluidRow(class = 'modal-footer-row',
                                      actionButton(ns('submitFeedback'), label = 'Submit', class = 'btn btn-success', class = 'riv'),
                                      actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
                                                   class = 'riv', class = 'getstarted')
                    )
                )
            )
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

        # Observe feedback modal submit -----

        observeEvent(input$submitFeedback, {
            req(length(LOCAL$userID) > 0)
            userID <- LOCAL$userID
            userName <- LOCAL$userName
            type <- input$`feedback-type`
            title <- input$`feedback-title`
            desc <- input$`feedback-desc`

            con <- rivConnect()
            dbExecute(con, 'start transaction;')
            dbExecute(con,
                paste0("insert into feedback (",
                       "USER_ID, F_TYPE, F_TITLE, F_DESC, UPTIME, UPUSER) VALUES (",
                       userID,", '",
                       type,"', '",
                       title,"', '",
                       desc,"', ",
                       "now()",", '",
                       userName,"');"
                )
            )
            dbExecute(con, 'commit;')
            dbDisconnect(con)
            showNotification("Thanks for your Feedback! We will follow up with any questions.",
                type = 'message'
            )
            removeModal()
        })

        # Observe logout button -----

        observeEvent(input$logOut, {logout()})

        # Return LOCAL reactive values object

        return(LOCAL)

    })
}

