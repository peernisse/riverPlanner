#' menu UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @importFrom dplyr anti_join
#' @noRd
mod_menu_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3(uiOutput(ns('sectionTitleSelectMeals'))),

        div(id = ns("menuSelect"), class = "accordion",
          accInner(ns, parentId = "menuSelect", buttonId = 'breakfasts', buttonTitle = 'Breakfasts',
            collapseId = 'collapseBreakfasts', body = uiOutput(ns('breakfast')), bgColor = FALSE),
          accInner(ns, parentId = "menuSelect", buttonId = 'lunches', buttonTitle = 'Lunches',
            collapseId = 'collapseLunches', body = uiOutput(ns('lunch')), bgColor = FALSE),
          accInner(ns, parentId = "menuSelect", buttonId = 'dinners', buttonTitle = 'Dinners',
            collapseId = 'collapseDinners', body = uiOutput(ns('dinner')), bgColor = FALSE),
          accInner(ns, parentId = "menuSelect", buttonId = 'appetizers', buttonTitle = 'Appetizers',
            collapseId = 'collapseAppetizers', body = uiOutput(ns('appetizer')), bgColor = FALSE),
          accInner(ns, parentId = "menuSelect", buttonId = 'desserts', buttonTitle = 'Desserts',
            collapseId = 'collapseDesserts', body = uiOutput(ns('dessert')), bgColor = FALSE),
          accInner(ns, parentId = "menuSelect", buttonId = 'cocktails', buttonTitle = 'Cocktails',
            collapseId = 'collapseCocktails', body = uiOutput(ns('cocktails')), bgColor = FALSE)
        ),

        br(),
        actionButton(ns('createMeal'), label = 'Create a New Meal', class = 'btn btn-success',
                     style = 'margin:5px;')
      )
    ),
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3(uiOutput(ns('sectionTitleTripMenu'))),
        uiOutput(ns('tripMenu')),
        br(),
        actionButton(ns('menuExport'), label = 'View and Download',
          class = 'btn btn-success',
          #disabled = 'disabled',
          style = 'margin:5px;'
        ),
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;')
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

    #####VECTOR OF CREATED OBSERVERS FOR EDIT MEAL MODAL#####
    createdObservers <- c()

    #####REACTIVE BUTTON OBSERVERS#####

    # Delete button IDs
    new_del_button <- reactiveVal(0)
    delMealButtonIDs <- c()

    # Edit button IDs
    new_edit_button <- reactiveVal(0)
    editMealButtonIDs <- c()

    # Add meal button observers -----

    addButtonIDs <- reactive(LOCAL$ALL_DATA$MEAL_ADD_ID %>% unique(.))

    observe({

      #addButtonIDs <- reactive(LOCAL$ALL_DATA$MEAL_ADD_ID %>% unique(.))

      if(is.null(createdObservers)){
        addButtonIDs <- addButtonIDs()
      } else {
        addButtonIDs <- addButtonIDs()[!addButtonIDs() %in% createdObservers]
      }

      if(length(addButtonIDs) > 0){

            map(addButtonIDs, ~ observeEvent(input[[.x]],{

              # Validate user has selected a river day

                if(is.na(as.numeric(input[[paste0('rd-',gsub('add-','',.x))]]))){
                  showNotification('Select River Day!', type = 'error', duration = 10)
                  return(NULL)
                }

              # Validate user has created a trip

                if(length(LOCAL$tripName) == 0 & LOCAL$noAdults == 1){
                  showNotification('Please create a Trip (i.e., click "Save Trip") before adding meals.', type = 'error', duration = 10)
                  return(NULL)
                }


              # Gather user inputs, calculate mealId and mealUniqueId and make new record to append to myMeals

              riverDay <- input[[paste0('rd-',gsub('add-','',.x))]] %>% as.numeric()
              mealType <- input[[paste0('mt-',gsub('add-','',.x))]]
              mealId <- gsub('add-','',.x)
              mealUniqueId <- paste0(mealId,'_',mealType,'_',riverDay)

              newRecord <- LOCAL$ALL_DATA %>% filter(MEAL_ADD_ID == .x) %>%
                  mutate(RIVER_DAY = riverDay,
                         NO_ADULTS = LOCAL$noAdults,
                         NO_KIDS = LOCAL$noKids,
                         NO_PEOPLE_CALC = LOCAL$noPeopleCalc,
                         MEAL_TYPE = mealType,
                         MEAL_UNIQUE_ID = mealUniqueId,
                         INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID),
                         #QTY = ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR),
                         MEAL_NOTES = '',
                         TRIPNAME = LOCAL$tripName,
                         TRIP_DESC = LOCAL$tripDesc,
                         USERNAME = LOCAL$userName,
                         UPTIME = Sys.Date(),
                         UPUSER = LOCAL$userName,
                         TRIP_ID = LOCAL$tripID
                  ) %>%
                  mutate(
                      QTY = case_when(
                          NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                              round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                          TRUE ~ ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
                      )
                  )
            # last_edit
              # Validate that that meal on that day does not already exist
                if(nrow(LOCAL$myMeals) > 0 & paste0(gsub('add-','',.x),'_',mealType,'_',riverDay) %in% LOCAL$myMeals$MEAL_UNIQUE_ID){
                  showNotification('This Meal Exists on this Day Already!', type = 'error', duration = 5)
                  return(NULL)

                }

              # Append new record to myMeals

                LOCAL$myMeals <- LOCAL$myMeals %>% bind_rows(newRecord)

              # Notify meal added to myMenu

                showNotification(paste(unique(newRecord$MEAL_NAME),'added to trip menu below!'), type = 'default', duration = 5)

               # Reset input choices
                #TODO (maybe I will do this, maybe leave them)

              # Add a delete button listener for this menu card
                delMealButtonIDs <<- c(delMealButtonIDs,paste0('del-',mealUniqueId))

                # Increment the delete button counter
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
              })# end observeEvent
            ) # end map
      } # end if

      createdObservers <<- c(createdObservers,isolate(addButtonIDs)) %>% unique()

    }) # end observe
#--------------------------------------------------------

  # Create New Meal button observer -----

  observeEvent(input$createMeal, {
    # Launch modal module create meal -----
    showModal(
      mod_meal_create_ui('createMeal', session)
    )
  })

  # Create Menu Export button observer -----

  observeEvent(input$menuExport, {

    if(is.null(LOCAL$tripName) || LOCAL$tripName == '' || nrow(LOCAL$myMeals) == 0) {
      showNotification('Please create or load a trip and enter some data first...', type = 'error', duration = 5)
      return(NULL)
    }

    # Launch modal module create meal
    LOCAL$exportMenuModalSwitch <- TRUE
    showModal(
      mod_menu_export_ui('menuExport', session)
    )
  })

  # View meal button observer -----

  viewButtonIDs <- reactive(LOCAL$ALL_DATA$MEAL_VIEW_ID %>% unique(.))
  observe({
    map(viewButtonIDs(), ~ observeEvent(input[[.x]],{
      viewMeal(session, id = .x, data = LOCAL)
      createdObservers <<- c(createdObservers,.x) %>% unique()
    })
    )
  })

  # Menu card delete and edit button observers -----

  observe({
    req(nrow(LOCAL$myMeals) > 0)

    # Trip loading conditions
    if(LOCAL$loadTripMode == TRUE){
      # Clear edit and delete button listener lists
      # Delete button IDs
      new_del_button <<- reactiveVal(0)
      delMealButtonIDs <<- c()

      # Edit button IDs
      new_edit_button <<- reactiveVal(0)
      editMealButtonIDs <<- c()

      # Get meal Unique IDs from loaded trip DF
      mealUniqueIDs <- unique(LOCAL$myMeals$MEAL_UNIQUE_ID)

      # Make edit and delete button listeners
      for(i in 1:length(mealUniqueIDs)){
        mealUniqueId <- mealUniqueIDs[i]
        # Add a delete button listener
        delMealButtonIDs <<- c(delMealButtonIDs,paste0('del-',mealUniqueId))

        # Increment the delete button counter
        isolate({
          val <- new_del_button()
          val <- val + 1
          new_del_button(val)
        })

        # Delete button observers -----
        del_id <- delMealButtonIDs[new_del_button()]
        map(del_id, ~ observeEvent(input[[.x]],{delMealResponse(
            session = session, input = input, output = output,
            id = .x, data = LOCAL)},
          ignoreInit = TRUE, once = TRUE)
        )

        # Add an edit button listener for this menu card
        editMealButtonIDs <<- c(editMealButtonIDs,paste0('edit-',mealUniqueId))

        #Increment the edit button counter
        isolate({
          val <- new_edit_button()
          val <- val + 1
          new_edit_button(val)
        })

        # Edit button observers -----
        edit_id <- editMealButtonIDs[new_edit_button()]
        map(edit_id, ~
          observeEvent(input[[.x]],{
            LOCAL$editMealModalSwitch <- TRUE
            LOCAL$editMealMealUniqueID <- gsub('edit-','',.x)
            showModal(
              mod_meal_edit_ui('editMeal', session)
              #editMealModal(input, output, session, mealUniqueID = gsub('edit-','',.x), data = LOCAL)
            )
          }, ignoreInit = TRUE)
        )
      }

      LOCAL$loadTripMode <- FALSE

    } else {

      # Delete button observers -----
      del_id <- delMealButtonIDs[new_del_button()]
      map(del_id, ~ observeEvent(input[[.x]],{delMealResponse(
          session = session, input = input, output = output,
          id = .x, data = LOCAL)},
        ignoreInit = TRUE, once = TRUE)
      )

      # Edit button observers -----
      edit_id <- editMealButtonIDs[new_edit_button()]
      map(edit_id, ~
        observeEvent(input[[.x]],{
          LOCAL$editMealModalSwitch <- TRUE
          LOCAL$editMealMealUniqueID <- gsub('edit-','',.x)
          showModal(
            mod_meal_edit_ui('editMeal', session)
            #editMealModal(input, output, session, mealUniqueID = gsub('edit-','',.x), data = LOCAL)
          )
        }, ignoreInit = TRUE)
      )
    }
  })


  #####UI OUTPUTS#####
  # Section titles
  output$sectionTitleSelectMeals <- renderUI({
    if(length(LOCAL$tripName) > 0){
      paste('Select Meals for',LOCAL$tripName)
    } else{'Select Meals'}
  })

  output$sectionTitleTripMenu <- renderUI({
    if(length(LOCAL$tripName) > 0){
      paste('Customize Trip Menu for',LOCAL$tripName)
    } else{'Customize Trip Menu'}
  })

  # Make meal card rows by meal type

  output$breakfast <- makeMealCards(input, output, session, mtype = 'Breakfast', LOCAL)
  output$lunch <- makeMealCards(input, output, session, mtype = 'Lunch', LOCAL)
  output$dinner <- makeMealCards(input, output, session, mtype = 'Dinner', LOCAL)
  output$appetizer <- makeMealCards(input, output, session, mtype = 'Appetizer', LOCAL)
  output$dessert <- makeMealCards(input, output, session, mtype = 'Dessert', LOCAL)
  output$cocktails <- makeMealCards(input, output, session, mtype = 'Cocktail', LOCAL)

  # Make Menu card rows by day

  output$tripMenu <- renderUI({
    if(nrow(LOCAL$myMeals) == 0){
        return(p('< No Meals Yet > HINT: Create or Load a Trip and add Meals...'))
    }

    days <- unique(LOCAL$myMeals$RIVER_DAY) %>% sort(.)
    div(id = ns('rdSelect'), class = "accordion",
      map(days, ~ makeDayBoxes(input, output, session,
        rd = .x, parentId = 'rdSelect', data = LOCAL$myMeals)
      )
    )
  })

  #---------------------------
  #####RETURN LOCAL DATA OBJECT#####
  return(LOCAL)

  # End of menu module server -----

  })
}
