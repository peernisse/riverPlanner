#' menu UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import bsplus
#' @importFrom shiny NS tagList
#' @importFrom dplyr anti_join
#' @noRd
mod_menu_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;',
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3(uiOutput(ns('sectionTitleSelectMeals'))),

        div(id = ns("menuSelect"), class = "accordion",
          accInner(ns, parentId = "menuSelect", buttonId = 'breakfasts', buttonTitle = 'Breakfasts',
            collapseId = 'collapseBreakfasts', body = uiOutput(ns('breakfast'))),
          accInner(ns, parentId = "menuSelect", buttonId = 'lunches', buttonTitle = 'Lunches',
            collapseId = 'collapseLunches', body = uiOutput(ns('lunch'))),
          accInner(ns, parentId = "menuSelect", buttonId = 'dinners', buttonTitle = 'Dinners',
            collapseId = 'collapseDinners', body = uiOutput(ns('dinner'))),
          accInner(ns, parentId = "menuSelect", buttonId = 'appetizers', buttonTitle = 'Appetizers',
            collapseId = 'collapseAppetizers', body = uiOutput(ns('appetizer'))),
          accInner(ns, parentId = "menuSelect", buttonId = 'desserts', buttonTitle = 'Desserts',
            collapseId = 'collapseDesserts', body = uiOutput(ns('dessert'))),
          accInner(ns, parentId = "menuSelect", buttonId = 'cocktails', buttonTitle = 'Cocktails',
            collapseId = 'collapseCocktails', body = uiOutput(ns('cocktail')))
        ),

        br(),
        actionButton(ns('createMeal'), label = 'Create a New Meal', class = 'btn btn-success',
                     style = 'margin:5px;')
      ) #end column
    ),# end fluidRow

    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;',
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        h3(uiOutput(ns('sectionTitleTripMenu'))),
        uiOutput(ns('tripMenu')),
        br(),
        #TODO make this button disabled when no menu is loaded
        actionButton(ns('menuExport'), label = 'View and Download',
                      class = 'btn btn-success',
                      #disabled = 'disabled',
                      style = 'margin:5px;'
        ),
        hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;')
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
                  showNotification('Please create a Trip before adding meals.', type = 'error', duration = 10)
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
                         MEAL_NOTES = '',
                         TRIPNAME = LOCAL$tripName,
                         TRIP_DESC = LOCAL$tripDesc,
                         USERNAME = LOCAL$userName,
                         UPTIME = Sys.Date(),
                         UPUSER = LOCAL$userName,
                         TRIP_ID = LOCAL$tripID
                  )

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
                showModal(
                  editMealModal(input, output, session, mealUniqueID = gsub('edit-','',.x), data = LOCAL)
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
              showModal(
                editMealModal(input, output, session, mealUniqueID = gsub('edit-','',.x), data = LOCAL)
              )
            }, ignoreInit = TRUE)
      )
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

#---------------------------------------------------------------

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

  # Observe new ingredient button -----
#This can eventually go away it was moved to a function
#   observeEvent(input[['newIngredient']], {
# #TODO this needs to become a function it is used in edit meal and create meal processes
#     req(LOCAL$editMealModalSwitch == TRUE)
#     req(nrow(LOCAL$editMealDF) > 0)
#
#     # Get current NO_PEOPLE_CALC
#     #noPeopleCalc <- LOCAL$editMealDF$NO_PEOPLE_CALC[1]
#
#     if(input[['ing-new-ing']] == '') {
#       showNotification('Ingredient name cannot be blank!', type = 'error', duration = 10)
#       return(NULL)
#     }
#
#     if(input[['ing-new-cat']] == 'Select category' | input[['ing-new-cat']] == '') {
#       showNotification('Ingredient category cannot be blank!',
#                        type = 'error', duration = 10)
#       return(NULL)
#     }
#
#     if(input[['ing-new-desc']] == '') {
#       showNotification('Ingredient description cannot be blank!',
#         type = 'error', duration = 10)
#       return(NULL)
#     }
#
#     if(input[['ing-new-unit']] == '') {
#       showNotification('Ingredient units cannot be blank!',
#         type = 'error', duration = 10)
#       return(NULL)
#     }
#
#     if(input[['ing-new-ssf']] == '') {
#       showNotification('Ingredient multiplier cannot be blank!
#         Enter a quantity for this number of people to produce a multiplier.',
#         type = 'error', duration = 10)
#       return(NULL)
#     }
#
#
#     ingName <- input[['ing-new-ing']]
#     ingCat <- input[['ing-new-cat']]
#     ingDesc <- input[['ing-new-desc']]
#     ingUnit <- input[['ing-new-unit']]
#     ingSSF <- as.numeric(input[['ing-new-ssf']])
#     ingStorage <- input[['ing-new-storage']]
#     ingQty <- input[['ing-new-qty']]
#
#
#     newRecord <- data.frame(
#       INGREDIENT_ID = max(LOCAL$LU_INGREDIENTS$INGREDIENT_ID) + 1,
#       INGREDIENT_CATEGORY = ingCat,
#       INGREDIENT = ingName,
#       INGREDIENT_DESCRIPTION = ingDesc,
#       SERVING_SIZE_DESCRIPTION = ingUnit,
#         SERVING_SIZE_FACTOR = ingSSF,
#         STORAGE_DESCRIPTION = ingStorage,
#       UPTIME = Sys.Date(),
#       UPUSER = LOCAL$userName
#     )
#
#     # Append to LU_INGREDIENTS
#
#     LOCAL$LU_INGREDIENTS <- bind_rows(LOCAL$LU_INGREDIENTS, newRecord) %>%
#       arrange(INGREDIENT)
#
#     # Notify ingredient added
#
#     showNotification(
#       paste(ingName, "was added to the 'Add Ingredient' dropdown list above.
#             You can use the dropdown to add",ingName,"to this meal."),
#                      type = 'message', duration = 10)
#
#   })

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
    LOCAL$editMealDF$MEAL_NOTES <- input[[paste0('notes-',mealUniqueID)]]

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

  #####UI OUTPUTS#####
  # Section titles
  output$sectionTitleSelectMeals <- renderUI({
    if(length(LOCAL$tripName) > 0){
      paste('Select Meals for',LOCAL$tripName)
    } else{'Select Meals'}
  })

  output$sectionTitleTripMenu <- renderUI({
    if(length(LOCAL$tripName) > 0){
      paste('Manage Trip Menu for',LOCAL$tripName)
    } else{'Manage Trip Menu'}
  })

  # Make meal card rows by meal type

  output$breakfast <- makeMealCards(input, output, session, mtype = 'Breakfast', LOCAL)
  output$lunch <- makeMealCards(input, output, session, mtype = 'Lunch', LOCAL)
  output$dinner <- makeMealCards(input, output, session, mtype = 'Dinner', LOCAL)
  output$appetizer <- makeMealCards(input, output, session, mtype = 'Appetizer', LOCAL)
  output$dessert <- makeMealCards(input, output, session, mtype = 'Dessert', LOCAL)
  output$cocktails <- renderUI({
    p('No Cocktails yet....')
  })


  # Make Menu card rows by day
  #TODO make this open the div for the menu day just updated and change the name from test

  output$tripMenu <- renderUI({
    if(nrow(LOCAL$myMeals) == 0){return(NULL)}

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
