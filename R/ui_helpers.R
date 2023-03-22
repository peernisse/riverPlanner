#####CARDS TEMPLATES#####

#' Meal card UI function
#' @description View and adjust Meal record info in a card.
#' @param session The shiny session
#' @param id The id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#' @noRd
mealCard <- function(session,id,data = LOCAL, mtype, ttl, subttl, desc){
    ns <- session$ns

    if(file.exists(paste0('./inst/app/www/assets/img/menu/',mtype,'-',id,'.jpg')) == TRUE){
        imgsrc <- paste0('www/assets/img/menu/',mtype,'-',id,'.jpg')
    } else {
        imgsrc <- 'www/assets/img/menu/default.jpg'
    }

    div(id = ns(id), class = "card card-block mx-2", style="min-width:300px; margin-bottom:10px;",
        tags$img(class="card-img-top", alt="100%x280" , src = imgsrc,
                 height = '300', width = '300'
        ),
        div(class = "card-body",style="min-width:300px;",
            h5(class = "card-title d-flex justify-content-between align-items-left", ttl,),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text',style = 'text-align: left;', desc),
            div(style = 'display: inline-flex; justify-content: flex-end; flex-wrap: nowrap; flex-direction: row;',
                selectInput(inputId = ns(paste0('rd-',id)),
                    width = 'fit-content',
                    size = 1,
                    selectize = FALSE,
                    label = NULL,
                    choices = c('Day',as.character(seq(1:30))),
                    selected = 'Day'),
                selectInput(inputId = ns(paste0('mt-',id)),
                    width = 'fit-content',
                    size = 1,
                    selectize = FALSE,
                    label = NULL,
                    choices = c(unique(data$LU_MEAL_TYPE$MEAL_TYPE)),
                    selected = mtype),
                actionButton(inputId = ns(paste0('view-',id)),label = NULL, icon = icon('eye'),
                    type = "button",class = "btn btn-md btn-primary",
                    style = 'margin-left: 3px;'
                ),
                actionButton(inputId = ns(paste0('add-',id)),label = NULL, icon = icon('plus'),
                    type = "button",class = "btn btn-md btn-success",
                    style = 'margin-left: 3px;'),
                # actionButton(inputId = ns(paste0('del-',id)),label = NULL, icon = icon('trash'),
                #              type = "button",class = "btn btn-sm btn-danger")
            ),
        )
    )
}

#' Menu card UI function
#' @description View and adjust Meal record info in a card.
#' @param session The shiny session
#' @param id The id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#' @noRd
menuCard <- function(session,id, data, mtype, ttl, subttl, desc){
    ns <- session$ns
    mealUniqueId <- data$MEAL_UNIQUE_ID %>% unique(.)

    if(file.exists(paste0('./inst/app/www/assets/img/menu/',mtype,'-',id,'.jpg')) == TRUE){
        imgsrc <- paste0('www/assets/img/menu/',mtype,'-',id,'.jpg')
    } else {
        imgsrc <- 'www/assets/img/menu/default.jpg'
    }

    div(id = ns(paste0('myMeals-',id)), class = "card card-block mx-2", style="min-width:300px; margin-bottom:10px;",
        tags$img(class="card-img-top", alt="100%x280" , src = imgsrc,
                 height = '300', width = '300'
        ),
        div(class = "card-body",style="max-width:350px;",
            h5(class = "card-title d-flex justify-content-between align-items-left", paste(mtype,'|',ttl),),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text',style = 'text-align: left;', desc),
            div(style = 'display: inline-flex; justify-content: flex-end; flex-wrap: nowrap; flex-direction: row;',

                actionButton(inputId = ns(paste0('edit-',mealUniqueId)),label = NULL, icon = icon('pencil'),
                             type = "button",class = "btn btn-md btn-primary",
                             style = 'margin-left: 3px;'
                ),
                actionButton(inputId = ns(paste0('del-',mealUniqueId)),label = NULL, icon = icon('trash'),
                             type = "button",class = "btn btn-md btn-danger",
                             style = 'margin-left: 30px;'),
            ),
        )
    )
}

#' Trip card
#' @description The UI card displays saved trip info
#' @param session The Shiny session object
#' @param tripID The Unique saved trip ID
#' @param tripName The saved trip name
#' @param days The max number of days in the saved trip
#' @param noAdults The max number of adults in the saved trip
#' @param noKids The max number of kids in the saved trip
#' @param tripDesc The saved trip description
#' @param upTime The last modified date of the saved trip
#' @noRd
tripCard <- function(session, data, tripID, tripName, days, noAdults, noKids, tripDesc, upTime){
  ns <- session$ns
  LOCAL <- data
  loadButtonID <- paste0('load-tripID-',tripID)
  killButtonID <- paste0('kill-tripID-', tripID)
  copyButtonID <- paste0('copy-tripID-', tripID)

    if(length(LOCAL$tripID) == 0){
        style <- "text-align: left; min-width:300px;
          margin-bottom:10px; border-left-color: #232b2b; border-left-width: .25rem;
          border-radius: .25rem;"
    }

    if(length(LOCAL$tripID) > 0 && !LOCAL$tripID %in% tripID){
        style <- "text-align: left; min-width:300px;
          margin-bottom:10px; border-left-color: #232b2b; border-left-width: .25rem;
          border-radius: .25rem;"
    }

    if(length(LOCAL$tripID) > 0 && LOCAL$tripID == tripID){
        style <- "text-align: left; min-width:300px;
          margin-bottom:10px; border-left-color: #5cb874; border-left-width: .25rem;
          border-radius: .25rem;"
    }

  div(class = "card card-block mx-2", style = style,
    div(class = "card-body", style="max-width:350px;",
      h5(class = "card-title", tripName),
      h6(class = "card-subtitle mb-2 text-muted",
         paste0(days, ' days | ', noAdults, ' Adults | ', noKids, ' Kids')
      ),
      p(class = "card-text", tripDesc),
      tags$button(id = ns(killButtonID),
                  class = "btn btn-danger action-button shiny-bound-input",
                  type = "button", icon('trash')
      ),
      tags$button(id = ns(loadButtonID), class = "btn action-button shiny-bound-input",
                  style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;',
                  type = "button",
                  'Load Trip'
      ),
      tags$button(id = ns(copyButtonID), class = "btn action-button btn-primary shiny-bound-input",
                  #style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;',
                  type = "button",
                  'Copy Trip'
      )
    )
  )
}

#####CARDS GENERATORS#####

#' Trip card generator
#' @param input,output,session The Shiny app objects for the session
#' @param data Passing in the 'LOCAL' reactiveValues data object
#' @noRd
makeTripCards <- function(input, output, session, data = LOCAL){
  LOCAL <- data
  ns <- session$ns

  renderUI({
    if(length(LOCAL$userName) == 0 | is.null(LOCAL$userName) | LOCAL$userName == ''){return(NULL)}
    if(nrow(LOCAL$LU_TRIPS) == 0){return(NULL)}

    tripMap1 <- LOCAL$LU_TRIPS %>%
        filter(USER_ID %in% LOCAL$userID) %>%
        select(TRIP_ID, TRIP_DESC, TRIPNAME) %>%
        distinct(.)

    tripMap <- LOCAL$LU_TRIPS %>%
      filter(USER_ID %in% LOCAL$userID) %>%
        group_by(TRIP_ID) %>%
        summarize(
            RIVER_DAY = max(as.numeric(RIVER_DAY)),
            NO_ADULTS = max(as.numeric(NO_ADULTS)),
            NO_KIDS = max(as.numeric(NO_KIDS)),
            #TRIP_DESC = TRIP_DESC,
            UPTIME = max(UPTIME)
        ) %>%
        unique() %>%
        arrange(desc(UPTIME)) %>%
        left_join(tripMap1, by = c('TRIP_ID'))

    div(class = "container-fluid py-2", style = 'padding-left: inherit; padding-right: inherit;',
      div(class = 'row', style = 'text-align: center; margin-bottom: 5px;',
          h6(paste0('<- Explore My Trips',' (',nrow(tripMap),') Items ->'))
      ),

      div(class = "d-flex flex-row flex-nowrap overflow-auto",
        map(tripMap$TRIP_ID, ~ tripCard(session = session,
          data = LOCAL,
          tripID = .x,
          tripName = tripMap[which(tripMap$TRIP_ID == .x), 'TRIPNAME'],
          days = tripMap[which(tripMap$TRIP_ID == .x), 'RIVER_DAY'],
          noAdults = tripMap[which(tripMap$TRIP_ID == .x), 'NO_ADULTS'],
          noKids = tripMap[which(tripMap$TRIP_ID == .x), 'NO_KIDS'],
          tripDesc = tripMap[which(tripMap$TRIP_ID == .x), 'TRIP_DESC'],
          upTime = tripMap[which(tripMap$TRIP_ID == .x), 'UPTIME'])
        )
      )
    )
  })
}

#' Meal card generator function
#' @description The functional programming logic to generate cards, input objects,
#' and related observers.
#' @param session The shiny session
#' @param data Passing in the 'LOCAL' reactiveValues data object
#' @param mtype One of the meal types in LOCAL$LU_MEAL$MEAL_TYPE. Used to break
#' the cards out into accordion collapsible sections on the page.
#' @noRd
makeMealCards <- function(input, output, session, mtype, data = LOCAL){
    LOCAL <- data

    renderUI({
        ns <- session$ns
        filtDat <- LOCAL$ALL_DATA %>% filter(MEAL_TYPE %in% mtype) %>%
            select(MEAL_ID,MEAL_TYPE,MEAL_NAME,MEAL_DESCRIPTION) %>% unique(.)
        mapIndexRows <- which(filtDat$MEAL_TYPE == mtype)

        # Make cards

        div(class = "container-fluid py-2", style = 'padding-left: inherit; padding-right: inherit;',
            div(class = 'row', style = 'text-align: center; margin-bottom: 5px;',
              h6(paste0('<- Explore ',mtype,' (',length(mapIndexRows),') Items ->'))
            ),

            div(class = "d-flex flex-row flex-nowrap overflow-auto",

                map(mapIndexRows, ~ mealCard(session,filtDat[.,'MEAL_ID'],data = LOCAL,
                     mtype = mtype, filtDat[.,'MEAL_NAME'],
                     NULL, filtDat[.,'MEAL_DESCRIPTION']))
            )

        )

    })
}

#' Menu card generator function
#' @description The functional programming logic to generate cards, input objects,
#' and related observers. Meant to be used in a loop for makeDayBoxes
#' @param input,output,session The shiny session objects
#' @param data Passing in the 'LOCAL' reactiveValues data object
#' @param day The river day.
#'
#' @noRd
makeMenuCards <- function(input, output, session, day, data){
    # The data should be the LOCAL$myMeals DF
    ns <- session$ns

    renderUI({
        ns <- session$ns

        filtDat <- data %>% filter(RIVER_DAY %in% day) %>%
            select(RIVER_DAY, MEAL_ID, MEAL_UNIQUE_ID, MEAL_TYPE,MEAL_NAME,MEAL_DESCRIPTION) %>% unique(.)

        mapIndexRows <- which(filtDat$RIVER_DAY == day)

        # Make cards
        div(class = "container-fluid py-2",
            div(class = 'row', style = 'text-align: center; margin-bottom: 5px;',
                h6(paste0('<- Explore Day ',day,' (',length(mapIndexRows),') Items ->'))
            ),
            div(class = "d-flex flex-row flex-nowrap overflow-auto",

                map(mapIndexRows, ~ menuCard(session,filtDat[.,'MEAL_ID'],data = filtDat[.,],
                                             mtype = filtDat[.,'MEAL_TYPE'], filtDat[.,'MEAL_NAME'],
                                             NULL, filtDat[.,'MEAL_DESCRIPTION']))
            )

        )

    })
}

#' makeDayBoxes
#' @description Creates a collapsible horizontal row of menu cards. Uses the BS5 accordion CSS
#' @param input,output,session The shiny session objects
#' @param rd The river day to focus on. Must be a single river day numeric
#' @param parentId The div ID of the parent accordion div
#' @param data The reactive myMeals dataframe. The growing DF of selected meals to be displayed as cards.
#'
#' @noRd
makeDayBoxes <- function(input, output, session, rd, parentId, data = LOCAL$myMeals){
  req(!is.null(data) == TRUE & (nrow(data) > 0) == TRUE)
  ns <- session$ns

  data <- data %>%
    filter(RIVER_DAY == rd) %>%
    mutate(
      MEAL_TYPE = factor(MEAL_TYPE,
                         levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail'))
    ) %>%
    arrange(RIVER_DAY,MEAL_TYPE)

  buttonId <- paste0('riverDay-',rd)
  collapseId <- paste0('collapse-',buttonId)
  buttonTitle <- paste0('River Day ',rd)

  accInner(ns, parentId, buttonId, buttonTitle, collapseId,
    body = makeMenuCards(input, output, session, day = rd, data = data)
  )
}

#####MODAL DIALOGS#####

#' This is just shiny modalDialog with one extra line to add a button in
#' @description The shiny modalDialog function with added upper right close button for bootstrap 5
#' @param ... UI elements for the body of the modal dialog box.
#' @param title An optional title for the dialog.
#' @param footer UI for footer. Use NULL for no footer.
#' @param session This is added to carry namespace to the UI.
#' @param size One of "s" for small, "m" (the default) for medium, or "l" for large.
#' @param easyClose If TRUE, the modal dialog can be dismissed by clicking outside the dialog box,
#' or be pressing the Escape key. If FALSE (the default), the modal dialog can't be dismissed in those ways;
#' instead it must be dismissed by clicking on a modalButton(), or from a call to removeModal() on the server.
#' @param fade If FALSE, the modal dialog will have no fade-in animation (it will simply appear rather than fade in to view).
#' @param label The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon An optional icon() to appear on the button.
#' @noRd

customModalDialog <- function (..., session, title = NULL, footer = modalButton("Dismiss"),
                               size = c("m", "s", "l", "xl", "fs"), easyClose = FALSE, fade = TRUE) {
  ns <- session$ns

  size <- match.arg(size)
  backdrop <- if (!easyClose) "static"
  keyboard <- if (!easyClose) "false"

  div(id = "shiny-modal", class = "modal", class = if (fade)
    "fade", class = 'show', tabindex = "-1", `data-backdrop` = backdrop,
    `data-bs-backdrop` = backdrop, `data-keyboard` = keyboard,
    `data-bs-keyboard` = keyboard, `aria-labelledby` = "#headerTitle",
    div(class = "modal-dialog modal-dialog-scrollable",
          class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg",
                        xl = "modal-xl", fs = "modal-fullscreen"),
        div(class = "modal-content",
            if (!is.null(title))
              div(class = "modal-header",
                tags$h5(id = ns('headerTitle'), class = "modal-title", title),
                tags$button(type="button", id = ns('editMealModalClose_1'), label = NULL,
                  class="btn-close shiny-bound-input", `data-dismiss`="modal",
                  `data-bs-dismiss`="modal", `aria-label`="Close")
              ),
              div(class = "modal-body", ...),
            if (!is.null(footer))
              div(class = "modal-footer", footer))), tags$script(HTML("if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {\n         var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));\n         modal.show();\n      } else {\n         $('#shiny-modal').modal().focus();\n      }")))
}

#' Trip info inputs for meal edit modal
#' @description Input fields for edit meal modal
#' @param input,output,session Shiny objects
#' @param data The LOCAL data rv object
#' @noRd
editMealTripInfoInputs <- function(input, output, session, data){
  ns <- session$ns
  LOCAL <- data

  mealUniqueID <- LOCAL$editMealMealUniqueID

  ingredientUniqueIDs <- unique(LOCAL$editMealDF$INGREDIENT_UNIQUE_ID)
  noAdultsID <- paste0('editMeal-noAdults-',mealUniqueID)
  noKidsID <- paste0('editMeal-noKids-',mealUniqueID)
  noPeopleCalcID <- paste0('editMeal-noPeopleCalc-',mealUniqueID)

  fluidRow(style = 'margin-top:20px;',
    column(width = 12,
      h5('Meal Trip Info'),
      customTextInput(inputId = ns('editMeal-tripName'), label = 'Trip Name',
        labelColor = '#162118', value = LOCAL$tripName, disabled = 'disabled'),
      customSelectInput(inputId = ns(noAdultsID), label = 'Adults 12+', labelColor = '#5cb874',
        choices = c('No. People Age 12+', '0', seq(1:30)), disabled = NULL,
        selected = unique(LOCAL$editMealDF$NO_ADULTS)
      ),
      customSelectInput(inputId = ns(noKidsID), label = 'Kids <12', labelColor = '#5cb874',
        choices = c('No. People Age <12', '0', seq(1:30)), disabled = NULL,
        selected = unique(LOCAL$editMealDF$NO_KIDS)
      ),
      customTextInput(inputId = ns(noPeopleCalcID), label = 'Total People to Calc. (Kids as 2/3)',
        labelColor = '#162118', value = unique(LOCAL$editMealDF$NO_PEOPLE_CALC),
        disabled = 'disabled')
    )
  )
}

#' Ingredient info inputs for meal edit modal
#' @description Makes pre-filled input fields which serve to edit the data
#' @param input,output,session The shiny app session objects
#' @param data The meal dataframe being viewed/edited. Usually a subset of myMeals
#' @noRd
editMealIngredientInputs <- function(input, output, session,data, displayQty = TRUE){

    #TODO this runs multiple times once for every ingredient. It runs multiple
    # times because if there are any SSF to QTY discrepancies, they rebalance, triggering
    # the UI again. And because of update text input in the new ingresient form
  ns <- session$ns
  ingUniqueID <- unique(data$INGREDIENT_UNIQUE_ID)
  ing <- unique(data$INGREDIENT)
  desc <- unique(data$INGREDIENT_DESCRIPTION)
  unit <- unique(data$SERVING_SIZE_DESCRIPTION)
  ssf <- unique(data$SERVING_SIZE_FACTOR)

  qty <- ifelse(displayQty == FALSE, '', unique(data$QTY))
  #I THINK this is fixed for now 3/16/2023

  #TODO somewhere the LOCAL$editMealDF QTY gets calculated with a ceiling, or < .5 numbers round to zero
  #and it is triggered twice the first time viewing the meal. This corrects it for now...

  # qty <- ifelse(displayQty == FALSE, '', round(data$NO_PEOPLE_CALC*data$SERVING_SIZE_FACTOR))
  #
  # if(qty == 0){
  #   qty <- ifelse(displayQty == FALSE, '', plyr::round_any(data$NO_PEOPLE_CALC*data$SERVING_SIZE_FACTOR,.5,ceiling))
  # }


  tagList(
    div(class = "input-group", style ='margin-top:12px;',
      tags$span(class = "input-group-text", ing,
           style = 'background-color: #232b2b; border-color: #232b2b; color: #fff;'),
      tags$input(id = ns(paste0('ing-ing-',ingUniqueID)), value = ing, type = "text",
                 disabled = 'disabled', `aria-label` = "Ingredient", class = "form-control"),
      tags$button(id = ns(paste0('del-ing-',ingUniqueID)), class = "btn btn-danger action-button shiny-bound-input", type = "button", icon('trash'))

    ),
    div(class = "input-group",
        tags$span(class = "input-group-text", "Description"),
        tags$input(id = ns(paste0('ing-desc-',ingUniqueID)), value = desc, type = "text",
                   disabled = 'disabled', `aria-label` = "Description", class = "form-control")
    ),
    div(class = "input-group",
        tags$span(class = "input-group-text", 'Units'),
        tags$input(id = ns(paste0('ing-unit-',ingUniqueID)), value = unit, type = "text",
                   disabled = 'disabled', `aria-label` = "Units", class = "form-control")
    ),
    div(class = "input-group",
        tags$span(class = "input-group-text", 'Multiplier'),
        tags$input(id = ns(paste0('ing-ssf-',ingUniqueID)), value = ssf, type = "text",
                   disabled = 'disabled', `aria-label` = "Multiplier", class = "form-control")
    ),
    div(class = "input-group", style = if(displayQty == FALSE){"display: none;"},
        tags$span(class = "input-group-text", 'Quantity',
             style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;'),
        tags$input(id = ns(paste0('ing-qty-',ingUniqueID)), value = qty, type = "text",
                   `aria-label` = "Quantity", class = "form-control")
    )
  )
}

#' Ingredient picker in edit meal modal or wherever else
#' @description creates searchable datalist select input of the ingredients in LU_INGREDIENTS
#' @param input,output,session The shiny app session objects.
#' @param data The LOCAL$LU_INGREDIENTS dataframe
#' @noRd
selectIngredients <- function(input, output, session, data){
  ns <- session$ns

  ings <- data %>% select(INGREDIENT_ID,INGREDIENT) %>% arrange(INGREDIENT) %>% pull(INGREDIENT)
  choices <- c('Start typing to search...',ings)

  div(
    div(class = "input-group mb-3",
      tags$label(class = "input-group-text", class = 'create-meal',
        `for` = ns('selectIngredient'),
        #style = "background-color: #ed7000; border-color: #ed7000; color: #fff;",
        'Add Ingredient'),
      tags$select(class = "form-select", id = ns('selectIngredient'),
        tags$option(selected = "selected", choices[1]),
          map(
            choices[-1], ~ tags$option(value = .x,.x)
          )
      ),
      tags$button(id = ns('addIngredient'), class = "btn action-button shiny-bound-input",
        type = "button", icon('plus'),
        style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;'
      )
    )
  )
}

#' Create new ingredient data entry fields
#' @description Input fields UI to create a new ingredient from within editMeal modal
#' @param input,output,session The shiny app session objects.
#' @param data The LOCAL reactive values data object..
#' @noRd
createIngredients <- function(input, output, session, data){
  ns <- session$ns
  LOCAL <- data
  noPeopleCalc <- as.numeric(LOCAL$editMealDF$NO_PEOPLE_CALC[1])
  cats <- c('Select category',LOCAL$LU_INGREDIENTS$INGREDIENT_CATEGORY %>% unique(.) %>% sort(.))

    tagList(

      div(class = "input-group", class = 'create-meal', style ='margin-top:12px;',
          tags$span(class = "input-group-text", 'New Ingredient',
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;'),
          tags$input(id = ns('ing-new-ing'),
                     placeholder = 'Ingredient Name', type = "text",
                     `aria-label` = "Ingredient", class = "form-control"),
          tags$button(id = ns('newIngredient'), class = "btn action-button shiny-bound-input",
                      type = "button", icon('plus'),
                      style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;'
          )
      ),
      div(class = "input-group", class = 'create-meal',
          tags$label(class = "input-group-text",
                     `for` = ns('ing-new-cat'),
                     style = "background-color: #ed7000; border-color: #ed7000; color: #fff;",'Category'
          ),
          tags$select(class = "form-select", id = ns('ing-new-cat'),
                      tags$option(selected = "selected", cats[1]),
                      map(
                        cats[-1], ~ tags$option(value = .x,.x)
                      )
          )
      ),
      div(class = "input-group", class = 'create-meal',
          tags$span(class = "input-group-text",
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;',
                    'Description'),
          tags$input(id = ns('ing-new-desc'),
                     placeholder = 'Ingredient Description', type = "text",
                     `aria-label` = "Description", class = "form-control")
      ),
      div(class = "input-group", class = 'create-meal',
          tags$span(class = "input-group-text",
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;',
                    'Units'),
          tags$input(id = ns('ing-new-unit'),
                     placeholder = 'Units of measure (e.g., 12 0z can, 1 apple, etc.)',
                     type = "text", `aria-label` = "Units", class = "form-control")
      ),
      div(class = "input-group", class = 'create-meal',
          tags$label(class = "input-group-text",
                     `for` = ns('ing-new-storage'),
                     style = "background-color: #ed7000; border-color: #ed7000; color: #fff;",
                     'Storage'
          ),
          tags$select(class = "form-select", id = ns('ing-new-storage'),
            tags$option(selected = "selected", 'Dry Storage'),
            tags$option(value = 'Cooler Storage', 'Cooler Storage')
          )
      ),

      div(class = "input-group mb-3", class = 'create-meal',
          tags$span(class = "input-group-text", style = 'border-color: #5cb874;
                    border-width: 4px; border-style: solid; border-radius: .3rem;',
                    'Multiplier'),
          tags$input(id = ns('ing-new-ssf'),
                     placeholder = 'Use calculator below...', type = "text",
                     `aria-label` = "Multiplier", class = "form-control", disabled = 'disabled'
          )
      ),

      p('Multiplier Calculator:', style = 'font-weight: bold;'),
      p("Tip: Enter '1' for 'Quantity' below, and adjust 'Serves'. If your ingredient
        'Units' is a packaged item (e.g., 12 oz can), look up its 'Servings per Container'
        online and put this in 'Serves'."),

      div(class = "input-group mt-1", style = 'border-color: #5cb874;
            border-width: 4px; border-style: solid; border-radius: .3rem;',
          tags$span(class = "input-group-text",
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;',
                    'Quantity'),
          tags$input(id = ns('ing-new-qty'),
                     placeholder = 'Qty. of Ing.', type = "text",
                     `aria-label` = "Quantity", class = "form-control"),
          tags$span(class = "input-group-text",
                    style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;',
                    'Serves'
          ),
          tags$input(id = ns('ing-new-hypPeople'),
                     placeholder = 'Qty. of Ing.', type = "text",
                     `aria-label` = "Quantity", class = "form-control",
                    value = noPeopleCalc
          )
      )
    ) # end tagList
}

#####INPUT BOX GROUPS#####

#' Trip info input group
#' @description BS5 Select input with label as left side addon. Selected is first in
#' list by default.
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param labelColor Background color for label
#' @param labelTextColor Label text color
#' @param choices List of values to select from. See selectInput
#' @param selected The initially selected value(s). See selectInput
#' @param disabled Whether to disable the input field and allow no user input
#' @noRd
customSelectInput <- function(inputId, label, labelColor, labelTextColor = '#fff',
                              choices, selected = choices[1], disabled = NULL){

  if(selected == choices[1]) {choices <- choices[!choices %in% selected]} else {choices = choices}

    div(class = "input-group mb-3",
      tags$label(class = "input-group-text",
        `for` = inputId,
        style = paste("background-color: ",labelColor,"; border-color: ",
        labelColor,"; color: ",labelTextColor,";"), label),
      tags$select(class = "form-select", id = inputId, disabled = disabled,
        tags$option(selected = "selected", selected),
      map(
          choices, ~ tags$option(value = .x,.x))
      )
    )
}

#' Custom text input
#' @description BS5 Text input with label as left side addon.
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value An initial value for the input field
#' @param labelColor Background color for label
#' @param labelTextColor Label text color
#' @param placeholder A character string giving the user a hint as to what can be entered into the control.
#' Internet Explorer 8 and 9 do not support this option.
#' @param disabled Whether to disable the input field and allow no user input
#' @noRd
customTextInput <- function(inputId, label, value = "", labelColor, labelTextColor = '#fff',
                            placeholder = label, disabled = NULL){
  div(class = "input-group mb-3",
    span(class = "input-group-text", id = paste0('lab-',inputId),
      style = paste("background-color: ",labelColor,"; border-color: ",
      labelColor,"; color: ",labelTextColor,";"), label
    ),
    tags$input(id = inputId, type = "text", disabled = disabled, class = "form-control",
      value = value, placeholder = placeholder, `aria-label` = placeholder,
      `aria-describedby` = paste0('lab-',inputId)
    )
  )
}

#' Custom text Area input
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param labelColor Background color for label
#' @param labelTextColor Label text color
#' @param disabled Whether to disable the input field and allow no user input
#' @noRd
customTextAreaInput <- function(inputId, label, value = '', labelColor,
      width = NULL, height = NULL, cols = NULL, rows = NULL, placeholder = NULL,
      resize = NULL, labelTextColor = '#fff', disabled = NULL){

  value <- restoreInput(id = inputId, default = value)

  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }

  style <- css(width = if (!is.null(width))
    "width: 100%;", height = validateCssUnit(height),
    resize = resize)

  div(class = "input-group mb-3 shiny-input-container create-meal",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

      span(class = "input-group-text",
           style = paste("background-color: ",labelColor,"; border-color: ",
                         labelColor,"; color: ",labelTextColor,";"),
           label),
      tags$textarea(id = inputId, class = "form-control", disabled = disabled,
                    `aria-label` = label, placeholder = placeholder,
                    style = style, rows = rows, cols = cols, value)
  )
}



#' Accordion item function to be mapped inside accordion container
#' @param ns The namespace of the module the function is being used in.
#' @param parentId The id of the parent accordion div the function is called in.
#' @param buttonId The id for the accordion segment button. What will have observeEvent on it.
#' @param buttonTitle The label for the accordion segment button.
#' @param collapseId The id for the collapsible div.
#' @param show Logical. Should the collapsable div begin open or closed.
#' @param body The UI to be displayed in the collapsable accordion segment.
#' @param bgColor Logical. Whether to color the background the same as the page section.
#' the accordion is in, if the section background is other than #fff.
#' @param pad Logical. Default TRUE adds left and right padding to accordion segment.
#' FALSE sets the padding to 0, the segment content reaches edge of page.
#' @param body UI elements. Either uiOutput object or HTML or function that returns HTML.
#' @noRd
accInner <- function(ns, parentId, buttonId, buttonTitle, collapseId,
                     show = FALSE, body, bgColor = FALSE, pad = TRUE){
  ns <- ns
  classExtra <- character()
  styleExtra <- character()
  show <- ifelse(show == FALSE,'','show')

  if(bgColor == TRUE){classExtra <- 'section-bg'}
  if(pad == FALSE){styleExtra <- 'padding: 0px;'}

  div(class = "accordion-item", style = 'border: none;',
      h4(id = ns(buttonId), class = 'accordion-header', class = classExtra,
         style = "display: flex; justify-content: center; font-size: 16px;",
           tags$button(
             class = "accordion-button collapsed",
             class = "btn btn-default menu getstarted", style = 'width: 300px; height: 40px;',
             type = 'button', `data-toggle` = "collapse",
             `data-target` = paste0("#",ns(collapseId)),
             `aria-expanded` = "true",`aria-controls` = paste0("#",ns(collapseId)),
             buttonTitle
           )
      ),
      div(id = ns(collapseId), class = "accordion-collapse collapse", class = show,
          `aria-labelledby` = ns(buttonId), `data-parent` = paste0('#',ns(parentId)),
          div(class = "accordion-body", class = classExtra, style = styleExtra,
              body
          )
      )
  )
}

# Export menu items -----

#' shopList
#' @description Makes the grouped ingredient shopping list for the trip
#' @param data The LOCAL reactive values data object, specifically the myMeals dataframe
#' @param forOutput Logical. TRUE for dataframe export. FALSE for dataframe to be viewed in app.
#' @noRd
shopList <- function(data, forOutput = FALSE){
  LOCAL <- data
  req(nrow(LOCAL$myMeals) > 0)
  output <- LOCAL$myMeals %>%
    group_by(INGREDIENT, SERVING_SIZE_DESCRIPTION) %>%
    summarize(
      TOTAL = sum(QTY, na.rm = TRUE) %>% as.character(),
      MEAL_COUNT = length(INGREDIENT)
    ) %>%
    select(INGREDIENT, TOTAL, SERVING_SIZE_DESCRIPTION, MEAL_COUNT)
  names(output) <- c('Ingredient', 'Quantity', 'Units', 'Meal Count')

  if(forOutput == TRUE){return(output)} else {
    div(
      tags$table(class = "table table-striped",
        tags$thead(
          tags$tr(
            tags$th(scope = 'col', 'Ingredient'),
            tags$th(scope = 'col', 'Quantity'),
            tags$th(scope = 'col', 'Units'),
            tags$th(scope = 'col', 'Meal Count'),
          )
        ),
        tags$tbody(
          map(1:nrow(output), ~
            tags$tr(
              tags$td(output[.x,1]),
              tags$td(output[.x,2]),
              tags$td(output[.x,3]),
              tags$td(output[.x,4])
            )
          )
        )
      )
    )
  }
}

#' dailyMenu
#' @description Creates the HTML data table of meals and ingredients by river day.
#' This is used in a loop to create the daily menu item.
#' @param session SEssion used for ns.
#' @param id The meal unique ID iterator.
#' @param data The LOCAL rv data object.
#' @noRd
dailyMenu <- function(session, id, data){
  ns <- session$ns
  LOCAL <- data
  req(nrow(LOCAL$myMeals) > 0)

  meal <- LOCAL$myMeals %>%
    filter(MEAL_UNIQUE_ID == id)

  day <- unique(meal$RIVER_DAY)
  mtype <- unique(meal$MEAL_TYPE)
  ttl <- unique(meal$MEAL_NAME)
  noAdults <- unique(meal$NO_ADULTS)
  noKids <- unique(meal$NO_KIDS)

  header <- paste('Day',day,'|',mtype,'|',ttl,'|',noAdults,'Adults |', noKids,'Kids')

  ings <- meal %>%
    select(INGREDIENT, QTY, SERVING_SIZE_DESCRIPTION, STORAGE_DESCRIPTION) %>%
    arrange(INGREDIENT)


  div(
    h4(header, style = 'color: black; text-align: left;'),
    tags$table(class = "table table-striped",
               tags$thead(
                 tags$tr(
                   tags$th(scope = 'col', 'Ingredient'),
                   tags$th(scope = 'col', 'Quantity'),
                   tags$th(scope = 'col', 'Units'),
                   tags$th(scope = 'col', 'Storage'),
                 )
               ),
               tags$tbody(
                 map(1:nrow(ings), ~
                       tags$tr(
                         tags$td(ings[.x,1]),
                         tags$td(ings[.x,2]),
                         tags$td(ings[.x,3]),
                         tags$td(ings[.x,4])
                       )
                 )
               )
    ),
    br()
  )
}

