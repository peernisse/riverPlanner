#####CARDS TEMPLATES#####

#' Meal card UI function
#' @description View and adjust Meal record info in a card.
#' @param session The shiny session
#' @param id The id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#' @noRd
#'
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
                             type = "button",class = "btn btn-sm btn-primary",
                             style = 'margin-left: 3px;'
                ),
                actionButton(inputId = ns(paste0('add-',id)),label = NULL, icon = icon('plus'),
                             type = "button",class = "btn btn-sm btn-success",
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
#'
menuCard <- function(session,id, data, mtype, ttl, subttl, desc){
    ns <- session$ns
    mealUniqueId <- data$MEAL_UNIQUE_ID %>% unique(.)
#browser()
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
                             style = 'margin-left: 3px;'),
            ),
        )
    )
}

#####CARDS GENERATORS#####

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

        div(class = "container-fluid py-2",
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
  #browser()
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

#' Make row of menu cards for each river day. This displays the menu as it is being built.
#' @description Creates a collapsible horizontal row of menu cards. Uses the BS5 accordion CSS
#' @param session The shiny session object
#' @param outer the ID to use for the accordion group
#' @param data The reactive myMeals dataframe. The growing DF of selected meals to be displayed as cards.
#' @noRd
makeDayBoxes <- function(session, outer, data = LOCAL$myMeals){
    req(!is.null(data) == TRUE & (nrow(data) > 0) == TRUE)
    ns <- session$ns

    data <- data %>%
        mutate(
            MEAL_TYPE = factor(MEAL_TYPE,
                levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail'))
        ) %>%
        arrange(RIVER_DAY,MEAL_TYPE)


    days <- data %>% pull(RIVER_DAY) %>% unique(.) %>% sort(.)

    firstDiv <- paste0("bs_accordion(ns('",outer,"')) %>% ")
    opts <- "bs_set_opts(panel_type = 'default', use_heading_link = TRUE) %>% "
    dummy <- "bs_append(title = NULL, content = NULL) %>%"


    secondDiv <- character()

    for(i in 1:length(days)){

        #####DEV#####
        #i <- 1
        #browser()
        #############

        headerInfo <- paste('River Day', days[i])

        # This line should not have return characters
        str <- paste0("bs_append(title = '",headerInfo, "', content = makeMenuCards(input, output, session, day = days[",i ,"], data = data))")

        if(length(days) == 1){

            secondDiv <- paste(firstDiv, opts, dummy, str)

        } else

        if(days[i] == (days)[1]){

            secondDiv <- paste(firstDiv, opts, dummy, str," %>% ")

        } else

        if(days[i] == (days)[length(days)]){

            secondDiv <- paste(secondDiv, str)

        } else {

            secondDiv <- paste(secondDiv,str, " %>% ")
        }


    }

    return(eval(parse(text = secondDiv)))

}



#####MODAL DIALOGS#####

#' This is just shiny modalDialog with one extra line to add a button in
#' @description The shiny modalDialog function with added upper right close button for bootstrap 5
#' @param ... UI elements for the body of the modal dialog box.
#' @param title An optional title for the dialog.
#' @param footer UI for footer. Use NULL for no footer.
#' @param size One of "s" for small, "m" (the default) for medium, or "l" for large.
#' @param easyClose If TRUE, the modal dialog can be dismissed by clicking outside the dialog box,
#' or be pressing the Escape key. If FALSE (the default), the modal dialog can't be dismissed in those ways;
#' instead it must be dismissed by clicking on a modalButton(), or from a call to removeModal() on the server.
#' @param fade If FALSE, the modal dialog will have no fade-in animation (it will simply appear rather than fade in to view).
#' @param label The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon An optional icon() to appear on the button.
#' @noRd

customModalDialog <- function (..., session, title = NULL, footer = modalButton("Dismiss"),
                               size = c("m", "s", "l", "xl"), easyClose = FALSE, fade = TRUE) {
  ns <- session$ns
  size <- match.arg(size)
  backdrop <- if (!easyClose) "static"
  keyboard <- if (!easyClose) "false"

  div(id = "shiny-modal", class = "modal", class = if (fade)
    "fade", tabindex = "-1", `data-backdrop` = backdrop,
    `data-bs-backdrop` = backdrop, `data-keyboard` = keyboard,
    `data-bs-keyboard` = keyboard,
    div(class = "modal-dialog modal-dialog-scrollable", class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg",
                                                      xl = "modal-xl"),
        div(class = "modal-content",
            if (!is.null(title))
              div(class = "modal-header",
                  tags$h5(class = "modal-title", title),
                  tags$button(type="button", id = ns('editMealModalClose_1'), label = NULL,
                              class="btn-close shiny-bound-input", `data-dismiss`="modal",
                              `data-bs-dismiss`="modal", `aria-label`="Close")
              ),
              div(class = "modal-body", ...),
            if (!is.null(footer))
              div(class = "modal-footer", footer))), tags$script(HTML("if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {\n         var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));\n         modal.show();\n      } else {\n         $('#shiny-modal').modal().focus();\n      }")))
}

#' Ingredient info inputs for meal edit modal
#' @description Makes pre-filled input fields which serve to edit the data
#' @param input,output,session The shiny app session objects
#' @param data The meal dataframe being viewed/edited. Usually a subset of myMeals
#' @noRd
editMealIngredientInputs <- function(input, output, session,data){
  ns <- session$ns

  ingUniqueID <- unique(data$INGREDIENT_UNIQUE_ID)
  ing <- unique(data$INGREDIENT)
  desc <- unique(data$INGREDIENT_DESCRIPTION)
  unit <- unique(data$SERVING_SIZE_DESCRIPTION)
  ssf <- unique(data$SERVING_SIZE_FACTOR)
  qty <- unique(data$QTY)

  tagList(
    div(class = "input-group", style ='margin-top:12px;',
      tags$span(class = "input-group-text", ing,
           style = 'background-color: #162118; border-color: #162118; color: white;'),
      tags$input(id = ns(paste0('ing-ing-',ingUniqueID)), value = ing, type = "text",
                 disabled = 'disabled', `aria-label` = "Ingredient", class = "form-control"),
      tags$button(class = "btn btn-danger", type = "button", icon('trash'))
      #TODO needs button ID
    ),
    div(class = "input-group",
        tags$span(class = "input-group-text", 'Description'),
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
    div(class = "input-group",
        tags$span(class = "input-group-text", 'Quantity',
             style = 'background-color: #5cb874; border-color: #5cb874; color: white;'),
        tags$input(id = ns(paste0('ing-qty-',ingUniqueID)), value = qty, type = "text",
                   `aria-label` = "Quantity", class = "form-control")
    )
  )
}

#' Ingredient picker in edit meal modal or wherever else
#' @description creates searchable datalist select input of the ingredients in LU_INGREDIENTS
#' @param data The LOCAL$LU_INGREDIENTS dataframe
#'
#' @noRd
selectIngredients <- function(input, output, session,data){
  ns <- session$ns
#browser()
  ingIDs <- data %>% select(INGREDIENT_ID,INGREDIENT) %>% arrange(INGREDIENT)
  rows <- (seq(1:nrow(ingIDs)))

  renderUI({
    ns <- session$ns

    tagList(
      div(class = "input-group",
        tags$span(class = "input-group-text", 'Add Ingredient',
               style = 'background-color: #ed7000; border-color: #ed7000; color: white;'),
        tags$input(class = "form-control",list = "datalistOptions", id = "exampleDataList",
                   placeholder = "Select Ingredient or Type to Search..."
        ),
        tags$datalist(id = "datalistOptions",
          map(rows, ~ tags$option(value = ingIDs[.x,2]))
        ),
        tags$button(id = ns('addIngredient'), class = "btn", type = "button", icon('plus'),
                  style = 'background-color: #5cb874; border-color: #5cb874; color: white;'
        )
      )
    )
  })
}

#' Create new ingredient data entry fields
#' @description Input fields UI to create a new ingredient from within editMeal modal
#' @param session The session object
#' @noRd
createIngredients <- function(session){

  renderUI({
    ns <- session$ns

    tagList(

      div(class = "input-group", style ='margin-top:12px;',
          tags$span(class = "input-group-text", 'New Ingredient',
                    style = 'background-color: #ed7000; border-color: #ed7000; color: white;'),
          tags$input(id = ns('ing-new-ing'),
                     placeholder = 'Ingredient Name', type = "text",
                     `aria-label` = "Ingredient", class = "form-control"),
          tags$button(id = ns('newIngredient'), class = "btn", type = "button", icon('plus'),
                      style = 'background-color: #5cb874; border-color: #5cb874; color: white;'
          )
      ),
      div(class = "input-group",
          tags$span(class = "input-group-text", 'Description'),
          tags$input(id = ns('ing-new-desc'),
                     placeholder = 'Ingredient Description', type = "text",
                     `aria-label` = "Description", class = "form-control")
      ),
      div(class = "input-group",
          tags$span(class = "input-group-text", 'Units'),
          tags$input(id = ns('ing-new-unit'),
                     placeholder = 'Units of measure (e.g., 12 0z can, 1 apple, etc.',
                     type = "text", `aria-label` = "Units", class = "form-control")
      ),
      div(class = "input-group",
          tags$span(class = "input-group-text", 'Multiplier'),
          tags$input(id = ns('ing-new-ssf'),
                     placeholder = 'Adj. No. People X <This Multiplier> = Qty', type = "text",
                     `aria-label` = "Multiplier", class = "form-control")
      ),
      div(class = "input-group",
          tags$span(class = "input-group-text", 'Quantity',
                    style = 'background-color: #162118; border-color: #162118; color: white;'),
          tags$input(id = ns('ing-new-qty'),
                     placeholder = 'Adj. No. People X Multiplier = <This Qty>', type = "text",
                     `aria-label` = "Quantity", class = "form-control")
      )
    ) # end tagList
  })
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
customSelectInput <- function(inputId, label, labelColor, labelTextColor = 'white',
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
customTextInput <- function(inputId, label, value = "", labelColor, labelTextColor = 'white',
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
customTextAreaInput <- function(inputId, label, labelColor, labelTextColor = 'white', disabled = NULL){
  div(class = "input-group mb-3",
      span(class = "input-group-text",
           style = paste("background-color: ",labelColor,"; border-color: ",
                         labelColor,"; color: ",labelTextColor,";"),
           label),
      tags$textarea(id = inputId, class = "form-control", disabled = disabled, `aria-label` = label)
  )
}
