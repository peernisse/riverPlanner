# HELPER FUNCTIONS FOR mod_menu

#####CARDS TEMPLATES#####

#' Meal card UI function
#' @description View and adjust Meal record info in a card.
#' @param session The shiny session
#' @param id The id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#' @noRd
mealCard <- function(input, session,id,data = LOCAL, mtype, ttl, subttl, desc){
    ns <- session$ns
    LOCAL <- data

    if(file.exists(paste0('./inst/app/www/assets/img/menu/',mtype,'-',id,'.jpg')) == TRUE){
        imgsrc <- paste0('www/assets/img/menu/',mtype,'-',id,'.jpg')
    } else {
        imgsrc <- 'www/assets/img/menu/default.jpg'
    }

    # Choose which button view or edit based on user ID
# TODO 11/17/2023 I just added isolate here to see if it will prevent reloading modal
    mealUserID <- isolate(LOCAL$LU_MEAL[which(LOCAL$LU_MEAL$MEAL_ID == id),]$USER_ID)

    if(mealUserID %in% LOCAL$userID){
        actionChoice <- actionButton(inputId = ns(paste0('root-edit-',id)),
            label = NULL, icon = icon('pencil'),
            type = "button",class = "btn btn-md btn-primary",
            style = 'margin-left: 3px;'
        )
    }

    if(!mealUserID %in% LOCAL$userID){
        actionChoice <- actionButton(inputId = ns(paste0('view-',id)),
            label = NULL, icon = icon('eye'),
            type = "button",class = "btn btn-md btn-primary",
            style = 'margin-left: 3px;'
        )
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
                            choices = c(unique(LOCAL$LU_MEAL_TYPE$MEAL_TYPE)),
                            selected = mtype),
                actionChoice,
                # actionButton(inputId = ns(paste0('view-',id)),label = NULL, icon = icon('eye'),
                #     type = "button",class = "btn btn-md btn-primary",
                #     style = 'margin-left: 3px;'
                # ),
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

        filtDat <- LOCAL$ALL_DATA %>% filter(MEAL_TYPE %in% mtype, MEAL_ID != 0) %>% # removing meal_id 0 "startup"
        #filtDat <- LOCAL$ALL_DATA %>% filter(MEAL_TYPE %in% mtype) %>% # old 11/28/2023
            select(MEAL_ID,MEAL_TYPE,MEAL_NAME,MEAL_DESCRIPTION) %>% unique(.)

        mapIndexRows <- which(filtDat$MEAL_TYPE == mtype)

        # Make cards

        div(class = "container-fluid py-2", style = 'padding-left: inherit; padding-right: inherit;',
            div(class = 'row', style = 'text-align: center; margin-bottom: 5px;',
                h6(paste0('<- Explore ',mtype,' (',length(mapIndexRows),') Items ->'))
            ),

            div(class = "d-flex flex-row flex-nowrap overflow-auto",

                map(mapIndexRows, ~ mealCard(input, session,filtDat[.,'MEAL_ID'],data = LOCAL,
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




