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
                             #type = "button",class = "btn btn-sm btn-primary",
                             style = 'margin-left: 3px;'
                ),
                actionButton(inputId = ns(paste0('add-',id)),label = NULL, icon = icon('plus'),
                             #type = "button",class = "btn btn-sm btn-success",
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

                actionButton(inputId = ns(paste0('edit-',id)),label = NULL, icon = icon('pencil'),
                             #type = "button",class = "btn btn-sm btn-primary",
                             style = 'margin-left: 3px;'
                ),
                actionButton(inputId = ns(paste0('del-',mealUniqueId)),label = NULL, icon = icon('trash'),
                             #type = "button",class = "btn btn-sm btn-success",
                             style = 'margin-left: 3px;'),
            ),
        )
    )
}

#' Ingredient card UI function
#'
#'
#'
#'
ingredientCard <- function(session,id, ttl, subttl, desc){
    ns <- session$ns
    div(id = ns(id), class='card',
        div(class='card-body',
            h5(class='card-title', ttl),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text', desc),
            actionButton(inputId = ns(paste0('ingEdit-',id)),label = NULL, icon = icon('pencil')),
            actionButton(inputId = ns(paste0('ingAdd-',id)),label = NULL, icon = icon('plus')),
            actionButton(inputId = ns(paste0('ingNew-',id)),label = NULL, icon = icon('check')),
            actionButton(inputId = ns(paste0('ingKill-',id)),label = NULL, icon = icon('trash'))
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
#'
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
#' @param session The shiny session
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
            div(class = "d-flex flex-row flex-nowrap overflow-auto",

                map(mapIndexRows, ~ menuCard(session,filtDat[.,'MEAL_ID'],data = filtDat[.,],
                                             mtype = filtDat[.,'MEAL_TYPE'], filtDat[.,'MEAL_NAME'],
                                             NULL, filtDat[.,'MEAL_DESCRIPTION']))
            )

        )

    })
}

#' Ingredient card generator
#' @description Map over ingredients by meal and make cards
#' @noRd
makeIngredientCards <- function(input, output, session, data, mealUniqueId){

    renderUI({
        ns <- session$ns
#browser()
        mapIngredientRows <- which(data$MEAL_UNIQUE_ID == mealUniqueId)

        map(mapIngredientRows, ~ ingredientCard(session,
                                    id = data[.,'INGREDIENT_ID'],
                                    ttl = data[.,'INGREDIENT'],
                                    subttl = data[.,'SERVING_SIZE_DESCRIPTION'],
                                    desc = data[.,'INGREDIENT_DESCRIPTION'])
        )
    })
}



#####ACCORDION GENERATORS#####


#' Accordion box generator
#' @param session the shiny session object
#' @param outer Single character string to be the ID of the accordion group
#' @param data Dataframe of all meal information for selected meals
#'
#' @noRd
makeMealBoxes <- function(session, outer,data){
    #browser()
    req(!is.null(data) == TRUE & (nrow(data) > 0) == TRUE)
    ns <- session$ns

    mealIDs <- data %>% pull(MEAL_UNIQUE_ID) %>% unique(.)

    firstDiv <- paste0("bs_accordion(ns('",outer,"')) %>% ")
    opts <- "bs_set_opts(panel_type = 'default', use_heading_link = TRUE) %>% "
    dummy <- "bs_append(title = NULL, content = NULL) %>%"

    secondDiv <- character()

    for(i in 1:length(mealIDs)){
        #browser()
        headerInfo <- data %>%
            filter(MEAL_UNIQUE_ID == mealIDs[i]) %>%
            #mutate(MEAL_BOX_HEADER = paste(MEAL_NAME,'|',MEAL_TYPE,'| River Day ',RIVER_DAY)) %>%
            mutate(MEAL_BOX_HEADER = paste('River Day ',RIVER_DAY,'|',MEAL_TYPE,'|',MEAL_NAME)) %>%
            pull(MEAL_BOX_HEADER) %>% unique(.)

        # This line should not have return characters
        str <- paste0("bs_append(title = '",headerInfo, "', content = makeIngredientCards(input, output, session, data = data, mealUniqueId = '",mealIDs[i],"'))")

        if(length(mealIDs) == 1){

            secondDiv <- paste(firstDiv, opts, dummy, str)

        } else

        if(mealIDs[i] == (mealIDs)[1]){

            secondDiv <- paste(firstDiv, opts, dummy, str," %>% ")

        } else

        if(mealIDs[i] == (mealIDs)[length(mealIDs)]){

            secondDiv <- paste(secondDiv, str)

        } else {

            secondDiv <- paste(secondDiv,str, " %>% ")
        }
    }

    return(eval(parse(text = secondDiv)))
}



#' Accordion for each river day
#' @param input the session input object
#'
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
