#####CARDS TEMPLATES#####

#' Meal card UI function
#' @description The UI template for displaying datframe record info in a card.
#' @param session The shiny session
#' @param id The id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#' @noRd
mealCard <- function(session,id, ttl, subttl, desc){
    ns <- session$ns
    # div(id = ns(id), class='card',
    #     div(class='card-body',
    #         h5(class='card-title', ttl),
    #         h6(class='card-subtitle mb-2 text-muted', subttl),
    #         p(class='card-text', desc),
    #         actionButton(inputId = ns(paste0('add-',id)),label = 'Add')
    #       )
    # )

    div(id = ns(id), class = "card",
        div(class = "card-body",
            h5(class = "card-title d-flex justify-content-between align-items-center", ttl,
               div(
                   actionButton(inputId = ns(paste0('add-',id)),label = NULL, icon = icon('plus'),
                                type = "button",class = "btn btn-sm btn-success"),
                   actionButton(inputId = ns(paste0('del-',id)),label = NULL, icon = icon('trash'),
                                type = "button",class = "btn btn-sm btn-danger")
               ),

            ),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text', desc)
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
makeMealCards <- function(input, output, session, mtype, data = LOCAL, idObj = buttonIDs){
    LOCAL <- data
    buttonIDs <- idObj

    renderUI({
        ns <- session$ns
        mapIndexRows <- which(LOCAL$LU_MEAL$MEAL_TYPE == mtype)
        addButtonIDs <- LOCAL$LU_MEAL[mapIndexRows,'MEAL_ID'] %>% pull() %>% paste0('add-',.)

        # Make card Add button observers
        map(addButtonIDs, ~ observeEvent(input[[.x]],{

            # buttonIDs is created in the mod_menu module level namespace.
            # This captures the button ID when a button is clicked into buttonIDs

            if ( is.null( buttonIDs[[.x]]) ) buttonIDs[[.x]] <- 1L
            if ( !is.null( buttonIDs[[.x]]) ) buttonIDs[[.x]] <- buttonIDs[[.x]] + 1L

            #browser()
            LOCAL$myMeals <- LOCAL$LU_MEAL %>%
                select(-c(UPTIME,UPUSER)) %>%
                left_join(LOCAL$XREF_INGREDIENT %>% select(MEAL_ID,INGREDIENT_ID), by = 'MEAL_ID') %>%
                left_join(LOCAL$LU_INGREDIENTS %>% select(-c(UPTIME,UPUSER)), by = 'INGREDIENT_ID') %>%
                filter(MEAL_ID %in% isolate(gsub('add-','',names(buttonIDs))))

            #output$selMeals <- renderTable(isolate(LOCAL$myMeals %>% select(MEAL_ID,MEAL_NAME) %>% unique(.)))

            }, autoDestroy = FALSE)
        )

        # Make cards
        map(mapIndexRows, ~ mealCard(session,LOCAL$LU_MEAL[.,'MEAL_ID'],
                                     LOCAL$LU_MEAL[.,'MEAL_NAME'],
                                     NULL, LOCAL$LU_MEAL[.,'MEAL_DESCRIPTION']))
    })
}

#' Ingredient card generator
#' @description Map over ingredients by meal and make cards
#' @noRd
makeIngredientCards <- function(input, output, session, data, mealid){

    renderUI({
        ns <- session$ns
#browser()
        mapIngredientRows <- which(data$MEAL_ID == mealid)

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

    mealIDs <- data %>% pull(MEAL_ID) %>% unique(.)

    firstDiv <- paste0("bs_accordion(ns('",outer,"')) %>% ")
    opts <- "bs_set_opts(panel_type = 'default', use_heading_link = TRUE) %>% "

    secondDiv <- character()

    for(i in 1:length(mealIDs)){
        #browser()
        headerInfo <- data %>%
            filter(MEAL_ID == mealIDs[i]) %>%
            mutate(MEAL_BOX_HEADER = paste(MEAL_NAME,'|',MEAL_TYPE)) %>%
            pull(MEAL_BOX_HEADER) %>% unique(.)



        # Make boxes and ingredient cards

        # This line should not have return characters
       # str <- paste0("bs_append(title = '",headerInfo, "', content = makeIngredientCards(input, output, session, data = data, mealid = mealIDs[i]))")

        str <- paste0("bs_append(title = '",headerInfo, "', content = makeIngredientCards(input, output, session, data = data, mealid = ",mealIDs[i],"))")

        if(length(mealIDs) == 1){

            secondDiv <- paste(firstDiv, opts, str)

        } else

        if(mealIDs[i] == (mealIDs)[1]){

            secondDiv <- paste(firstDiv, opts, str," %>% ")

        } else

        if(mealIDs[i] == (mealIDs)[length(mealIDs)]){

            secondDiv <- paste(secondDiv, str)

        } else {

            secondDiv <- paste(secondDiv,str, " %>% ")
        }
    }

    return(eval(parse(text = secondDiv)))
}






