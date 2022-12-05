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
    div(id = ns(id), class='card',
        div(class='card-body',
            h5(class='card-title', ttl),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text', desc),
            actionButton(inputId = ns(paste0('add-',id)),label = 'Add', onclick =
                             ns("Shiny.onInputChange('thisClick',this.id)"))
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

            # Make card button observers
            map(addButtonIDs, ~ observeEvent(input[[.x]],{

                # buttonIDs is created in the mod_menu module level namespace.
                # This captures the button ID when a button is clicked into buttonIDs

                if ( is.null( buttonIDs[[.x]]) ) buttonIDs[[.x]] <- 1L
                if ( !is.null( buttonIDs[[.x]]) ) buttonIDs[[.x]] <- buttonIDs[[.x]] + 1L

                LOCAL$myMeals <- LOCAL$LU_MEAL %>%
                    filter(MEAL_ID %in% isolate(gsub('add-','',names(buttonIDs))))

                output$selMeals <- renderTable(isolate(LOCAL$myMeals))

                }, autoDestroy = FALSE)
            )

            # Make cards
            map(mapIndexRows, ~ mealCard(session,LOCAL$LU_MEAL[.,'MEAL_ID'],
                                         LOCAL$LU_MEAL[.,'MEAL_NAME'],
                                         NULL, LOCAL$LU_MEAL[.,'MEAL_DESCRIPTION']))
        })
    }
