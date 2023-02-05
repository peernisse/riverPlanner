#' Edit meal modal
#' @description A large modal with to edit the ingredients of a meal and save
#' to the user's trip and trip history
#' @param input,output,session The shiny session objects
#' @param mealUniqueID The unique identifier for the meal derived from the button
#' ID clicked to open the modal
#' @param data The session reactiveValues object LOCAL#'
#' @noRd
editMealModal <- function(input, output, session, mealUniqueID, data){
    req(length(mealUniqueID) == 1)
    ns <- session$ns
    LOCAL <- data



#TODO figure out why this goes twice and blinks

    if(nrow(LOCAL$editMealDF) == 0 | !mealUniqueID %in% LOCAL$editMealDF$MEAL_UNIQUE_ID){
        LOCAL$editMealDF <- LOCAL$myMeals %>% filter(MEAL_UNIQUE_ID == mealUniqueID)
    } else {
        LOCAL$editMealDF <- LOCAL$editMealDF
    }

    # Turn on modal observers -----
    LOCAL$editMealModalSwitch <- TRUE

    # Set meal unique ID and ingredient uniqe IDs in LOCAL to use for calculating input IDs from the modal -----
    LOCAL$editMealMealUniqueID <- mealUniqueID

    ttl <- textOutput(ns('modalTitle'))
    ttl2 <- textOutput(ns('modalTitle2'))

    desc <- unique(LOCAL$editMealDF$MEAL_DESCRIPTION)
    notes <- unique(LOCAL$editMealDF$MEAL_NOTES)
    tools <- LOCAL$editMealDF$TOOLS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()
    inst <- LOCAL$editMealDF$INSTRUCTIONS %>% unique() %>% gsub('; ',';',.) %>%
        strsplit(.,';') %>% unlist()
    rows <- c(1:nrow(LOCAL$editMealDF))

    # Modal HTML -----
    # customModalDialog(
    #
    #     div(class = "container",
    #
    #
    #
    #             tags$ul(class = "nav navbar nav-tabs", style = 'diplay: flex; justify-content: space-between;',
    #                 tags$li(class = "active",
    #                     a(class = "active", `data-toggle` = "tab", href = "#people", "People")
    #                 ),
    #                 tags$li(
    #                     a(`data-toggle` = "tab", href = "#ings", "Ingredients")
    #                 ),
    #                 tags$li(
    #                     a(`data-toggle` = "tab", href = "#addIng", "Add Ingredients")
    #                 ),
    #                 tags$li(
    #                     a(`data-toggle` = "tab", href = "#newIng", "Create Ingredients")
    #                 ),
    #                 tags$li(
    #                     a(`data-toggle` = "tab", href = "#notes", "Notes/Info")
    #                 )
    #             ),
    #
    #
    #
    #         div(class = "tab-content",
    #             div(id = "people", class = "tab-pane fade in active",
    #                 h5(desc),
    #                 p('If there will be a change of people during the trip, you can change the people inputs here
    #                    and they will just affect this meal on this day. You will need to do this for each meal that
    #                   has a change of people from the original numbers set up in `Trip Info`.'),
    #                 p('Each ingredient quantity can be edited and the multiplier will update for this meal only. This change
    #                   is not global and must be done again when using the same ingredient elsewhere, if desired.'),
    #
    #                 uiOutput(ns('mealTripInfo'))
    #             ),
    #             div(id = "ings", class = "tab-pane fade",
    #                 fluidRow(style = 'margin-top:20px;',
    #                      column(width = 12,
    #                          h5('Ingredients/Quantities'),
    #                          p('--If you don\'t like the calculated quantity for an ingredient, you can adjust it
    #                          and will adjust the multiplier for that ingredient for this meal.--',style = 'font-style: italic;'),
    #                          p('--Click TRASH to remove an ingredient from this meal.--',style = 'font-style: italic;'),
    #                          p('--To ADD a different ingredient, use the Add Ingredient Tab.--', style = 'font-style: italic;'),
    #                          uiOutput(ns('modalIngs'))
    #                      )
    #                  )
    #             ),
    #             div(id = "addIng", class = "tab-pane fade",
    #                 fluidRow(style = 'margin-top:20px;',
    #                      column(width = 12,
    #                          h5(ttl2),
    #                          p('--Start typing a word to filter the dropdown. Click + to add ingredient to this meal.--',
    #                          style = 'font-style: italic;'),
    #                          uiOutput(ns('modalSelIng'))
    #                      )
    #                  )
    #             ),
    #             div(id = "newIng", class = "tab-pane fade",
    #                 fluidRow(style = 'margin-top:20px;',
    #                     column(width = 12,
    #                         h5('Create New Ingredient'), #TODO make an i icon with info popover
    #                         icon(name = 'info'),
    #                         p('--Your new ingredient will appear in the Add Ingredient dropdown above.--',
    #                         style = 'font-style: italic;'),
    #                         uiOutput(ns('modalNewIng'))
    #                     )
    #                 )
    #             ),
    #             div(id = "notes", class = "tab-pane fade",
    #                 fluidRow(style = 'margin-top:20px;',
    #                     column(width = 12,
    #                         h5('Meal Notes'),
    #                         customTextAreaInput(inputId = ns(paste0('notes-',mealUniqueID)),
    #                             label = 'Meal Notes', value = notes, labelColor = '#162118', width = '100%',
    #                             height = NULL, cols = NULL, rows = NULL, placeholder = NULL,
    #                             resize = 'vertical', labelTextColor = '#fff', disabled = NULL)
    #                     )
    #                 ),
    #                 fluidRow(style = 'margin-top:20px;',
    #                     column(width = 12,
    #                         h5('Tools'),
    #                         tags$ul(
    #                             map(tools, ~ tags$li(.x))
    #                         )
    #                     )
    #                 ),
    #                 fluidRow(style = 'margin-top:20px;',
    #                     column(width = 12,
    #                         h5('Instructions'),
    #                         tags$ol(
    #                             map(inst, ~ tags$li(.x))
    #                         )
    #                     )
    #                 )
    #             )
    #
    #         )#end tab content
    #     ),#end container
    #     session = session,
    #     title = h4(ttl, style = 'color: #5cb874'),
    #     size = 'xl',
    #     easyClose = FALSE,
    #     fade = FALSE,
    #     footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',
    #         actionButton(ns('updateMeal'), label = 'Save', class = 'btn btn-success'),
    #         actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
    #                      class = 'riv', class = 'getstarted')
    #     )
    #
    # )

    customModalDialog(

        h5(desc),
        p('If there will be a change of people during the trip, you can change the people inputs here
           and they will just affect this meal on this day. You will need to do this for each meal that
          has a change of people from the original numbers set up in `Trip Info`.'),
        p('Each ingredient quantity can be edited and the multiplier will update for this meal only. This change
          is not global and must be done again when using the same ingredient elsewhere, if desired.'),

        uiOutput(ns('mealTripInfo')),

        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5('Ingredients/Quantities'),
                p('--If you don\'t like the calculated quantity for an ingredient, you can adjust it
                and will adjust the multiplier for that ingredient for this meal.--',style = 'font-style: italic;'),
                p('--Click TRASH to remove an ingredient from this meal.--',style = 'font-style: italic;'),
                p('--To ADD a different ingredient, use the ingredient picker below.--', style = 'font-style: italic;'),
                uiOutput(ns('modalIngs'))
            )
        ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5(ttl2),
                p('--Start typing a word to filter the dropdown. Click + to add ingredient to this meal.--',
                style = 'font-style: italic;'),
                uiOutput(ns('modalSelIng'))
            )
        ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5('Create New Ingredient'), #TODO make an i icon with info popover
                icon(name = 'info'),
                p('--Your new ingredient will appear in the Add Ingredient dropdown above.--',
                style = 'font-style: italic;'),
                uiOutput(ns('modalNewIng'))
            )
        ),
        fluidRow(style = 'margin-top:20px;',
            column(width = 12,
                h5('Meal Notes'),
                customTextAreaInput(inputId = ns(paste0('notes-',mealUniqueID)),
                    label = 'Meal Notes', value = ifelse(is.na(notes), NA_character_, notes),
                    labelColor = '#162118', width = '100%', height = NULL, cols = NULL,
                    rows = NULL, placeholder = NULL, resize = 'vertical',
                    labelTextColor = '#fff', disabled = NULL)
            )
        ),
        fluidRow(style = 'margin-top:20px;',
          column(width = 12,
            h5('Tools'),
            tags$ul(
              map(tools, ~ tags$li(.x))
            )
          )
        ),
        fluidRow(style = 'margin-top:20px;',
          column(width = 12,
            h5('Instructions'),
            tags$ol(
              map(inst, ~ tags$li(.x))
            )
          )
        ),
        session = session,
        title = h4(ttl, style = 'color: #5cb874'),
        size = 'xl',
        easyClose = FALSE,
        fade = FALSE,
        footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',
            actionButton(ns('updateMeal'), label = 'Save', class = 'btn btn-success'),
            actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
                         class = 'riv', class = 'getstarted')
        )
    )
}
