#' Create meal modal
#' @description A large modal to create a new meal and save
#' to the user's trip and trip history
#' @param input,output,session The shiny session objects
#'
#' @noRd
createMealModal <- function(input, output, session, data = LOCAL){
    ns <- session$ns
    LOCAL <- data

    ttl <- 'Creating New Meal'
    luMtypes <- c('Choose meal type...',LOCAL$LU_MEAL_TYPE$MEAL_TYPE %>% as.character())
    LOCAL$editMealModalSwitch <- TRUE

    customModalDialog(
        # Instructions -----
        p('Here will be the create new meal modal'),

        # Create Meal Data Entry -----
        div(class = "input-group", class = 'create-meal',
            tags$span(class = "input-group-text", class = 'create-meal',
                      'Meal Name',
                      #style = 'background-color: #ed7000; border-color: #ed7000; color: #fff;'
            ),
            tags$input(id = ns('meal-new-name'),
                       placeholder = 'New Meal Name', type = "text",
                       `aria-label` = "New Meal Name", class = "form-control", class = 'create-meal')
        ),

        div(class = "input-group", class = 'create-meal',
            tags$label(class = "input-group-text", class = 'create-meal',
                       `for` = ns('meal-new-type'),
                       #style = "background-color: #ed7000; border-color: #ed7000; color: #fff;",
                       'Meal Type'
            ),
            tags$select(id = ns('meal-new-type'), class = "form-select", class = 'create-meal',
                        tags$option(selected = "selected", luMtypes[1]),
                        map(
                            luMtypes[-1], ~ tags$option(value = .x,.x)
                        )
            )
        ),
        customTextAreaInput(inputId = ns('meal-new-desc'), label = 'Meal Description',
                            labelColor = '#ed7000', width = '100%'),
        fluidRow(style = 'margin-top:20px;',
                 column(width = 12,
                        h5('Ingredients/Quantities'),
                        p('Building ingredient list here')
                        #uiOutput(ns('modalIngs'))
                 )
        ),
        fluidRow(style = 'margin-top:20px;',
                 column(width = 12,
                        h5(ttl),
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

        fluidRow(
            column(width = 12,
                   h5('Tools Needed'),
                   p("Enter the general cooking tools needed, separated by semi-colons ';'",
                     style = 'font-style: italic;'),
                   customTextAreaInput(inputId = ns('meal-new-tools'), label = 'Tools',
                                       labelColor = '#ed7000', width = '100%',
                                       placeholder = "Separate tools with ';'"),
            )
        ),

        fluidRow(
            column(width = 12,
                   h5('Instructions'),
                   p("Enter the general cooking steps needed, separated by semi-colons ';'",
                     style = 'font-style: italic;'),
                   customTextAreaInput(inputId = ns('meal-new-inst'), label = 'Instructions',
                                       labelColor = '#ed7000', width = '100%',
                                       placeholder = "Separate steps with ';'"),
            )
        ),

        # customModalDialog arguments -----

        session = session,
        title = h4(ttl, style = 'color: #5cb874'),
        size = 'xl',
        easyClose = FALSE,
        fade = FALSE,
        footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',
                          actionButton(ns('createMeal'), label = 'Save', class = 'btn btn-success'),
                          actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
                                       class = 'riv', class = 'getstarted')
        ) # end footer

    ) # End customModalDialog




}

#
