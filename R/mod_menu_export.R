#' menu_export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_menu_export_ui <- function(id, session){
  ns <- NS(id)
  tagList(

    customModalDialog(
        tabsetPanel(id = ns('menuExports'),
          tabPanel(title = 'shopList',
            uiOutput(ns('placeholder')),
            tableOutput(ns('viewShoplist'))
          ),
          tabPanel(title = 'menu',
            uiOutput(ns('placeholder'))
          )
        ),

        # customModalDialog arguments -----
        session = session,
        title = h4(ttl, style = 'color: #5cb874'),
        size = 'xl',
        easyClose = FALSE,
        fade = FALSE,
        footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; justify-content: flex-end;',
                          actionButton(ns('exportMenu'), label = 'Export', class = 'btn btn-success'),
                          actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
                                       class = 'riv', class = 'getstarted')
        ) # end footer
    )
  )
}

#' menu_export Server Functions
#'
#' @noRd
mod_menu_export_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    LOCAL <- data

    # Observer list -----

    createdObservers <- c()

    # Dynamic Title -----

    observe({
      ttl <<- paste('Viewing menu or shoplist here...')

    })


    # Shoplist View -----

    shopList <- function(data){
      LOCAL <- data
      req(nrow(LOCAL$myMeals) > 0)

      output <- LOCAL$myMeals %>%
        group_by(INGREDIENT, SERVING_SIZE_DESCRIPTION) %>%
        summarize(
          TOTAL = sum(QTY, na.rm = TRUE) %>% as.character(),
          MEAL_COUNT = length(INGREDIENT)
        )
      names(output) <- c('Ingredient','Units','Quantity', 'Meal Count')
      return(output)
    }

    output$viewShoplist <- renderTable(shopList(data = LOCAL), striped = TRUE,
      hover = TRUE, spacing = 's'
    )


    # Shoplist Export -----

    # Menu View -----

    # Menu Export -----

    # UI Outputs -----

    # Placeholder
    output$placeholder <- renderUI({
      if(nrow(LOCAL$myMeals) > 0){
        return(NULL)
      }

      if(nrow(LOCAL$myMeals) == 0){
        p('This item requires you to build or load a menu first...')
      }
    })


    #--------------------------------------------------------------------------
  })
}

## To be copied in the UI
# mod_menu_export_ui("menu_export_1")

## To be copied in the server
# mod_menu_export_server("menu_export_1")
