#' menu_export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @import rmarkdown
#' @noRd
#'
mod_menu_export_ui <- function(id, session){
  ns <- NS(id)

  tagList(
    customModalDialog(
      div(id = ns('menuExports'), class = "accordion", style = 'text-align: center;',
        accInner(ns, parentId = 'menuExports', buttonId = ns('shopList'),
          buttonTitle = 'View Shopping List', collapseId = paste0('collapse-',ns('shopList')),
          body = tableOutput(ns('viewShoplist'))
        ),
        accInner(ns, parentId = 'menuExports', buttonId = ns('dailyMenu'),
          buttonTitle = 'View Daily Menu', collapseId = paste0('collapse-',ns('dailyMenu')),
          body = uiOutput(ns('viewDailyMenu'))
        )
      ),
      # customModalDialog arguments -----
      session = session,
      title = uiOutput(ns('exportMenuModalTitle')),
      size = 'xl',
      easyClose = FALSE,
      fade = FALSE,
      footer = fluidRow(style = 'display: flex; flex-wrap: nowrap; flex-direction: column; justify-content: center;',
        downloadButton(ns('exportShoplist'), label = 'Export Shopping List',
          style = 'margin-top:5px; margin-left 5px;', class = 'btn btn-success'),
        downloadButton(ns('exportMenu'), label = 'Export Menu',
          style = 'margin-top:5px; margin-left 5px;', class = 'btn btn-success'),
        actionButton(ns('editMealModalClose_2'), label = 'Close',
          style = 'margin-top:5px;', class = 'btn btn-default',
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

    # OBSERVERS -----

    # Cancel menu export modal button footer -----

    observeEvent(input$editMealModalClose_2,{
      LOCAL$exportMenuModalSwitch <- FALSE
      removeModal()
    })

    # Export menu items modal buttons footer -----

    # Export shoplist
    # TODO contact someone about the download puts out the preceding filename
    # if you export, then load a different trip, then export, the first export filename is used again
    output$exportShoplist<-downloadHandler(
      filename = paste(LOCAL$tripName,"_shopList", ".html", sep = ""),
      content = function(file){
#browser()
        tempReport <- file.path(tempdir(), "shopList.Rmd")
        file.copy("./inst/app/www/shopList.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          storageTitle = gsub(' ','_',LOCAL$tripName),
          data = shopList(data = LOCAL),
          title = paste0('Shop List | ',LOCAL$tripName,' | ',LOCAL$noAdults,' Adults | ',
                         LOCAL$noKids, ' Kids'
                  )
        )

        withProgress(message = 'Creating Shopping List Output...',
          rmarkdown::render(tempReport, output_file = file, params = params,
            clean = TRUE, envir = new.env(parent = globalenv())
          )
        )#end progress
      }

    )#end download handler

    # Export menu

    output$exportMenu<-downloadHandler(
      filename = paste(LOCAL$tripName,"_dailyMenu", ".html", sep = ""),
      content = function(file){
        #browser()
        tempReport <- file.path(tempdir(), "dailyMenu.Rmd")
        file.copy("./inst/app/www/dailyMenu.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          #storageTitle = gsub(' ','_',LOCAL$tripName),
          data = LOCAL$myMeals,
          title = paste0('Daily Menu | ',LOCAL$tripName,' | ',LOCAL$noAdults,' Adults | ',
                         LOCAL$noKids, ' Kids'
          )
        )

        withProgress(message = 'Creating Daily Menu Output...',
          rmarkdown::render(tempReport, output_file = file, params = params,
                            clean = TRUE,
                            #envir = new.env(parent = globalenv()) # This fails to make functions available to RMarkdown
                            envir = new.env()
          )
        )#end progress
      }

    )#end download handler


    # UI OUTPUTS -----

    # Shopping list view -----
    output$viewShoplist <- renderTable(shopList(data = LOCAL), striped = TRUE,
      hover = TRUE, spacing = 's', align = 'c', width = 'auto'
    )

    # Menu View -----
    output$viewDailyMenu <- renderUI({
      ns <- session$ns
      #browser()
      req(nrow(LOCAL$myMeals) > 0)
      req(LOCAL$exportMenuModalSwitch == TRUE)
      mealIDs <- LOCAL$myMeals %>%
        mutate(
          MEAL_TYPE = factor(MEAL_TYPE, levels = c('Breakfast','Lunch','Dinner','Appetizer','Dessert','Cocktail', 'Snack'))
        ) %>%
        arrange(RIVER_DAY,MEAL_TYPE) %>%
        pull(MEAL_UNIQUE_ID) %>%
        unique(.)
        map(mealIDs, ~ dailyMenu(session = session, id = .x, data = LOCAL))
    })

    # Dynamic Title -----

    output$exportMenuModalTitle <- renderUI({
      h4(paste('Outputs for', LOCAL$tripName), style = 'color: #5cb874')
    })

    #--------------------------------------------------------------------------
  })
}

