#' The application User-Interface
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib bsplus shinyBS
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage('River Planner',
               header = tags$head(
                 tags$link(rel="stylesheet",
                           href="https://cdn.jsdelivr.net/npm/bootstrap@3.3.7/dist/css/bootstrap.min.css",
                           integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u",
                           crossorigin="anonymous"),
                 tags$link(rel="stylesheet", type = "text/css", href="www/style.css"),
               ),
      tabPanel('Home'),
      tabPanel('Menu',
        mod_menu_ui('menu')
      ),
      tabPanel('Crew'),
      tabPanel('Gear')
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "riverPlanner"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
