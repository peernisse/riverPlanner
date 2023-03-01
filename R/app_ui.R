#' The application User-Interface
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyFeedback feedback feedbackWarning feedbackDanger feedbackSuccess useShinyFeedback
#' @importFrom htmltools css
#' @noRd
app_ui <- function(request) {
  suppressDependencies('bootstrap','javascript','jquery')
  #suppressDependencies('bootstrap','javascript')


  golem_add_external_resources()
  htmlTemplate(filename = './inst/app/www/index.html',
  #htmlTemplate(filename = './inst/app/www/index.html',

    trip = mod_trip_ui('trip'),
    menu = mod_menu_ui('menu'),
    userName = mod_data_ui('data_gs')

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
