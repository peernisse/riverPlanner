#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param mdata The global reactiveValues object shared between modules
#' @import shiny tictoc
#' @importFrom purrr map map_chr
#' @importFrom utils data
#' @noRd
app_server <- function(input, output, session) {


    LOCAL <- mod_data_server('data_gs')

    LOCAL <- mod_trip_server('trip', data = LOCAL)

    LOCAL <- mod_menu_server('menu', data = LOCAL)

    LOCAL <- mod_meal_edit_server('editMeal', data = LOCAL)

    LOCAL <- mod_meal_create_server('createMeal', data = LOCAL)

    mod_menu_export_server('menuExport', data = LOCAL)

    mod_gear_server('gear')

    mod_donate_server('donate')

    mod_travel_server('travel')

}
