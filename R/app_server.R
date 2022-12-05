#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param mdata The global reactiveValues object shared between modules
#' @import shiny
#' @importFrom purrr map map_chr
#' @noRd
app_server <- function(input, output, session) {
    mdata <- mod_data_server('data_gs')
    mod_menu_server('menu',data = mdata)
}
