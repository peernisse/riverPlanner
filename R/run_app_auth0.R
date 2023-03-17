#' run_app_auth0
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#'
run_app_auth0 <- function(...) {
    with_golem_options(
        app = auth0::shinyAppAuth0(
            ui = app_ui(),        # these parenthesis are important
            server = app_server,
            config_file = system.file("app/_auth0.yml", package = "riverPlanner")
        ),
        golem_opts = list(...)
    )
}
