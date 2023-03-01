# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)


riverPlanner::run_app_auth0() # add parameters here (if any)


#riverPlanner::run_app() # add parameters here (if any)


#FOR LOCAL TESTING WITH AUTH0 enabled, comment out before putting to shinyapp.io
options( shiny.port = 4242)
