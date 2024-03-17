#' gear UI Function
#' @description Gear module UI.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_gear_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        #h3('Select Equipment Checklists'),
        h3(textOutput(ns('checkTitle'))),
        # div(class = 'section-title',
        #   p('Use the checklist categories below to build a trip gear list.
        #     Preview and export the combined trip checklist as one HTML checklist
        #     file to use on your mobile device.'
        #   )
        # ),
        div(id = ns("checklistSelect"), class = "accordion",
          accInner(ns, parentId = "checklistSelect", buttonId = 'selectChecklists',
           buttonTitle = 'View Checklists', collapseId = 'collapseChecklists',
           show = FALSE, body = uiOutput(ns('catChecklists')), bgColor = TRUE)
        )
      )
    ),
    fluidRow(
      column(width = 12, style = 'text-align: center; margin: 5px;padding: 20px;',
        #hr(style = 'width: 33%; margin-left: 33%; margin-right: 33%;'),
        # h3('Customize Trip Checklists'),
        # div(class = 'section-title',
        #   p('Edit the checklists you have selected. When they are ready,
        #     export a combined trip checklist to use on your mobile device.')
        # ),
        # div(id = ns("checklistCustomize"), class = "accordion",
        #   accInner(ns, parentId = "checklistCustomize", buttonId = 'customizeChecklists',
        #    buttonTitle = 'Selected Checklists', collapseId = 'collapseSelectedChecklists',
        #    show = FALSE, body = p('Nothing here yet...'), bgColor = TRUE)
        # ),
        # br(),
        actionButton(ns('checklistExport'), label = 'Export Equipment Checklist',
                     class = 'btn btn-success', style = 'margin:5px;')
      )
    )
  )
}

#' gear Server Functions
#'
#' @noRd
mod_gear_server <- function(id, data){
    moduleServer( id, function(input, output, session){
    ns <- session$ns
    LOCAL <- data

    # CONSTANTS ----

    gearCatIDs <- reactive(unique(LOCAL$LU_GEAR_CAT$GEAR_CAT_ID))
    gearCatViewBtns <- reactive(paste0('view-', gearCatIDs()))
    gearCatAddBtns <- reactive(paste0('add-', gearCatIDs()))

    # observe({
    #     gearCatIDs <<- unique(LOCAL$LU_GEAR_CAT$GEAR_CAT_ID)
    #     gearCatViewBtns <<- paste0('view-', gearCatIDs)
    # })


    # OBSERVE BUTTONS ----

    observe({

        ## View Gear Checklists Buttons ----

        purrr::map(seq_along(gearCatViewBtns()), ~
            observeEvent(input[[gearCatViewBtns()[[.x]]]], {
                id <- gsub('view-', '', gearCatViewBtns()[[.x]])

                if(length(LOCAL$tripName) == 0 & LOCAL$noAdults == 1){
                    showNotification('Please load, or create and save a Trip before
                        using gear checklists.', type = 'error', duration = 10)
                    return(NULL)
                }
# TODO 3/16/2024 HERE you could update a LOCAL$gearCatActive value as `id`
                LOCAL$gearCatActive <- id
                viewChecklist(session, id = id, data = LOCAL)
            })
        )

        ## Close Checklist Modal Button ----

        observeEvent(input$closeChecklist, {
            LOCAL$gearCatActive <- numeric()
            removeModal()
        })

        ## Save Checklist Modal Button ----

        observeEvent(input$saveChecklist, {

            ## Get List of Selected Gear IDs ----

            # TODO 3/16/2024 Need to account for which gear category is active
            # bc it is listing all/any item IDs from input
            # that have been checked in any category, so deletes during dbUpdate
            # items other than the active category (HINT: look in the dbUpdate routine)

            sldIds <- LOCAL$LU_GEAR %>%
                filter(GEAR_CAT_ID == LOCAL$gearCatActive) %>%
                mutate(SLD_IDS = paste0('sld-', GEAR_ID)) %>%
                pull(SLD_IDS)


                #sldrs <- names(input)[grep('sld-', names(input))]
                #sldvals <- map(sldrs, ~ input[[.x]]) %>% unlist()
                sldvals <- map(sldIds, ~ input[[.x]]) %>% unlist()
                item_ids <- sldIds[which(sldvals == TRUE)] %>%
                    gsub('sld-', '', .) %>%
                    as.numeric(.)
                item_cat_ids <- LOCAL$LU_GEAR$GEAR_CAT_ID[which(LOCAL$LU_GEAR$GEAR_ID %in% item_ids)]

                ## Validate only one unique cat ID and that it matches ----
                # LOCAL$gearCatActive
                if(length(unique(item_cat_ids)) > 1) stop('Gear category is not unique')

                ## Save operations ----

                ## Case all items in category have been switched off ----
                ## Delete any records in xref_gear where gear_cat_id == LOCAL$GearCatActive
                if(length(unique(item_cat_ids)) == 0) {
                    withProgress({
                        incProgress(.25)
                        con <- rivConnect()
                            deleteXrefGearCat(con = con, rvObj = LOCAL, catId = LOCAL$gearCatActive)
                            refreshLOCAL(con = con, data = LOCAL, tables = c('XREF_GEAR'))
                        dbDisconnect(con)
                        incProgress(.75)

                    }, message = 'Saving trip checklist updates...')

                } else {

                    if(unique(item_cat_ids) != LOCAL$gearCatActive) stop('Gear category mismatch')
                    # run dbUpdate for xref_gear

                    ## Get List of Selected Gear Quantities ----

                    gqty <- map(item_ids, ~ input[[paste0('gear-qty-', .x)]]) %>% unlist()

                    ## Validate all Checked have Values ----

                    if(any(gqty[!is.na(gqty)] < 0)) {

                        neg_ids <- item_ids[which(gqty < 0)]
                        rows <- which(LOCAL$LU_GEAR$GEAR_ID %in% neg_ids)
                        neg_items <- LOCAL$LU_GEAR$GEAR_NAME[rows] %>% sort()

                        showNotification(
                            p('Gearlist item quantities can not be negative:'),
                            tags$ul(map(neg_items, ~ tags$li(.x))),
                            type = 'error', duration = 10
                        )

                        return(NULL)
                    }

                    if(any(is.na(gqty))) {

                        empty_ids <- item_ids[which(is.na(gqty))]
                        rows <- which(LOCAL$LU_GEAR$GEAR_ID %in% empty_ids)
                        empty_items <- LOCAL$LU_GEAR$GEAR_NAME[rows] %>% sort()

                        showNotification(
                            p('Active items are missing quantities:'),
                            tags$ul(map(empty_items, ~ tags$li(.x))),
                            type = 'error', duration = 10
                        )

                        return(NULL)
                    }

                    # Create records to be updated

                    withProgress({
                        incProgress(.25)
                        toAdd <- data.frame(
                            TRIP_ID = LOCAL$tripID,
                            GEAR_ID = item_ids,
                            GEAR_CAT_ID = item_cat_ids,
                            GEAR_QTY = gqty,
                            USER_ID = LOCAL$userID,
                            UPTIME = Sys.Date(),
                            UPUSER = LOCAL$userName
                        )
                        incProgress(.75)
                        dbUpdate(from = toAdd, to = 'XREF_GEAR', data = LOCAL)
                    }, message = 'Saving trip checklist updates...')
                }

            removeModal()
            #clear out LOCAL$gearCatActive
            LOCAL$gearCatActive <- numeric()
            showNotification('Trip gear checklist updated!', type = 'message')
        })
    })

    # UI OUTPUTS ----

    ## Ckeclists Title ----

    output$checkTitle <- renderText({

        if(length(LOCAL$tripName) > 0) {
            out <- paste('Equipment Checklists for', LOCAL$tripName)
        }

        if(length(LOCAL$tripName) == 0) out <- 'Equipment Checklists'

        out
    })

    ## Gear Category Checklists ----

    output$catChecklists <- renderUI({

        if(length(LOCAL$tripID) > 0) {
            cats <- select(LOCAL$LU_GEAR_CAT, GEAR_CAT_ID)
            tripGear  <- LOCAL$XREF_GEAR %>%
                filter(TRIP_ID == LOCAL$tripID) %>%
                group_by(GEAR_CAT_ID) %>%
                summarize(count = length(GEAR_CAT_ID))

            gearCatTripCounts <- cats %>%
                left_join(tripGear, by = c('GEAR_CAT_ID')) %>%
                mutate(count = ifelse(is.na(count), 0, count)) %>%
                pull(count)
            subttl <- paste('Items:',gearCatTripCounts)
        } else {
            subttl <- NULL
        }

        div(class = "container-fluid py-2", style = 'padding-left: inherit; padding-right: inherit;',
            div(class = 'row', style = 'text-align: center; margin-bottom: 5px;',
                h6(paste0('<- Explore Gear Checklists (',length(gearCatIDs()),') Items ->'))
            ),
            div(class = "d-flex flex-row flex-nowrap overflow-auto",
                purrr::map(seq_along(gearCatIDs()), ~
                    gearCard(session = session,
                        gcat = LOCAL$LU_GEAR_CAT$GEAR_CAT_NAME[[.x]],
                        id = LOCAL$LU_GEAR_CAT$GEAR_CAT_ID[[.x]],
                        ttl = LOCAL$LU_GEAR_CAT$GEAR_CAT_NAME[[.x]],
                        subttl = subttl[[.x]],
                        desc = LOCAL$LU_GEAR_CAT$GEAR_CAT_DESC[[.x]]
                    )
                )
            )
        )
    })

    # RETURN LOCAL DATA OBJECT ----

    return(LOCAL)

    # END MOD_GEAR ----

    })
}

## To be copied in the UI
# mod_gear_ui("gear_1")

## To be copied in the server
# mod_gear_server("gear_1")
