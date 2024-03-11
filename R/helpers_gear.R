# HELPER FUNCTIONS FOR GEAR MODULE

#' Template card UI for gear categories
#' @description View and select gear checklist categories.
#' @param gcat The gear category name
#' @param session The shiny session
#' @param id The gear category id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#'
#' @noRd
gearCard <- function(session, gcat, id, ttl, subttl, desc){
    ns <- session$ns

    ## Select image for card by gear category

    if(file.exists(paste0('./inst/app/www/assets/img/gear/',gcat,'-',id,'.jpg')) == TRUE){
        imgsrc <- imgsrc <- paste0('www/assets/img/gear/',gcat,'-',id,'.jpg')
    } else {
        imgsrc <- 'www/assets/img/gear/default.jpg'
    }

    ## Card UI Output

    div(id = ns(id), class = "card card-block mx-2", style="min-width:300px; margin-bottom:10px;",
        tags$img(class="card-img-top", alt="100%x280" , src = imgsrc,
                 height = '300', width = '300'
        ),
        div(class = "card-body",style="min-width:300px;",
            h5(class = "card-title d-flex justify-content-between align-items-left", ttl,),
            h6(class='card-subtitle d-flex mb-2 text-muted align-items-left', subttl),
            p(class='card-text',style = 'text-align: left;', desc),
            div(style = 'display: inline-flex; justify-content: flex-end; flex-wrap: nowrap; flex-direction: row;',
                actionButton(inputId = ns(paste0('view-',id)),label = NULL, icon = icon('pencil'),
                    type = "button",class = "btn btn-md btn-primary",
                    style = 'margin-left: 3px;'
                )#,
                # actionButton(inputId = ns(paste0('add-',id)),label = NULL, icon = icon('plus'),
                #     type = "button",class = "btn btn-md btn-success",
                #     style = 'margin-left: 3px;')
            )
        )
    )
}

#' The action to take when the preview checklist button is pressed
#' @description Opens a modal to view the checklist items
#' @param session The shiny session object
#' @param id The GEAR_CAT_ID for that checklist category
#' @param data The LOCAL data object
#' @noRd
viewChecklist <- function(session, id, data){
    ns <- session$ns
    LOCAL <- data

# TODO 3/11/2024 Make this get any trip gear that is in xref_gear and
# turn those witches on and fill in quantities
    ## Define Variables ----

    catrow <- which(LOCAL$LU_GEAR_CAT$GEAR_CAT_ID %in% id)
    rows <- which(LOCAL$LU_GEAR$GEAR_CAT_ID %in% id)
    ttl <- paste(LOCAL$tripName, '|', LOCAL$LU_GEAR_CAT$GEAR_CAT_NAME[[catrow]],
        'Checklist')

    gear_ids <- LOCAL$LU_GEAR$GEAR_ID[rows]
    names <- LOCAL$LU_GEAR$GEAR_NAME[rows]
    items <- LOCAL$LU_GEAR$GEAR_DESC[rows]

    stopifnot(length(gear_ids) == length(items))

    # DEtermine gear from this checklist gear category that is present
    # in xref_gear for this trip

    included <- LOCAL$XREF_GEAR %>%
        filter(GEAR_CAT_ID %in% id, TRIP_ID %in% LOCAL$tripID)
    values <- ifelse(gear_ids %in% included$GEAR_ID, TRUE, FALSE)
    onQtys <- ifelse(gear_ids %in% included$GEAR_ID, included$GEAR_QTY, NA_real_)



    # SHOW MODAL

    showModal(
        customModalDialog(
            collapseInstructions(nmsp = ns, id = 'gearChklist-1',
                ttl = '<Open Instructions>', icon = 'circle-info',
                p('Turn on items below and enter quantity. When done,
                  click "Save" to add the items to the trip checklist'
                ),
                p('To remove an item, switch it off and click "Save"'),
                p('To create a new checklist item for this category,
                  use the "Create New Item" form.')
            ),
            h3('Create New Item'),
            collapseInstructions(nmsp = ns, id = 'newItem-1',
                ttl = '<Open Create New Item Form>', icon = 'wrench',
                p('Here will be the form')
            ),
            h3(paste(ttl,'Items')),
            div(style = "text-align: left; padding-left: 10px;",
                purrr::map(seq_along(rows), ~
                    checkSwitch(session,
                        id = gear_ids[[.x]],
                        name = names[[.x]],
                        desc = items[[.x]],
                        status = values[[.x]],
                        qty = onQtys[[.x]]
                    )
                ),
            ),
            session = session,
            title = h4(ttl, style = 'color: #5cb874; '),
            size = 'fs',
            easyClose = FALSE,
            fade = TRUE,
            footer = fluidRow(class = 'modal-footer-row',
                actionButton(ns('saveChecklist'), label = 'Save', class = 'btn btn-success', class = 'riv'),
                actionButton(ns('closeChecklist'), label = 'Cancel', class = 'btn btn-default',
                    class = 'riv', class = 'getstarted')
            )
        )
    )
}

#' Draw a sliding check button
#' @description Draws one checkbox slider, description and qty input row
#' @param session The shiny session object
#' @param id Numeric Unique GEAR_ID from LU_GEAR
#' @param name Character Gear name will be bolded
#' @param desc Character Gear item description for the id
#' @noRd
checkSwitch <- function(session, id, name, desc, status, qty){

    ns <- session$ns

    slider_id <- paste0('sld-', id)
    gear_qty_id <- paste0('gear-qty-', id)
    ###descCat <- substr(desc, 1, grep(' | ', desc))

    if(status == TRUE) {
        checkbox <- tags$input(class = "form-check-input", type = "checkbox",
            id = ns(slider_id), checked = 'checked'
        )
    } else {
        checkbox <- tags$input(class = "form-check-input", type = "checkbox",
            id = ns(slider_id)
        )
    }


    tags$div(class = "gear-check",
# TODO this class is ignored by display: block in parent divs. using style instead
        style = "display: flex; flex-wrap: nowrap; padding: .25rem;
            justify-content: space-between; align-items: center;",
        tags$div(class = "form-check form-switch",
            # tags$input(class = "form-check-input",
            #     type = "checkbox",
            #     id = ns(slider_id)
            # ),
            checkbox,
            tags$label(class = "form-check-label",
                `for` = ns(slider_id),
                p(tags$strong(name), " | ", desc)
            )
        ),
        tags$input(id = ns(gear_qty_id),
            type = "number",
            class = "form-control gear-input-qty",
            style = "width: 20%;",
            placeholder = "qty.",
            value = qty
        )
    )
}

