#####CARDS TEMPLATES#####
#' Meal card w image
#'
#'
#'
#' @noRd
mealCard2 <- function(session){
    ns <- session$ns

    div(class="card",
        tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1517760444937-f6397edcbbcd?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=42b2d9ae6feb9c4ff98b9133addfb698"),
        div(class = "card-body",
            h4(class="card-title","Special title treatment"),
            p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
        )#end card body
    )#end card


}






#' Meal card UI function
#' @description View and adjust Meal record info in a card.
#' @param session The shiny session
#' @param id The id to give the card div. Also, prepended with 'add-' to use as the button id.
#' @param ttl The card title displayed in h5()
#' @param subttl The card subtitle displayed in h6()
#' @param desc The card body text displayed in p()
#' @noRd
#'
mealCard <- function(session,id,data = LOCAL, mtype, ttl, subttl, desc){
    ns <- session$ns

    if(file.exists(paste0('./inst/app/www/assets/img/menu/',mtype,'-',id,'.jpg')) == TRUE){
        imgsrc <- paste0('www/assets/img/menu/',mtype,'-',id,'.jpg')
    } else {
        imgsrc <- 'www/assets/img/menu/default.jpg'
    }

    div(id = ns(id), class = "card card-block mx-2", style="min-width:300px; margin-bottom:10px;",
        tags$img(class="card-img-top", alt="100%x280" , src = imgsrc,
                 height = '300', width = '300'
        ),
        div(class = "card-body",style="min-width:300px;",
            h5(class = "card-title d-flex justify-content-between align-items-center", ttl,),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text', desc),
            div(style = 'display: inline-flex; justify-content: space-evenly; flex-wrap: wrap;',
                selectInput(inputId = ns(paste0('rd-',id)),
                            width = 'fit-content',
                            size = 1,
                            selectize = FALSE,
                            label = NULL,
                            choices = c('River Day',as.character(seq(1:30))),
                            selected = 'River Day'),
                selectInput(inputId = ns(paste0('mt-',id)),
                            width = 'fit-content',
                            size = 1,
                            selectize = FALSE,
                            label = NULL,
                            choices = c(unique(data$LU_MEAL_TYPE$MEAL_TYPE)),
                            selected = mtype),
                actionButton(inputId = ns(paste0('add-',id)),label = NULL, icon = icon('plus'),
                             type = "button",class = "btn btn-sm btn-success",
                             style = 'margin-left: 10px;'),
                # actionButton(inputId = ns(paste0('del-',id)),label = NULL, icon = icon('trash'),
                #              type = "button",class = "btn btn-sm btn-danger")
            ),
        )
    )
}

#' Ingredient card UI function
#'
#'
#'
#'
ingredientCard <- function(session,id, ttl, subttl, desc){
    ns <- session$ns
    div(id = ns(id), class='card',
        div(class='card-body',
            h5(class='card-title', ttl),
            h6(class='card-subtitle mb-2 text-muted', subttl),
            p(class='card-text', desc),
            actionButton(inputId = ns(paste0('ingEdit-',id)),label = NULL, icon = icon('pencil')),
            actionButton(inputId = ns(paste0('ingAdd-',id)),label = NULL, icon = icon('plus')),
            actionButton(inputId = ns(paste0('ingNew-',id)),label = NULL, icon = icon('check')),
            actionButton(inputId = ns(paste0('ingKill-',id)),label = NULL, icon = icon('trash'))
        )
    )
}



#####CARDS GENERATORS#####

#' Meal card generator function
#' @description The functional programming logic to generate cards, input objects,
#' and related observers.
#' @param session The shiny session
#' @param data Passing in the 'LOCAL' reactiveValues data object
#' @param mtype One of the meal types in LOCAL$LU_MEAL$MEAL_TYPE. Used to break
#' the cards out into accordion collapsible sections on the page.
#'
#' @noRd
makeMealCards <- function(input, output, session, mtype, data = LOCAL){
    LOCAL <- data

    renderUI({

        ns <- session$ns
        filtDat <- LOCAL$ALL_DATA %>% filter(MEAL_TYPE %in% mtype) %>%
            select(MEAL_ID,MEAL_TYPE,MEAL_NAME,MEAL_DESCRIPTION) %>% unique(.)
        mapIndexRows <- which(filtDat$MEAL_TYPE == mtype)

        # Make cards
        div(class = "container-fluid py-2",
            div(class = "d-flex flex-row flex-nowrap overflow-auto",

                map(mapIndexRows, ~ mealCard(session,filtDat[.,'MEAL_ID'],data = LOCAL,
                                             mtype = mtype, filtDat[.,'MEAL_NAME'],
                                             NULL, filtDat[.,'MEAL_DESCRIPTION']))
            )

        )

    })
}

#' Ingredient card generator
#' @description Map over ingredients by meal and make cards
#' @noRd
makeIngredientCards <- function(input, output, session, data, mealUniqueId){

    renderUI({
        ns <- session$ns
#browser()
        mapIngredientRows <- which(data$MEAL_UNIQUE_ID == mealUniqueId)

        map(mapIngredientRows, ~ ingredientCard(session,
                                    id = data[.,'INGREDIENT_ID'],
                                    ttl = data[.,'INGREDIENT'],
                                    subttl = data[.,'SERVING_SIZE_DESCRIPTION'],
                                    desc = data[.,'INGREDIENT_DESCRIPTION'])
        )
    })
}

#####ACCORDION GENERATORS#####


#' Accordion box generator
#' @param session the shiny session object
#' @param outer Single character string to be the ID of the accordion group
#' @param data Dataframe of all meal information for selected meals
#'
#' @noRd
makeMealBoxes <- function(session, outer,data){
    #browser()
    req(!is.null(data) == TRUE & (nrow(data) > 0) == TRUE)
    ns <- session$ns

    mealIDs <- data %>% pull(MEAL_UNIQUE_ID) %>% unique(.)

    firstDiv <- paste0("bs_accordion(ns('",outer,"')) %>% ")
    opts <- "bs_set_opts(panel_type = 'default', use_heading_link = TRUE) %>% "
    dummy <- "bs_append(title = NULL, content = NULL) %>%"

    secondDiv <- character()

    for(i in 1:length(mealIDs)){
        #browser()
        headerInfo <- data %>%
            filter(MEAL_UNIQUE_ID == mealIDs[i]) %>%
            #mutate(MEAL_BOX_HEADER = paste(MEAL_NAME,'|',MEAL_TYPE,'| River Day ',RIVER_DAY)) %>%
            mutate(MEAL_BOX_HEADER = paste('River Day ',RIVER_DAY,'|',MEAL_TYPE,'|',MEAL_NAME)) %>%
            pull(MEAL_BOX_HEADER) %>% unique(.)

        # This line should not have return characters
        str <- paste0("bs_append(title = '",headerInfo, "', content = makeIngredientCards(input, output, session, data = data, mealUniqueId = '",mealIDs[i],"'))")

        if(length(mealIDs) == 1){

            secondDiv <- paste(firstDiv, opts, dummy, str)

        } else

        if(mealIDs[i] == (mealIDs)[1]){

            secondDiv <- paste(firstDiv, opts, dummy, str," %>% ")

        } else

        if(mealIDs[i] == (mealIDs)[length(mealIDs)]){

            secondDiv <- paste(secondDiv, str)

        } else {

            secondDiv <- paste(secondDiv,str, " %>% ")
        }
    }

    return(eval(parse(text = secondDiv)))
}



#####NAVBAR#####
navbar <- tags$nav(class = "navbar navbar-expand-lg navbar-light bg-light",
         a(class = "navbar-brand",
           href = "#",
           "Navbar"),
         tags$button(class = "navbar-toggler",
                     type = "button",
                     `data-toggle` = "collapse",
                     `data-target` = "#navbarSupportedContent",
                     `aria-controls` = "navbarSupportedContent",
                     `aria-expanded` = "false",
                     `aria-label` = "Toggle navigation",
                     span(class = "navbar-toggler-icon")),
         tags$div(class = "collapse navbar-collapse",
                  id = "navbarSupportedContent",
                  tags$ul(class = "navbar-nav mr-auto",
                          tags$li(class = "nav-item active",
                                  tags$a(class = "nav-link",
                                         href = "#",
                                         "Home",
                                         span(class = "sr-only",
                                              "(current)"))),
                          tags$li(class = "nav-item",
                                  tags$a(class = "nav-link",
                                         href = "#",
                                         "Link")),
                          tags$li(class = "nav-item dropdown",
                                  tags$a(class = "nav-link dropdown-toggle",
                                         href = "#",
                                         id = "navbarDropdown",
                                         role = "button",
                                         `data-toggle` = "dropdown",
                                         `aria-haspopup` = "true",
                                         `aria-expanded` = "false",
                                         "Dropdown"),
                                  tags$div(class = "dropdown-menu",
                                           `aria-labelledby` = "navbarDropdown",
                                           tags$a(class = "dropdown-item",
                                                  href = "#",
                                                  "Action"),
                                           tags$a(class = "dropdown-item",
                                                  href = "#",
                                                  "Another action"),
                                           tags$div(class = "dropdown-divider"),
                                           tags$a(class = "dropdown-item",
                                                  href = "#",
                                                  "Something else here"))),
                          tags$li(class = "nav-item",
                                  tags$a(class = "nav-link disabled",
                                         href = "#",
                                         "Disabled"))),
                  tags$form(class = "form-inline my-2 my-lg-0",
                            tags$input(class = "form-control mr-sm-2",
                                       type = "search",
                                       placeholder = "Search",
                                       `aria-label` = "Search"),
                            tags$button(class = "btn btn-outline-success my-2 my-sm-0",
                                        type = "submit",
                                        "Search"))))


testCard <- HTML('

    <div class="card">
    <div class="card-header">
        <a data-toggle="collapse" href="#test-block" aria-expanded="true" aria-controls="test-block">
            card header
        </a>
    </div>
    <div id="test-block" class="collapse">
        <div class="card-block">
            card block
        </div>
    </div>
</div>

')












testCard2 <- HTML('

<div class="card" style="width: 18rem;">
    <div class="card-body">
        <h5 class="card-title">Card title</h5>
        <h6 class="card-subtitle mb-2 text-muted">Card subtitle</h6>
        <p class="card-text">Some quick example text to build on the card title and make up the bulk of the cards content.</p>
            <a href="#" class="card-link">Card link</a>
            <a href="#" class="card-link">Another link</a>
    </div>
</div>

')





accordionR <- renderUI({

    div(class = "accordion",
        id = "accordionExample",
        div(class = "accordion-item",
            h2(class = "accordion-header",
               id = "headingOne",
               tags$button(class = "accordion-button",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#collapseOne",
                      `aria-expanded` = "true",
                      `aria-controls` = "collapseOne",
                      "Accordion Item #1")),
            div(id = "collapseOne",
                class = "accordion-collapse collapse show",
                `aria-labelledby` = "headingOne",
                `data-bs-parent` = "#accordionExample",
                div(class = "accordion-body",
                    strong("This is the first items accordion body."),
                    "It is shown by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. Its also worth noting that just about any HTML can go within the",
                    code(".accordion-body"),
                    ", though the transition does limit overflow."))),
        div(class = "accordion-item",
            h2(class = "accordion-header",
               id = "headingTwo",
               tags$button(class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#collapseTwo",
                      `aria-expanded` = "false",
                      `aria-controls` = "collapseTwo",
                      "Accordion Item #2")),
            div(id = "collapseTwo",
                class = "accordion-collapse collapse",
                `aria-labelledby` = "headingTwo",
                `data-bs-parent` = "#accordionExample",
                div(class = "accordion-body",
                    strong("This is the second items accordion body."),
                    "It is hidden by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. Its also worth noting that just about any HTML can go within the",
                    code(".accordion-body"),
                    ", though the transition does limit overflow."))),
        div(class = "accordion-item",
            h2(class = "accordion-header",
               id = "headingThree",
               tags$button(class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#collapseThree",
                      `aria-expanded` = "false",
                      `aria-controls` = "collapseThree",
                      "Accordion Item #3")),
            div(id = "collapseThree",
                class = "accordion-collapse collapse",
                `aria-labelledby` = "headingThree",
                `data-bs-parent` = "#accordionExample",
                div(class = "accordion-body",
                    strong("This is the third items accordion body."),
                    "It is hidden by default, until the collapse plugin adds the appropriate classes that we use to style each element. These classes control the overall appearance, as well as the showing and hiding via CSS transitions. You can modify any of this with custom CSS or overriding our default variables. Its also worth noting that just about any HTML can go within the",
                    code(".accordion-body"),
                    ", though the transition does limit overflow."))))



})


carousel <- renderUI({


div(id = "carouselExampleDark",class = "carousel carousel-dark slide",`data-bs-ride` = "carousel",
    div(class = "carousel-indicators",
        tags$button(type = "button",`data-bs-target` = "#carouselExampleDark",`data-bs-slide-to` = "0",
            class = "active",`aria-current` = "true",`aria-label` = "Slide 1"),
        tags$button(type = "button",`data-bs-target` = "#carouselExampleDark",`data-bs-slide-to` = "1",
            `aria-label` = "Slide 2"),
        tags$button(type = "button",`data-bs-target` = "#carouselExampleDark",`data-bs-slide-to` = "2",
            `aria-label` = "Slide 3")),
    div(class = "carousel-inner",
        div(class = "carousel-item active",`data-bs-interval` = "10000",
            img(src = "www/assets/img/favicon.png",class = "d-block w-100",alt = "..."),
            div(class = "carousel-caption d-none d-md-block",
                h5("First slide label"),
                p("Some representative placeholder content for the first slide."))),
        div(class = "carousel-item active",`data-bs-interval` = "10000",
            img(src = "...",class = "d-block w-100",alt = "..."),
            div(class = "carousel-caption d-none d-md-block",
                h5("First slide label"),
                p("Some representative placeholder content for the first slide."))),
        div(class = "carousel-item",`data-bs-interval` = "2000",
            img(src = "...",class = "d-block w-100",alt = "..."),
            div(class = "carousel-caption d-none d-md-block",
                h5("Second slide label"),
                p("Some representative placeholder content for the second slide."))),
        div(class = "carousel-item",
            img(src = "...",class = "d-block w-100",alt = "..."),
            div(class = "carousel-caption d-none d-md-block",
                h5("Third slide label"),
                p("Some representative placeholder content for the third slide.")))),
    tags$button(class = "carousel-control-prev",type = "button",`data-bs-target` = "#carouselExampleDark",
        `data-bs-slide` = "prev",
        span(class = "carousel-control-prev-icon",`aria-hidden` = "true"),
        span(class = "visually-hidden","Previous")),
    tags$button(class = "carousel-control-next",type = "button",`data-bs-target` = "#carouselExampleDark",
        `data-bs-slide` = "next",
        span(class = "carousel-control-next-icon",`aria-hidden` = "true"),
        span(class = "visually-hidden","Next")))



})



carousel2 <- renderUI({

    div(id = "myCarousel",
        class = "carousel slide container",
        `data-bs-ride` = "carousel",
        div(class = "carousel-inner w-100",
            div(class = "carousel-item active",
                div(class = "row",
                    div(class = "col-md-4",
                        div(class = "card card-body",
                            img(class = "img-fluid", src = "https://via.placeholder.com/640x360?text=1")
                        )
                    ),
                    div(class = "col-md-4",
                        div(class = "card card-body",
                            img(class = "img-fluid", src = "https://via.placeholder.com/640x360?text=1")
                        )
                    ),
                    div(class = "col-md-4",
                        div(class = "card card-body",
                            img(class = "img-fluid", src = "https://via.placeholder.com/640x360?text=1")
                        )
                    )

                )#end row

            ),#end carousel-item

            div(class = "carousel-item",
                div(class="row",
                    div(class = "col-md-4",
                        div(class = "card card-body",
                            img(class = "img-fluid", src = "https://via.placeholder.com/640x360?text=2")
                        )
                    ),


                    div(class="col-md-4 mb-3",
                        div(class="card",
                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1517760444937-f6397edcbbcd?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=42b2d9ae6feb9c4ff98b9133addfb698"),
                            div(class = "card-body",
                                h4(class="card-title","Special title treatment"),
                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                            )#end card body
                        )#end card
                    ), #end col-md-4 mb-3

                    div(class="col-md-4 mb-3",
                        div(class="card",
                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1517760444937-f6397edcbbcd?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=42b2d9ae6feb9c4ff98b9133addfb698"),
                            div(class = "card-body",
                                h4(class="card-title","Special title treatment"),
                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                            )#end card body
                        )#end card
                    ), #end col-md-4 mb-3

                    div(class = "col-md-4",
                        div(class = "card card-body",
                            img(class = "img-fluid", src = "https://via.placeholder.com/640x360?text=2")
                        )
                    ),


                )#end row

            ),#end carousel item

            div(class = "carousel-item",
                div(class = "card-body",
                    h5(class = "card-title", "Card title"),
                    h6(class = "card-subtitle mb-2 text-muted","Card subtitle"),
                    p(class = "card-text", "Some quick example text to build on the card title and make up the bulk of the cards content."),
                    a(href = "#",class = "card-link","Card link"),
                    a(href = "#", class = "card-link","Another link")
                )
            ),
            div(class = "carousel-item",
                div(class = "col-md-3",
                    div(class = "card card-body",
                        img(class = "img-fluid",
                            src = "https://via.placeholder.com/640x360?text=3")))),
            div(class = "carousel-item",
                div(class = "col-md-3",
                    div(class = "card card-body",
                        img(class = "img-fluid",
                            src = "https://via.placeholder.com/640x360?text=4")))),
            div(class = "carousel-item",
                div(class = "col-md-3",
                    div(class = "card card-body",
                        img(class = "img-fluid",
                            src = "https://via.placeholder.com/640x360?text=5")))),
            div(class = "carousel-item",
                div(class = "col-md-3",
                    div(class = "card card-body",
                        img(class = "img-fluid",
                            src = "https://via.placeholder.com/640x360?text=6")))),
            div(class = "carousel-item",
                div(class = "col-md-3",
                    div(class = "card card-body",
                        img(class = "img-fluid",
                            src = "https://via.placeholder.com/640x360?text=7")))),
            div(class = "carousel-item",
                div(class = "col-md-3",
                    div(class = "card card-body",
                        img(class = "img-fluid",
                            src = "https://via.placeholder.com/640x360?text=8"))))),
        tags$button(class = "carousel-control-prev",
               type = "button",
               `data-bs-target` = "#myCarousel",
               `data-bs-slide` = "prev",
               span(class = "carousel-control-prev-icon",
                    `aria-hidden` = "true"),
               span(class = "visually-hidden",
                    "Previous")),
        tags$button(class = "carousel-control-next",
               type = "button",
               `data-bs-target` = "#myCarousel",
               `data-bs-slide` = "next",
               span(class = "carousel-control-next-icon",
                    `aria-hidden` = "true"),
               span(class = "visually-hidden",
                    "Next")))




})



carousel3 <- renderUI({

    tags$section(class="pt-5 pb-5",
        div(class = "container",
            div(class = "row",
                div(class = "col-6",
                    h3(class="mb-3", "Carousel cards title"),
                ),#end col-6
                div(class="col-6 text-right",
                    tags$a(class="btn btn-primary mb-3 mr-1", href="#carouselExampleIndicators2", role="button", `data-slide`="prev",
                        tags$i(class="fa fa-arrow-left")
                    ),#end a
                    tags$a(class="btn btn-primary mb-3", href="#carouselExampleIndicators2", role="button", `data-slide`="next",
                           tags$i(class="fa fa-arrow-right")
                    )#end a
                ),#end col-6 text-right
                div(class = "col-12",
                    div(id="carouselExampleIndicators2", class="carousel slide", `data-ride`="carousel",
                        div(class = "carousel inner",
                            div(class = "carousel-item active",
                                div(class = "row",

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1532781914607-2031eca2f00d?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=7c625ea379640da3ef2e24f20df7ce8d"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1517760444937-f6397edcbbcd?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=42b2d9ae6feb9c4ff98b9133addfb698"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1532712938310-34cb3982ef74?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=3d2e8a2039c06dd26db977fe6ac6186a"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3
                                )#end row
                            ),#end carousel-item (active)

                            div(class = "carousel-item",
                                div(class = "row",

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1532771098148-525cefe10c23?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=3f317c1f7a16116dec454fbc267dd8e4"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1532715088550-62f09305f765?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=ebadb044b374504ef8e81bdec4d0e840"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1506197603052-3cc9c3a201bd?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=0754ab085804ae8a3b562548e6b4aa2e"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3
                                )#end row
                            ),#end carousel-item

                            div(class = "carousel-item",
                                div(class = "row",

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1507525428034-b723cf961d3e?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=ee8417f0ea2a50d53a12665820b54e23"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1532777946373-b6783242f211?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=8ac55cf3a68785643998730839663129"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3

                                    div(class="col-md-4 mb-3",
                                        div(class="card",
                                            tags$img(class="img-fluid", alt="100%x280" ,src="https://images.unsplash.com/photo-1532763303805-529d595877c5?ixlib=rb-0.3.5&amp;q=80&amp;fm=jpg&amp;crop=entropy&amp;cs=tinysrgb&amp;w=1080&amp;fit=max&amp;ixid=eyJhcHBfaWQiOjMyMDc0fQ&amp;s=5ee4fd5d19b40f93eadb21871757eda6"),
                                            div(class = "card-body",
                                                h4(class="card-title","Special title treatment"),
                                                p(class="card-text", "With supporting text below as a natural lead-in to additional content.")
                                            )#end card body
                                        )#end card
                                    ), #end col-md-4 mb-3
                                )#end row
                            )#end carousel-item



                        )#end carousel inner
                    )#end carousel slide
                )#end col-12
            )#end row
        )#end container
    )#end section

})




carousel4 <- renderUI({

    div(class = "container-fluid py-2",
        h2(class = "font-weight-light",
           "Bootstrap 5 Horizontal Scrolling Cards with Flexbox"),
        div(class = "d-flex flex-row flex-nowrap overflow-auto",
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card"),
            div(class = "card card-block mx-2", style="min-width: 300px;",
                "Card")
            )
        )


})



mealRow <- function(content){

    div(class = "container-fluid py-2",
        div(class = "d-flex flex-row flex-nowrap overflow-auto",

            content

        )

    )


}

