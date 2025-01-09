# EDIT ROOT MEAL INSTANCE MODULE mod_root_meal_edit

#' meal_edit UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @importFrom plyr round_any
#' @noRd
#mod_root_meal_edit_ui <- function(id, session){
mod_root_meal_edit_ui <- function(id, session){
    ns <- NS(id) #The session ns is meal- so we use NS(id) for rootEditMeal

    tagList(
        customModalDialog(

            h5('Meal Title/Description'),
            collapseInstructions(nmsp = ns, id = 'rootEditInst-1',
                ttl = '<Open Instructions>', icon = 'circle-info',
                p('This meal, and the ingredients within it that YOU CREATED/OWN can
                    be edited within this form. You can edit meal information, edit the
                    ROOT INSTANCE of ingredients that YOU OWN, and remove any ingredients
                    from the ROOT INSTANCE of this meal.'
                ),
                p('Change the meal type, title or description...')
            ),
            fluidRow(
                column(width = 12,
                    uiOutput(ns('mealTtl')),
                    uiOutput(ns('mealDesc')),
                    uiOutput(ns('mealType'))
                )
            ),
            h5('Ingredients/Multipliers', style = 'margin-top:20px;',),
            collapseInstructions(nmsp = ns, id = 'rootEditInst-2',
                ttl = '<Open Ingredient Instructions>', icon = 'circle-info',
                p(tags$strong('NOTE:'), 'DELETING AN INGREDIENT removes it from the ROOT INSTANCE of
                    this meal but DOES NOT DELETE the ROOT INSTANCE of the ingredient.'
                ),
                p(tags$strong('NOTE:'), 'EDITING AN INGREDIENT WILL CHANGE the ROOT INSTANCE of
                    that ingredient. CAUTION! -- Changes may not take effect until the app is reloaded.
                    To be safe, it is best to save, double check the meal in saved trips,
                    remove, and re-add any edited meals in your saved trips.'
                ),
                p('-- Edit any ROOT INSTANCE ingredient info. Use the calculator
                    to modify the ingredient multiplier. If no calculator is shown
                    with the ingredient, it is not yours to modify. But you can still
                    delete it from this ROOT INSTANCE of this meal, which you do own. --',
                    style = 'font-style: italic;'
                ),
                p( p(style = 'color: #5cb874; display: inline;', 'GREEN COLORED INGREDIENTS'),
                    style = 'font-style: italic;',
                    ' are yours to edit. When clicking
                    `Save` below, your changes will be saved to the ROOT INSTANCE of that
                    ingredient, but may not extend to instances of the ingredient elsewhere in your
                    saved trips. It is best to double check, remove, and re-add edited
                    meals within your trips after editing.'
                ),
                p('-- Click TRASH to remove an ingredient from this ROOT
                      INSTANCE meal. --',style = 'font-style: italic;'),
                p('-- To ADD a different ingredient, use the ingredient
                      picker below. --', style = 'font-style: italic;')
            ),
            fluidRow(#style = 'margin-top:20px;',
                column(width = 12,
                    uiOutput(ns('modalIngs')),
                    uiOutput(ns('modalIngsNoEdit')) # This is not being used 10/14/2023
                )
            ),
            hr(style = 'margin:20px;'),
            h5('Add Ingredient to this Meal', style = 'margin-top:20px;'),
            fluidRow(#style = 'margin-top:20px;',
                column(width = 12,
                    # h5(textOutput(ns('modalTitle2'))),

                    p('-- Start typing a word to filter the dropdown. Click + to add ingredient to this meal. --',
                        style = 'font-style: italic;'),
                    uiOutput(ns('modalSelIng'))
                )
            ),
            h5('Create New Ingredient', style = 'margin-top:20px;'),
            collapseInstructions(nmsp = ns, id = 'rootEditInst-3',
                ttl = '<Open Create Ingredient Instructions>', icon = 'circle-info',
                p('Below the input form in `Orange` allows you to create a new ingredient
                    for your profile to use again and again.'),
                p('Use the `Multiplier Calculator` to determine the best multiplier
                  for your ingredient and units.'),
                p('What you choose for Units is up to you. It may make sense to have
                    units of "ounces", or it might make sense to use units such as
                    "12 ounce can" or "Standard box of Zattaran\'s rice".'),
                p('-- Your new ingredient will be saved in the database when you click ` + `. --',
                  style = 'font-style: italic;'),
                p('--Your new ingredient will appear in the Add Ingredient dropdown above.--',
                  style = 'font-style: italic;'),
            ),
            fluidRow(#style = 'margin-top:20px;',
                column(width = 12,
                    uiOutput(ns('modalNewIng'))
                )
            ),
            # fluidRow(style = 'margin-top:20px;',
            #     column(width = 12,
            #         h5('Meal Notes'),
            #         uiOutput(ns('notes'))
            #     )
            # ),
            hr(style = 'margin:20px;'),
            fluidRow(style = 'margin-top:20px;',
                column(width = 12,
                    h5('Tools'),
                    helpText('Separate tools with semi-colons `;`'),
                    uiOutput(ns('tools'))
                )
            ),
            fluidRow(style = 'margin-top:20px;',
                column(width = 12,
                    h5('Instructions'),
                    helpText('Separate instructions steps with semi-colons `;`'),
                    uiOutput(ns('inst'))
                )
            ),
            fluidRow(style = 'margin-top:20px;',
                column(width = 12,
                    alertIcon(icon = 'exclamation-triangle', type = 'danger',
                            style = 'margin: 10px; border-left-width: 10px;', dismissible = FALSE,
                        p(style = 'margin-top: 15px;', tags$strong('DANGER ZONE:'),'Below you can
                            permanently delete this meal.'
                        ),
                        p(tags$strong('Meal Name:'), textOutput(ns('killMeal_Name'))),
                        textInput(ns('checkMeal_Name'), label = 'Confirm Delete:', width = 'auto',
                            placeholder = 'Enter Meal Name to Confirm Delete'
                        ),
                        actionButton(inputId = ns('confirmKillMeal'), label = 'Confirm Delete',
                            class = 'riv btn btn-danger'
                        )
                    )
                )
            ),
            session = session,
            displayXClose = TRUE,
            title = h4(textOutput(ns('modalTitle')), style = 'color: #5cb874; '),
            size = 'fs',
            easyClose = FALSE,
            fade = FALSE,
            footer = fluidRow(class = 'modal-footer-row',
                actionButton(ns('updateMeal'), label = 'Save', class = 'btn btn-success', class = 'riv'),
                actionButton(ns('editMealModalClose_2'), label = 'Cancel', class = 'btn btn-default',
                    class = 'riv', class = 'getstarted')
            )
        )
    )
}

#' meal_edit Server Functions
#' @description Server function for edit meal modal.
#' @noRd
mod_root_meal_edit_server <- function(id, data = LOCAL){
    moduleServer( id, function(input, output, session){
        ns <- session$ns
        LOCAL <- data
        createdObservers <- c()

        observe({

            if(LOCAL$rootEditMealModalSwitch == TRUE) {

                mealUniqueID <- LOCAL$rootEditMealMealUniqueID
                req(!length(mealUniqueID) > 1)

                LOCAL$rootEditMeal <- LOCAL$LU_MEAL %>%
                    filter(MEAL_ID %in% mealUniqueID)

                # NOTE: XREF_INGREDIENT$USER_ID is the ID of the user who created
                # the meal, NOT THE INGREDIENT!!! This means we cant use it
                # to tell if the ing is editable or not

                LOCAL$rootEditXrefIngs <- LOCAL$XREF_INGREDIENT %>%
                    filter(MEAL_ID %in% mealUniqueID)

                LOCAL$rootEditIngs <- LOCAL$LU_INGREDIENTS %>%
                    filter(INGREDIENT_ID %in% LOCAL$rootEditXrefIngs$INGREDIENT_ID) %>%
                    mutate(INGREDIENT_UNIQUE_ID = paste0(INGREDIENT_ID, '_', USER_ID))
            }
        })

        # Reactive updates in edit meal modal -----

        observe({

            if(LOCAL$rootEditMealModalSwitch == TRUE) {

                #####RENDER MODAL UI ELEMENTS#####
                # Modal titles -----

                ## Display titles ----

                output$modalTitle <- renderText({
                    paste('Editing Root Instance of:', LOCAL$rootEditMeal$MEAL_NAME)
                })

                # TODO 10/14/2023 This can probably go away
                # output$modalTitle2 <- renderText({
                #     paste('Editing Root Instance of:', LOCAL$rootEditMeal$MEAL_NAME)
                # })

                # Editable Modal Ingredient list -----

                ingredientUniqueIDs <- unique(LOCAL$rootEditIngs$INGREDIENT_UNIQUE_ID)

                output$modalIngs <- renderUI({
                    if(nrow(LOCAL$rootEditIngs) == 0){return(NULL)}
                    rows <- c(1:nrow(LOCAL$rootEditIngs))
                    map(rows, ~ rootEditMealIngredientInputs(input, output, session,
                        data = isolate(LOCAL$rootEditIngs[.x,]),
                        userID = isolate(LOCAL$userID))
                    )
                })

                # Modal rootEditMeal Add ingredients dropdown -----

                output$modalSelIng <- renderUI({
                    if(nrow(LOCAL$rootEditMeal) == 0){return(NULL)}
                    selectIngredients(input, output, session, data = LOCAL$LU_INGREDIENTS)
                })

                # Modal New Ingredient form -----

                output$modalNewIng <- renderUI({
                    if(nrow(LOCAL$rootEditMeal) == 0){return(NULL)}
                    createIngredients(input, output, session, data = isolate(LOCAL))
                })

                # Observe delete ingredient buttons -----

                delIngIDs <- paste0('del-ing-',ingredientUniqueIDs)
                delIngIDs <- delIngIDs[!delIngIDs %in% createdObservers]
                if(length(delIngIDs) > 0){
                    map(delIngIDs, ~ observeEvent(input[[.x]], {
                        rootEditMealDelIng(session, id = .x, data = LOCAL)
                    }, ignoreInit = TRUE, ignoreNULL = TRUE, autoDestroy = TRUE)
                    )
                    createdObservers <<- c(createdObservers,delIngIDs)
                }

                # Observe Edit Ingredient Buttons ----
                # TODO 10/14/2023 This can probably go away
                # editIngIDs <- paste0('edit-ing-',ingredientUniqueIDs)
                # editIngIDs <- editIngIDs[!editIngIDs %in% createdObservers]
                # if(length(editIngIDs) > 0){
                #     map(editIngIDs, ~ observeEvent(input[[.x]], {
                #         rootEditMealEditIng(input, session, id = .x, data = LOCAL)
                #     }, ignoreInit = TRUE, ignoreNULL = TRUE, autoDestroy = TRUE)
                #     )
                #     createdObservers <<- c(createdObservers,editIngIDs)
                # }

                # End of when modal is open code -----

            } # end if
        }) # end observe
        # ------------------------------------------------------------------------

        # Observe add ingredient button -----

        observeEvent(input[['addIngredient']], {
            #withProgress(message = 'Adding Ingredient', detail = 'saving to database...', {
                rootEditMealAddIng(input, output, session, data = LOCAL)
            #})
        }, ignoreInit = TRUE, ignoreNULL = TRUE)

        # Observe new ingredient quantity input and update multiplier -----

        observeEvent(input[['ing-new-qty']],{
            req(
                round(as.numeric(input[['ing-new-qty']])/as.numeric(input[['ing-new-hypPeople']]), 3) !=
                    as.numeric(input[['ing-new-ssf']]) || is.na(as.numeric(input[['ing-new-ssf']]))
            )
            req(LOCAL$rootEditMealModalSwitch == TRUE)
            req(nrow(LOCAL$rootEditIngs) > 0)
            noPeopleCalc <- as.numeric(input[['ing-new-hypPeople']])

            updateTextInput(session, inputId = 'ing-new-ssf',
                value = round(as.numeric(input[['ing-new-qty']]) / noPeopleCalc,3)
            )
        }, ignoreInit = TRUE)

        # Observe new hypothetical NoPeople input and update multiplier -----

        observeEvent(input[['ing-new-hypPeople']],{
            req(LOCAL$rootEditMealModalSwitch == TRUE)
            req(nrow(LOCAL$rootEditIngs) > 0)
            noPeopleCalc <- as.numeric(input[['ing-new-hypPeople']])
            ingQty <- if(is.na(as.numeric(input[['ing-new-qty']]))) {1} else {as.numeric(input[['ing-new-qty']])}

            updateTextInput(session, inputId = 'ing-new-ssf',
                value = round(ingQty / noPeopleCalc, 3)
            )

            updateTextInput(session, inputId = 'ing-new-qty', value = ingQty)
        }, ignoreInit = TRUE)

        # Observe new ingredient button -----

        observeEvent(input[['newIngredient']], {
            withProgress(message = 'New Ingredient', detail = 'saving to database...', {
                setProgress(0.5)
                newIngredientResponse(input, output, session, data = LOCAL)
                setProgress(0.9)
            })
        })

        # Root Update save meal button observe -----

        observeEvent(input$updateMeal,{

            req(nrow(LOCAL$rootEditMeal) == 1)
            req(nrow(LOCAL$rootEditXrefIngs) > 0)
            req(nrow(LOCAL$rootEditIngs) > 0)

            mealUniqueID <- LOCAL$rootEditMealMealUniqueID
            req(mealUniqueID %in% LOCAL$rootEditMeal$MEAL_ID)

            # Validate a new ingredient is not sitting in input
            if(input[['ing-new-ing']] != ''){
                showNotification('New Ingredient form has data!
                    Do you need to finish creating a new ingredient?',
                    type = 'error', duration = 10
                )
                return(NULL)
            }

            # Update LOCAL$rootEditMeal ----

            shinyjs::disable('updateMeal')

            ## Meal Name ----

            # CHECK ALL INPUTS AGAINST THE RESPECTIVE TABLES VALUES AND UPDATE ----
            if(LOCAL$rootEditMeal$MEAL_NAME != input$rootEditMealName) {
                LOCAL$rootEditMeal$MEAL_NAME <- input$rootEditMealName
            }

            ## Meal Description ----

            if(LOCAL$rootEditMeal$MEAL_DESCRIPTION != input$rootEditMealDesc) {
                LOCAL$rootEditMeal$MEAL_DESCRIPTION <- input$rootEditMealDesc
            }

            ## Meal Type and Meal Type ID ----

            if(LOCAL$rootEditMeal$MEAL_TYPE != input$rootEditMealType) {
                LOCAL$rootEditMeal$MEAL_TYPE <- input$rootEditMealType
                LOCAL$rootEditMeal$MEAL_TYPE_ID <- LOCAL$LU_MEAL_TYPE %>%
                    filter(MEAL_TYPE %in% input$rootEditMealType) %>%
                    pull(MEAL_TYPE_ID)
            }

            ## Tools ----

            if(LOCAL$rootEditMeal$TOOLS != input$`rootEdit-tools`) {
                LOCAL$rootEditMeal$TOOLS <- input$`rootEdit-tools`
            }

            ## Instructions ----

            if(LOCAL$rootEditMeal$INSTRUCTIONS != input$`rootEdit-inst`) {
                LOCAL$rootEditMeal$INSTRUCTIONS <- input$`rootEdit-inst`
            }

            ## Update LOCAL$rootEditIngs ----

            ids <- LOCAL$rootEditIngs$INGREDIENT_ID %>% unique()

            for(i in seq_along(ids)){
                row <- which(LOCAL$rootEditIngs$INGREDIENT_ID == ids[i])

                if(LOCAL$rootEditIngs$USER_ID[[row]] != LOCAL$userID) next

                id <- LOCAL$rootEditIngs$INGREDIENT_UNIQUE_ID[[row]]

                if(LOCAL$rootEditIngs$INGREDIENT[[row]] != input[[paste0('ing-ing-',id)]]){
                    LOCAL$rootEditIngs$INGREDIENT[[row]] <- input[[paste0('ing-ing-',id)]]
                }

                if(LOCAL$rootEditIngs$INGREDIENT_DESCRIPTION[[row]] != input[[paste0('ing-desc-',id)]]){
                    LOCAL$rootEditIngs$INGREDIENT_DESCRIPTION[[row]] <- input[[paste0('ing-desc-',id)]]
                }

                if(LOCAL$rootEditIngs$SERVING_SIZE_DESCRIPTION[[row]] != input[[paste0('ing-unit-',id)]]){
                    LOCAL$rootEditIngs$SERVING_SIZE_DESCRIPTION[[row]] <- input[[paste0('ing-unit-',id)]]
                }

                if(LOCAL$rootEditIngs$SERVING_SIZE_FACTOR[[row]] != as.numeric(input[[paste0('ing-ssf-',id)]])){
                    LOCAL$rootEditIngs$SERVING_SIZE_FACTOR[[row]] <- as.numeric(input[[paste0('ing-ssf-',id)]])
                }
            }

            ## Update LOCAL$rootEditXrefIngs ----

            newXref <- LOCAL$rootEditIngs %>%
                select(INGREDIENT_ID, INGREDIENT) %>%
                mutate(
                    MEAL_ID = LOCAL$rootEditMeal$MEAL_ID,
                    MEAL_NAME = LOCAL$rootEditMeal$MEAL_NAME,
                    UPTIME = Sys.Date(),
                    UPUSER = LOCAL$userName,
                    USER_ID = LOCAL$userID
                ) %>%
                select(names(LOCAL$rootEditXrefIngs))

            req(identical(names(newXref), names(LOCAL$rootEditXrefIngs)))

            LOCAL$rootEditXrefIngs <- newXref

            # UPDATE DB ----

            req(nrow(LOCAL$rootEditMeal) == 1)
            req(LOCAL$rootEditMeal$USER_ID == LOCAL$userID)

            lu_meal <- isolate(LOCAL$LU_MEAL) %>%
                filter(!MEAL_ID %in% LOCAL$rootEditMeal$MEAL_ID) %>%
                bind_rows(LOCAL$rootEditMeal)

            LOCAL$LU_MEAL <- lu_meal

            lu_ingredients <- isolate(LOCAL$LU_INGREDIENTS) %>%
                filter(!INGREDIENT_ID %in% LOCAL$rootEditIngs$INGREDIENT_ID) %>%
                bind_rows(LOCAL$rootEditIngs) %>%
                select(-INGREDIENT_UNIQUE_ID)

            LOCAL$LU_INGREDIENTS <- lu_ingredients

            xref_ingredient <- isolate(LOCAL$XREF_INGREDIENT) %>%
                filter(!MEAL_ID %in% LOCAL$rootEditMeal$MEAL_ID) %>%
                bind_rows(LOCAL$rootEditXrefIngs)

            LOCAL$XREF_INGREDIENT <- xref_ingredient

            withProgress(message = paste('Updating meal', LOCAL$rootEditMeal$MEAL_NAME),
                detail = 'saving to database...', {
                    incProgress(.25, 'Contacting database...')

                    con <- rivConnect()
#TODO 11/19/2023 should the row arg be something other than hard coded "1"?
                        upsertLuMeal(con = con, from = LOCAL$rootEditMeal, data = LOCAL, row = 1)

                        updateIngs <- LOCAL$rootEditIngs %>% filter(USER_ID %in% LOCAL$userID)
                        if(nrow(updateIngs) > 0){
                            map(1:nrow(updateIngs), ~ upsertLuIngredients(con = con,
                                from = updateIngs, data = LOCAL, row = .x)
                            )
                        }

                        incProgress(.5, 'Querying database...')

                        ## Remove any ingredients in xref_ingredient for this meal
                        ## in case an ingredient has been deleted from the root meal

                        mealId <- unique(LOCAL$rootEditXrefIngs$MEAL_ID)
                        req(length(mealId) == 1)
                        xrefIng <- LOCAL$rootEditXrefIngs

                        dbExecute(con, "start transaction;")
                        dbXrefIng <- dbGetQuery(con, paste0("SELECT * FROM xref_ingredient WHERE USER_ID = ",
                            LOCAL$userID," AND MEAL_ID = ", mealId,";"))
                        dbExecute(con,"commit;")

                        # Check for records that have been deleted locally and delete from DB

                        toDelete <- anti_join(dbXrefIng,xrefIng,
                            by = c('MEAL_ID','INGREDIENT_ID')
                        )

                        if(nrow(toDelete) > 0){
                            req(unique(toDelete$USER_ID) == LOCAL$userID)
                            map(1:nrow(toDelete), ~
                                delIngXrefIng(con = con,
                                    mealId = toDelete$MEAL_ID[[.x]],
                                    ingId = toDelete$INGREDIENT_ID[[.x]],
                                    userId = toDelete$USER_ID[[.x]]
                                )
                            )
                        }

                        ## Upsert xref_ingredient AFTER any deletetions

                        map(1:nrow(LOCAL$rootEditXrefIngs), ~ upsertXrefIng(con = con,
                            from = LOCAL$rootEditXrefIngs, data = LOCAL, row = .x)
                        )

                        refreshLOCAL(con = con, data = LOCAL, tables = c('LU_MEAL', 'LU_INGREDIENTS', 'XREF_INGREDIENT'))

                    dbDisconnect(con)

                incProgress(.99, 'Done')
            })

            # CLEAN UP ROOT EDIT TABLES ----

# TODO 11/17/2023 can this prevent modal re open? no
#LOCAL$rootEditMealButton <- FALSE


            LOCAL$rootEditMeal <- data.frame()
            LOCAL$rootEditIngs <- data.frame()
            LOCAL$rootEditXrefIngs <- data.frame()
            LOCAL$rootEditMealModalSwitch <- FALSE
            LOCAL$rootEditMealMealUniqueID <- NULL

            # CLOSE MODAL ----

            shinyjs::enable('updateMeal')
            removeModal()

            # NOTIFY ----

            showNotification(paste(LOCAL$rootEditMeal$MEAL_NAME[1], 'ROOT INSTANCE Updated!'),type = 'message')

        })

        # Cancel edit meal modal button footer -----

        observeEvent(input$editMealModalClose_2,{
            LOCAL$rootEditMealModalSwitch <- FALSE
            LOCAL$rootEditMealMealUniqueID <- NULL
            LOCAL$rootEditMeal <- data.frame()
            LOCAL$rootEditIngs <- data.frame()
            LOCAL$rootEditXrefIngs <- data.frame()
            removeModal()
        })

        # Text Input Fields -----
        observe({

            ## Editable Meal Name ----

            output$mealTtl <- renderUI({
                tagList(
                    div(class = "input-group", class = 'create-meal', style ='margin-top: 12px;',
                        tags$span(class = "input-group-text", 'Edit Meal Name',
                            style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;'),
                        tags$input(id = ns('rootEditMealName'),
                            placeholder = 'Meal Name', type = "text",
                            `aria-label` = "Meal Name", class = "form-control",
                            value = LOCAL$rootEditMeal$MEAL_NAME
                            #value = isolate(LOCAL$rootEditMeal$MEAL_NAME)
                        )
                    )
                )
            })

            ## Editable Meal Description ----

            output$mealDesc <- renderUI({
                tagList(
                    div(class = "input-group", class = 'create-meal', style ='margin-top: 12px; margin-bottom: 12px;',
                        tags$span(class = "input-group-text", 'Edit Meal Description',
                            style = 'background-color: #5cb874; border-color: #5cb874; color: #fff;'),
                        tags$input(id = ns('rootEditMealDesc'),
                            placeholder = 'Meal Description', type = "text",
                            `aria-label` = "Meal Description", class = "form-control",
                            value = LOCAL$rootEditMeal$MEAL_DESCRIPTION
                        )
                    )
                )
            })

            ## Editable Meal Type ----

            output$mealType <- renderUI({

                # TODO 11/17/2023 I put isolate around choices and selected here to prevent reload modal
                customSelectInput(inputId = ns('rootEditMealType'), label = 'Edit Meal Type',
                    labelColor = '#5CB874', labelTextColor = '#fff',
                    choices = isolate(LOCAL$LU_MEAL_TYPE$MEAL_TYPE),
                    selected = isolate(LOCAL$rootEditMeal$MEAL_TYPE),
                    disabled = NULL
                )
            })


            ## Editable Meal notes ----
# TODO 11/17/2023 These are stored in xref trips so have no place being in this modal
            # output$notes <- renderUI({
            #     ns <- session$ns
            #     customTextAreaInput(inputId = ns('rootEdit-notes'),
            #         label = 'Meal Notes',
            #         value = unique(LOCAL$rootEditMeal$MEAL_NOTES),
            #         labelColor = '#5cb874', width = '100%', height = NULL, cols = NULL,
            #         rows = NULL, placeholder = NULL, resize = 'vertical',
            #         labelTextColor = '#fff', disabled = NULL)
            # })

            ## Editable Tools ----

            output$tools <- renderUI({
                ns <- session$ns
                customTextAreaInput(inputId = ns('rootEdit-tools'),
                    label = 'Tools',
                    value = LOCAL$rootEditMeal$TOOLS %>% unique() %>% gsub('; ',';',.),
                    labelColor = '#5cb874', width = '100%', height = NULL, cols = NULL,
                    rows = NULL, placeholder = NULL, resize = 'vertical',
                    labelTextColor = '#fff', disabled = NULL)
            })

            ## Editable Instructions ----

            output$inst <- renderUI({
                ns <- session$ns
                customTextAreaInput(inputId = ns('rootEdit-inst'),
                    label = 'Instructions',
                    value = LOCAL$rootEditMeal$INSTRUCTIONS %>% unique() %>% gsub('; ',';',.),
                    labelColor = '#5cb874', width = '100%', height = NULL, cols = NULL,
                    rows = NULL, placeholder = NULL, resize = 'vertical',
                    labelTextColor = '#fff', disabled = NULL)
            })

            ## Kill Meal Display Name ----

            output$killMeal_Name <- renderText(LOCAL$rootEditMeal$MEAL_NAME)

        }) # end observe

        ## Kill Meal Confirm Button ----

        observeEvent(input$confirmKillMeal, {

            req(!is.null(input$checkMeal_Name) || input$checkMeal_Name != '')
            if(!input$checkMeal_Name == LOCAL$rootEditMeal$MEAL_NAME) {
                showNotification('Names do not match...', type = 'error')
                return(NULL)
            }

            if(input$checkMeal_Name == LOCAL$rootEditMeal$MEAL_NAME) {


                # get the meal id to be deleted from LOCAL
                killMealId <- isolate(LOCAL$rootEditMeal$MEAL_ID)

                # run rootDeleteMeal
                rootDeleteMeal(session, rvObj = LOCAL, mealId = killMealId)

                #showNotification(paste(input$checkMeal_Name,'ROOT INSTANCE has been deleted!'), type = 'message')
                #return(NULL) # TODO 10/14/2023 This is where to put rootDeleteMeal()
            }

        })

        #####RETURN LOCAL DATA OBJECT#####
        return(LOCAL)

        #####END MODULE SERVER#####

    })
}
