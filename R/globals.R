# GLOBAL OBJECTS AND OPTIONS

# OPTIONS ----

options(auth0_config_file = "inst/app/_auth0.yml")

# FUNCTIONS ----

#' top of page alerts
#' @noRd
alerts <- function(){
    tagList(
        alertIcon(icon = 'info', type = 'primary',
            style = 'margin: 10px; border-left-width: 10px;', dismissible = TRUE,
            p(style = 'margin-top: 15px;', tags$strong('New!'),'You can now edit your personal meals at the base level.
                Look for the `pencil` icon on your meal cards under `Select Meals`.'
            )
        ),
        alertIcon(icon = 'info', type = 'primary',
            style = 'margin: 10px; border-left-width: 10px;', dismissible = TRUE,
            p(style = 'margin-top: 15px;', tags$strong('New!'),
                'Improved instructions on meal editing screens...
                Look for the `<Open...Instructions>` clickable links when
                editing/creating meals.'
            )
        )
    )

}

# Define globa variables or get warned during package check

utils::globalVariables(
    c("<<-",".","pageNav", "INGREDIENT_ID", "INGREDIENT_CATEGORY" ,
     "INGREDIENT" ,"INGREDIENT_DESCRIPTION", "SERVING_SIZE_DESCRIPTION",
     "SERVING_SIZE_FACTOR", "STORAGE_DESCRIPTION" ,'UNIQUE_ID',
     'RIVER_DAY','NO_PEOPLE','QUANTITY','REVISED','MEAL_ID','MEAL_NAME',
     'QUANTITY','uniqueDay_MT_MN','all_of','pagenav','HYP_TRIP_SIZE',
     'HYP_QUANTITY','LU_MEAL_TYPE','MEAL_DESCRIPTION','XREF_INGREDIENT',
     'LU_INGREDIENTS','MEAL_TYPE','UNIQUE_ID','UPUSER','UPTIME', 'dbURL',
     'FWD', 'BCK', 'DUPS', 'RANK','QTY','.','MEAL_UNIQUE_ID','LOCAL','MEAL_TYPE',
     'MEAL_ID','MEAL_NAME','MEAL_DESCRIPTION','RIVER_DAY','','MEAL_UNIQUE_ID',
     'INGREDIENT_UNIQUE_ID','MEAL_ADD_ID','MEAL_VIEW_ID','TRIP_ID','NO_PEOPLE_CALC',
     'USERNAME','TRIP_ID','TRIPNAME','NO_ADULTS','NO_KIDS','TRIP_DESC','<<-',
     'luMtypes','ttl','USERNAME','TOTAL','MEAL_COUNT','data','desc', 'USER_ID',
     'LU_GS_COLS', 'GS_INDEX', 'COL_INDEX','INSTRUCTIONS', 'TOOLS',
     'DB_IDS', 'MEAL_DEL_ID', 'MEAL_EDIT_ID', 'MEAL_TYPE_ID', 'RECORD_ID',
     'con', 'ingsToAdd', 'mealsToAdd', 'read_sheet', 'toDelete', 'xrefIngsToAdd',
     'xrefT','mealDelId'
    )
)


