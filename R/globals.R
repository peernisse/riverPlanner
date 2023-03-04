

# Options

options(auth0_config_file = "inst/app/_auth0.yml")

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
     'luMtypes','ttl','USERNAME','TOTAL','MEAL_COUNT','data','desc'
    )
)


