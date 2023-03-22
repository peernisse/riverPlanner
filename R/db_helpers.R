#Helper functions and dev items while transferring to a database

#'rivConnect
#' @description Function opens MariaDB connection to mySQL database
#' @noRd
rivConnect <- function(){
    # DBI::dbConnect(RMariaDB::MariaDB(), dbname = "mydb",
    #     username = 'peernisse', password = '@5thStreet',
    #     host = 'SEDOH-L01P-2TCD', port = 3306
    # )

    DBI::dbConnect(RMariaDB::MariaDB(),
       dbname = "rpdb",
       username = Sys.getenv("RPD_USER"),
       password = Sys.getenv("RPD_PWD"),
       host = Sys.getenv("RPD_HOST"),
       port = 3306
    )
}

#'reserveNewUserID
#' @description Reserves new USER_ID from lu_users.
#' @param userName The userName provided by Authentication client.
#' Used to determine new USER_ID when adding new user.
#' @returns The newly reserved USER_ID
#' @noRd
reserveNewUserID <- function(con, userName){
    # Reserve new ID within same transaction
    newid <- dbGetQuery(con,
        "select max(USER_ID) + 1 as USER_ID from lu_users;") %>%
        pull() %>%
        as.integer(.)

    dbExecute(con, paste0("insert into lu_users (USER_ID, USERNAME, EMAIL,
                UPTIME, UPUSER) VALUES (",newid,",'",userName,
                "', '", userName,"', now(), 'riverplanner@system.com');")
              )

    return(newid)
}

#'getMaxTripID
#' @description Gets the maximum TRIP_ID from LU_TRIPS.
#' Used to determine new TRIP_ID when starting new trip.
#' @noRd
getMaxTripID <- function(){
    con <- rivConnect()
    dbExecute(con, "start transaction;")
    maxDbTripID <- dbGetQuery(con, 'SELECT max(TRIP_ID) from lu_trips;')
    dbExecute(con,"commit;")
    dbDisconnect(con)
    return(as.integer(maxDbTripID))
}

#' getMaxIngID
#' @description Gets the maximum INGREDIENT_ID from LU_INGREDIENTS.
#' @noRd
getMaxIngID <- function(){
    con <- rivConnect()
    dbExecute(con, "start transaction;")
    maxDbIngpID <- dbGetQuery(con, 'SELECT max(INGREDIENT_ID) from lu_ingredients;')
    dbExecute(con,"commit;")
    dbDisconnect(con)
    return(as.integer(maxDbIngpID))
}

#' getMaxMealID
#' @description Gets the maximum MEAL_ID from lu_meal.
#' @noRd
getMaxMealID <- function(){
    con <- rivConnect()
    dbExecute(con, "start transaction;")
    maxDbMealID <- dbGetQuery(con, 'SELECT max(MEAL_ID) from lu_meal;') %>% pull()
    maxDbXrefMealID <- dbGetQuery(con, 'SELECT max(MEAL_ID) from xref_ingredient;') %>% pull()
    dbExecute(con,"commit;")
    maxDbMealID <- max(c(maxDbMealID,maxDbXrefMealID))
    dbDisconnect(con)
    return(as.integer(maxDbMealID))
}

#' Save Trip
#' @description Saves trip to LU_TRIPS. Sets UI to loaded trip, used when trip info changes
#' @param input,output,session The shiny session objects
#' @param data List. The LOCAL reactiveValues object is intended to be passed to saveTrip
#' @noRd
saveTrip <- function(input, output, session, data){

    ns <- session$ns
    LOCAL <- data
    # Validate User ID exists and there are data to save
    req(!is.null(LOCAL$userName) & LOCAL$userName != '' & length(LOCAL$userName) > 0)
    req(nrow(LOCAL$myMeals) > 0)
    #req(!is.null(id) & id != '' & length(id) > 0)

    # Determine if ingredient re-calculation needs to be done
    if(input$noAdults != LOCAL$noAdults | input$noKids != LOCAL$noKids){

        # Update calculations in LOCAL$myMeals
        LOCAL$myMeals$NO_ADULTS <- as.numeric(input$noAdults)
        LOCAL$myMeals$NO_KIDS <- as.numeric(input$noKids)
        LOCAL$myMeals$NO_PEOPLE_CALC <- ceiling(as.numeric(LOCAL$myMeals$NO_ADULTS) + (as.numeric(LOCAL$myMeals$NO_KIDS) * .65))
        #LOCAL$myMeals$QTY <- round(LOCAL$myMeals$NO_PEOPLE_CALC * LOCAL$myMeals$SERVING_SIZE_FACTOR)

        LOCAL$myMeals <- isolate(LOCAL$myMeals) %>%
            mutate(
                QTY = case_when(
                    NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                        round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                    TRUE ~ ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
                )
            )

        # Update calculations in LOCAL$LU_TRIPS placeholder
        # TODO this might not be needed so I did not code it. LU_TRIPS already gets replaced in dbUpdate()

    }

    # Update LOCAL reactive session trip values

    LOCAL$tripName <- input$tripName
    LOCAL$tripDesc <- input$tripDesc
    LOCAL$noAdults <- as.numeric(input$noAdults)
    LOCAL$noKids <- ifelse(input$noKids == '' | input$noKids == 'No. People Age <12',0,as.numeric(input$noKids))
    LOCAL$noPeople <- as.numeric(LOCAL$noAdults) + as.numeric(LOCAL$noKids)
    LOCAL$noPeopleCalc <- ceiling(as.numeric(LOCAL$noAdults) + (as.numeric(LOCAL$noKids) * .65))

    # Update LOCAL$myMeals trip info

    LOCAL$myMeals$TRIP_ID <- LOCAL$tripID
    LOCAL$myMeals$TRIPNAME <- LOCAL$tripName
    LOCAL$myMeals$UPTIME <- Sys.Date()
    LOCAL$myMeals$UPUSER <- LOCAL$userName
    LOCAL$myMeals$TRIP_DESC <- LOCAL$tripDesc
    LOCAL$myMeals$USERNAME <- LOCAL$userName

    # Save trip to database
    con <- rivConnect()
        dbUpdate(con = con, LOCAL$myMeals, 'LU_TRIPS', data = LOCAL)
    dbDisconnect(con)
    # Notify save
    showNotification(paste0('Trip ', LOCAL$tripName,' saved!'),
                     type = 'message', duration = 10)
}

#' upsertXrefTrips
#' @description Updates or inserts records to xref_trips.
#' This function is part of the save operation. Made to be used in a loop
#' @param con The database connection object
#' @param from The LOCAL$myMEals dataframe which contains all trip meal info
#' @param data List. The LOCAL reactiveValues object is intended to be passed to saveTrip
#' @param row The row number of the `from` dataframe being looped
#' @noRd
upsertXrefTrips <- function(con = con, from = xrefT, data = LOCAL, row){
    LOCAL <- data
    i <- row

    recordID <- from[i,] %>%
        mutate(RECORD_ID = paste(MEAL_ID,RIVER_DAY,INGREDIENT_ID,TRIP_ID,MEAL_TYPE_ID, sep = "_")) %>%
        select(RECORD_ID) %>%
        pull(.)

    dbIDs <- dbGetQuery(con,paste0("SELECT * from xref_trips WHERE USER_ID =",
        LOCAL$userID," AND trip_id ='",LOCAL$tripID,"';")) %>%
        mutate(DB_IDS = paste(MEAL_ID,RIVER_DAY,INGREDIENT_ID,TRIP_ID,MEAL_TYPE_ID, sep = "_")) %>%
        select(DB_IDS) %>%
        pull(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){
        dbExecute(con,
          paste0("UPDATE xref_trips SET MEAL_ID = '",from$MEAL_ID[i],"',",
                 "`RIVER_DAY` = '",from$RIVER_DAY[i],"',",
                 "`INGREDIENT_ID` = '",from$INGREDIENT_ID[i],"',",
                 "`TRIP_ID` = '",from$TRIP_ID[i],"',",
                 "`MEAL_TYPE_ID` = '",from$MEAL_TYPE_ID[i],"',",
                 "`MEAL_NOTES` = '",from$MEAL_NOTES[i],"',",
                 "`NO_ADULTS` = '",from$NO_ADULTS[i],"',",
                 "`NO_KIDS` = '",from$NO_KIDS[i],"',",
                 "`NO_PEOPLE_CALC` = '",from$NO_PEOPLE_CALC[i],"',",
                 "`SERVING_SIZE_FACTOR` = '",from$SERVING_SIZE_FACTOR[i],"',",
                 "`USER_ID` = '",LOCAL$userID,"',",
                 "`UPTIME` = '",Sys.Date(),"',",
                 "`UPUSER` = '",LOCAL$userName,"'",
                 " WHERE ",
                 "`MEAL_ID` = '",from$MEAL_ID[i],"' AND ",
                 "`RIVER_DAY` = '",from$RIVER_DAY[i],"' AND ",
                 "`INGREDIENT_ID` = '",from$INGREDIENT_ID[i],"' AND ",
                 "`TRIP_ID` = '",from$TRIP_ID[i],"' AND ",
                 "`MEAL_TYPE_ID` = '",from$MEAL_TYPE_ID[i],"' AND ",
                 "`USER_ID` = '",from$USER_ID[i],"';"
          )
        )
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){
        dbExecute(con,
            paste0("INSERT INTO xref_trips (",
                "`MEAL_ID`,`RIVER_DAY`,`INGREDIENT_ID`,`TRIP_ID`,",
                "`MEAL_TYPE_ID`,`MEAL_NOTES`,`NO_ADULTS`,`NO_KIDS`,",
                "`NO_PEOPLE_CALC`,`SERVING_SIZE_FACTOR`,`USER_ID`,`UPTIME`,`UPUSER`) VALUES ('",
                from$MEAL_ID[i],"','",
                from$RIVER_DAY[i],"','",
                from$INGREDIENT_ID[i],"','",
                from$TRIP_ID[i],"','",
                from$MEAL_TYPE_ID[i],"','",
                from$MEAL_NOTES[i],"','",
                from$NO_ADULTS[i],"','",
                from$NO_KIDS[i],"','",
                from$NO_PEOPLE_CALC[i],"','",
                from$SERVING_SIZE_FACTOR[i],"','",
                LOCAL$userID,"','",
                Sys.Date(),"','",
                LOCAL$userName,"');"
            )
        )
    }
    dbExecute(con,"commit;")
}

#' deleteXrefTrips
#' @description Deletes one record from xref_trips.
#' Designed to be used in a loop or with map() to loop through
#' rows of a dataframe of records to delete from the database
#' @param con The database connection object
#' @param from A dataframe of records that are in the DB but have been removed from
#' the local session LOCAL$myMEals dataframe. Obtained with anti_join before calling this function
#' @param data The LOCAL reactive values data onject
#' @param row The row number of the `from` dataframe being looped
#' @noRd
deleteXrefTrips <- function(con = con, from = toDelete, data = LOCAL, row){
    LOCAL <- data

    i <- row
    df <- from[i,]
    dbExecute(con, "start transaction;")
        dbExecute(con, paste0("DELETE FROM xref_trips WHERE ",
            "USER_ID = ",LOCAL$userID, " AND ",
            "MEAL_ID = ",df$MEAL_ID, " AND ",
            "RIVER_DAY = ",df$RIVER_DAY, " AND ",
            "INGREDIENT_ID = ",df$INGREDIENT_ID, " AND ",
            "TRIP_ID = ",df$TRIP_ID, " AND ",
            "MEAL_TYPE_ID = ",df$MEAL_TYPE_ID, ";"
            )
        )
    dbExecute(con,"commit;")
}

#' deleteLuTrips
#' @description Deletes one record from lu_trips
#' @param con The database connection object
#' @param id The trip ID to delete
#' @param data The LOCAL reactive values data object
#' @noRd
deleteLuTrips <- function(con, id, data){
    LOCAL <- data

    dbExecute(con, "start transaction;")
        dbExecute(con, paste0("DELETE FROM lu_trips WHERE TRIP_ID = ", id,
                " AND USER_ID = ", LOCAL$userID, ";"
            )
        )
    dbExecute(con,"commit;")
}

#' upsertLuIngredients
#' @description Made to be used with map(). Inserts or updates lu_ingredients records in a loop.
#' @param con The database connection object
#' @param from The dataframe to upsert records from
#' @param data The LOCAL reactive values data object
#' @param row The iterator. The row of the `from` dataframe being evaluated.
#' This is the .x object within map
#' @noRd
upsertLuIngredients <- function(con = con, from = ingsToAdd, data = LOCAL, row){
    LOCAL <- data
    i <- row

    recordID <- from[i,] %>%
        mutate(RECORD_ID = INGREDIENT_ID) %>%
        select(RECORD_ID) %>%
        pull(.)

    dbIDs <- dbGetQuery(con, "SELECT INGREDIENT_ID FROM lu_ingredients;") %>%
        mutate(DB_IDS = INGREDIENT_ID) %>%
        select(DB_IDS) %>%
        pull(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){
        dbExecute(con,
            paste0("UPDATE lu_ingredients SET INGREDIENT_ID = '",from$INGREDIENT_ID[i],"',",
                 "`INGREDIENT_CATEGORY` = '",from$INGREDIENT_CATEGORY[i],"',",
                 "`INGREDIENT` = '",from$INGREDIENT[i],"',",
                 "`INGREDIENT_DESCRIPTION` = '",from$INGREDIENT_DESCRIPTION[i],"',",
                 "`SERVING_SIZE_DESCRIPTION` = '",from$SERVING_SIZE_DESCRIPTION[i],"',",
                 "`SERVING_SIZE_FACTOR` = '",from$SERVING_SIZE_FACTOR[i],"',",
                 "`STORAGE_DESCRIPTION` = '",from$STORAGE_DESCRIPTION[i],"',",
                 "`USER_ID` = '",LOCAL$userID,"',",
                 "`UPTIME` = '",Sys.Date(),"',",
                 "`UPUSER` = '",LOCAL$userName,"'",
                 " WHERE ",
                 "`INGREDIENT_ID` = '",from$INGREDIENT_ID[i],"';"
            )
        )
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){
        dbExecute(con,
            paste0("INSERT INTO lu_ingredients (",
                 "`INGREDIENT_ID`,`INGREDIENT_CATEGORY`,`INGREDIENT`,`INGREDIENT_DESCRIPTION`,",
                 "`SERVING_SIZE_DESCRIPTION`,`SERVING_SIZE_FACTOR`,`STORAGE_DESCRIPTION`,`USER_ID`,`UPTIME`,`UPUSER`) VALUES ('",
                 from$INGREDIENT_ID[i],"','",
                 from$INGREDIENT_CATEGORY[i],"','",
                 from$INGREDIENT[i],"','",
                 from$INGREDIENT_DESCRIPTION[i],"','",
                 from$SERVING_SIZE_DESCRIPTION[i],"','",
                 from$SERVING_SIZE_FACTOR[i],"','",
                 from$STORAGE_DESCRIPTION[i],"','",
                 LOCAL$userID,"','",
                 Sys.Date(),"','",
                 LOCAL$userName,"');"
            )
        )
    }
    dbExecute(con,"commit;")
}

#' upsertXrefIng
#' @description Made to be used with map(). Inserts or updates xref_ingredient records in a loop.
#' @param con The database connection object
#' @param from The dataframe to upsert records from
#' @param data The LOCAL reactive values data object
#' @param row The iterator. The row of the `from` dataframe being evaluated.
#' This is the .x object within map
#' @noRd
upsertXrefIng <- function(con = con, from = xrefIngsToAdd, data = LOCAL, row){
    LOCAL <- data
    i <- row

    recordID <- from[i,] %>%
        mutate(RECORD_ID = paste(INGREDIENT_ID, MEAL_ID, sep = "_")) %>%
        select(RECORD_ID) %>%
        pull(.)

    dbIDs <- dbGetQuery(con,paste0("SELECT INGREDIENT_ID, MEAL_ID from xref_ingredient",
            " WHERE USER_ID = '",LOCAL$userID,"';")
        ) %>%
        mutate(DB_IDS = paste(INGREDIENT_ID, MEAL_ID, sep = "_")) %>%
        select(DB_IDS) %>%
        pull(.) %>%
        unique(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){
        dbExecute(con,
            paste0("UPDATE xref_ingredient SET INGREDIENT_ID = '",from$INGREDIENT_ID[i],"',",
                 "`MEAL_ID` = '",from$MEAL_ID[i],"',",
                 "`INGREDIENT` = '",from$INGREDIENT[i],"',",
                 "`MEAL_NAME` = '",from$MEAL_NAME[i],"',",
                 "`UPTIME` = '",Sys.Date(),"',",
                 "`UPUSER` = '",LOCAL$userName,"',",
                 "`USER_ID` = '",LOCAL$userID,"'",
                 " WHERE ",
                 "`MEAL_ID` = '",from$MEAL_ID[i],"'",
                 " AND INGREDIENT_ID = '", from$INGREDIENT_ID[i],"';"
            )
        )
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){
        dbExecute(con,
            paste0("INSERT INTO xref_ingredient (",
                 "`INGREDIENT_ID`,`MEAL_ID`,`INGREDIENT`,`MEAL_NAME`,",
                 "`UPTIME`,`UPUSER`,`USER_ID`) VALUES ('",
                 from$INGREDIENT_ID[i],"','",
                 from$MEAL_ID[i],"','",
                 from$INGREDIENT[i],"','",
                 from$MEAL_NAME[i],"','",
                 Sys.Date(),"','",
                 LOCAL$userName,"','",
                 LOCAL$userID,"');"
            )
        )
    }
    dbExecute(con,"commit;")
}

#' upsertLuMeal
#' @description Made to be used with map(). Inserts or updates lu_meal records in a loop.
#' @param con The database connection object
#' @param from The dataframe to upsert records from
#' @param data The LOCAL reactive values data object
#' @param row The iterator. The row of the `from` dataframe being evaluated.
#' This is the .x object within map
#' @noRd
upsertLuMeal <- function(con = con, from = mealsToAdd, data = LOCAL, row){
    LOCAL <- data
    i <- row

    recordID <- from[i,] %>%
        mutate(RECORD_ID = MEAL_ID) %>%
        select(RECORD_ID) %>%
        pull(.)

    dbIDs <- dbGetQuery(con, "SELECT MEAL_ID FROM lu_meal;") %>%
        mutate(DB_IDS = MEAL_ID) %>%
        select(DB_IDS) %>%
        pull(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){
        dbExecute(con,
            paste0("UPDATE lu_meal SET MEAL_ID = '",from$MEAL_ID[i],"',",
                 "`MEAL_NAME` = '",from$MEAL_NAME[i],"',",
                 "`MEAL_TYPE` = '",from$MEAL_TYPE[i],"',",
                 "`MEAL_DESCRIPTION` = '",from$MEAL_DESCRIPTION[i],"',",
                 "`TOOLS` = '",from$TOOLS[i],"',",
                 "`UPTIME` = '",Sys.Date(),"',",
                 "`UPUSER` = '",LOCAL$userName,"',",
                 "`USER_ID` = '",LOCAL$userID,"',",
                 "`MEAL_TYPE_ID` = '",from$MEAL_TYPE_ID[i],"'",
                 " WHERE ",
                 "`MEAL_ID` = '",from$MEAL_ID[i],"';"
            )
        )
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){
        dbExecute(con,
            paste0("INSERT INTO lu_meal (",
                 "`MEAL_ID`,`MEAL_NAME`,`MEAL_TYPE`,`MEAL_DESCRIPTION`,",
                 "`TOOLS`,`INSTRUCTIONS`,`UPTIME`,`UPUSER`,`USER_ID`,`MEAL_TYPE_ID`) VALUES ('",
                 from$MEAL_ID[i],"','",
                 from$MEAL_NAME[i],"','",
                 from$MEAL_TYPE[i],"','",
                 from$MEAL_DESCRIPTION[i],"','",
                 from$TOOLS[i],"','",
                 from$INSTRUCTIONS[i],"','",
                 Sys.Date(),"','",
                 LOCAL$userName,"','",
                 LOCAL$userID,"','",
                 from$MEAL_TYPE_ID,"');"
            )
        )
    }
    dbExecute(con,"commit;")
}

#' delMeal
#' @description Removes a meal from XREF_TRIPS
#' @param session,input,output Shiny session objects
#' @param id The meal id taken from the delete button ID
#' @param data The LOCAL reactive vaues data object
#' @noRd
delMeal <- function(session, input, output, id, data){
    ns <- session$ns
    LOCAL <- data
    con <- rivConnect()
        dbExecute(con, "start transaction;")
            dbExecute(con,
                paste0(
                    "DELETE from xref_trips WHERE USER_ID = ",
                    LOCAL$userID, " AND TRIP_ID = ",
                    LOCAL$tripID, " AND MEAL_ID = ",
                    strsplit(id, "_")[[1]][1], " AND MEAL_TYPE_ID = '",
                    getMealTypeID(strsplit(id, "_")[[1]][2]),
                    "' AND RIVER_DAY = ", strsplit(id, "_")[[1]][3],";"
                )
            )
        dbExecute(con, "commit;")
    dbDisconnect(con)
}

#' getMealType
#' @description Returns the meal type name from meal type ID
#' @param id The meal_type_id to return name for
#' @noRd
getMealType <- function(id){
    out <- switch(id,
        'Breakfast',
        'Lunch',
        'Dinner',
        'Dessert',
        'Snack',
        'Appetizer',
        'Cocktail'
    )
    return(out)
}

#' getMealTypeID
#' @description returns the meal type ID from meal type name
#' @param mType the meal type name to return ID for
#' @noRd
getMealTypeID <- function(mType){
    out <- switch(mType,
        'Breakfast' = 1,
        'Lunch' = 2,
        'Dinner' = 3,
        'Dessert' = 4,
        'Snack' = 5,
        'Appetizer' = 6,
        'Cocktail' = 7
    )
    return(out)
}

#' refreshLOCAL
#' @description Used after upsert functions. Pulls new data sets
#' from the database to the session LOCAL data object. Also creates new
#' LOCAL$XREF_TRIPS and new LOCAL$ALL_DATA. LU_USERS and LU_MEAL_TYPE
#' are not included as they are not updated during a session.
#' @param con The database connection object
#' @param data The LOCAL reactive vaues data object
#' @param tables A vector of table names in LOCAL to refresh from database.
#' @noRd
refreshLOCAL <- function(con, data, tables){
    LOCAL <- data

    # Refresh LOCAL tables -----
    dbExecute(con, "start transaction;")
        if('LU_INGREDIENTS' %in% tables){
            LOCAL$LU_INGREDIENTS <- dbGetQuery(con,
                paste0('select * from lu_ingredients where USER_ID in (0,',LOCAL$userID,');')
            )
        }

        if('LU_TRIPS' %in% tables){
            LOCAL$LU_TRIPS_DB <- dbGetQuery(con,
                paste0("select * from lu_trips where user_id = '",LOCAL$userID,"';")
            )
        }

        if('XREF_TRIPS' %in% tables){
            LOCAL$XREF_TRIPS <- dbGetQuery(con,
                paste0("select * from xref_trips where user_id = '",LOCAL$userID,"';")
            )
        }

        if('LU_MEAL' %in% tables){
            LOCAL$LU_MEAL <- dbGetQuery(con,
                paste0('select * from lu_meal where USER_ID in (0,',LOCAL$userID,');')
            )
        }

        if('XREF_INGREDIENT' %in% tables){
            LOCAL$XREF_INGREDIENT <- dbGetQuery(con,
                paste0('select * from xref_ingredient where USER_ID in (0,',LOCAL$userID,');')
            )
        }
    dbExecute(con, "commit;")

    LOCAL$LU_TRIPS <- LOCAL$XREF_TRIPS %>%
        #select(-c(UPUSER, UPTIME)) %>%
        left_join(
            LOCAL$LU_MEAL %>% select(c(MEAL_ID,MEAL_NAME,MEAL_DESCRIPTION,TOOLS,INSTRUCTIONS,
                                       MEAL_ADD_ID,MEAL_DEL_ID,MEAL_VIEW_ID,MEAL_EDIT_ID)),
            by = c('MEAL_ID')
        ) %>%
        left_join(
            LOCAL$LU_MEAL_TYPE %>% select(MEAL_TYPE_ID, MEAL_TYPE), by = c('MEAL_TYPE_ID')
        ) %>%
        left_join(
            LOCAL$LU_INGREDIENTS %>%
                select(INGREDIENT_ID, INGREDIENT_CATEGORY,INGREDIENT,
                       INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION, STORAGE_DESCRIPTION),
            by = c('INGREDIENT_ID')
        ) %>%

        left_join(LOCAL$LU_USERS %>% select(USER_ID, USERNAME), by = c('USER_ID')) %>%
        left_join(LOCAL$LU_TRIPS_DB %>% select(TRIP_ID, TRIPNAME, TRIP_DESC), by = c('TRIP_ID')) %>%
        mutate(
            MEAL_UNIQUE_ID = paste0(MEAL_ID,'_',MEAL_TYPE,'_',RIVER_DAY),
            INGREDIENT_UNIQUE_ID = paste0(MEAL_UNIQUE_ID,'_',INGREDIENT_ID),
            QTY = round(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
        )

    LOCAL$ALL_DATA <- LOCAL$LU_MEAL %>%
        select(-c(UPTIME, UPUSER, USER_ID)) %>%
        left_join(LOCAL$XREF_INGREDIENT %>% select(MEAL_ID,INGREDIENT_ID), by = 'MEAL_ID') %>%
        left_join(LOCAL$LU_INGREDIENTS %>% select(-c(UPTIME,UPUSER, USER_ID)), by = 'INGREDIENT_ID') %>%
        mutate(
            USERNAME = LOCAL$userName,
            USER_ID = LOCAL$userID
        )
}
