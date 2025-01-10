#Helper functions and dev items while transferring to a database

#'rivConnect
#' @description Function opens MariaDB connection to mySQL database
#' @noRd
rivConnect <- function(){
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

    sql <- "insert into lu_users (USER_ID, USERNAME, EMAIL,
                UPTIME, UPUSER) VALUES (?newID, ?uName, ?uName, now(), 'riverplanner@system.com');"

    query <- DBI::sqlInterpolate(con, sql, newID = newid, uName = userName )

    dbExecute(con, query)

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

    # TODO 2/18/2024 Swapping LOCAL$tripID for from$TRIP_ID[i]
    # dbIDs <- dbGetQuery(con,paste0("SELECT * from xref_trips WHERE USER_ID =",
    #     LOCAL$userID," AND trip_id ='",LOCAL$tripID,"';")) %>%
    #     mutate(DB_IDS = paste(MEAL_ID,RIVER_DAY,INGREDIENT_ID,TRIP_ID,MEAL_TYPE_ID, sep = "_")) %>%
    #     select(DB_IDS) %>%
    #     pull(.)

    dbIDs <- dbGetQuery(con,paste0("SELECT * from xref_trips WHERE USER_ID =",
        LOCAL$userID," AND trip_id ='", from$TRIP_ID[i],"';")) %>%
        mutate(DB_IDS = paste(MEAL_ID,RIVER_DAY,INGREDIENT_ID,TRIP_ID,MEAL_TYPE_ID, sep = "_")) %>%
        select(DB_IDS) %>%
        pull(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){

        sql <- "UPDATE xref_trips SET `MEAL_ID` = ?mealId, `RIVER_DAY` = ?riverDay,
            `INGREDIENT_ID` = ?ingId, `TRIP_ID` = ?tripId, `MEAL_TYPE_ID` = ?mealTypeId,
            `MEAL_NOTES` = ?mealNotes, `NO_ADULTS` = ?noAdults, `NO_KIDS` = ?noKids,
            `NO_PEOPLE_CALC` = ?noPeopleCalc, `SERVING_SIZE_FACTOR` = ?ssf,
            `USER_ID` = ?userId, `UPTIME` = ?uTime, `UPUSER` = ?uUser
            WHERE `MEAL_ID` = ?mealId AND `RIVER_DAY` = ?riverDay AND
            `INGREDIENT_ID` = ?ingId AND `TRIP_ID` = ?tripId AND `MEAL_TYPE_ID` = ?mealTypeId
            AND `USER_ID` = ?userId;"

        qry <- DBI::sqlInterpolate(con, sql, mealId = from$MEAL_ID[i],
            riverDay = from$RIVER_DAY[i], ingId = from$INGREDIENT_ID[i],
            tripId = from$TRIP_ID[i], mealTypeId = from$MEAL_TYPE_ID[i],
            mealNotes = from$MEAL_NOTES[i], noAdults = from$NO_ADULTS[i],
            noKids = from$NO_KIDS[i], noPeopleCalc = from$NO_PEOPLE_CALC[i],
            ssf = from$SERVING_SIZE_FACTOR[i], userId = LOCAL$userID,
            uTime = Sys.Date(), uUser = LOCAL$userName)

        dbExecute(con, qry)
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){

        sql <- "INSERT INTO xref_trips (`MEAL_ID`,`RIVER_DAY`,`INGREDIENT_ID`,
            `TRIP_ID`,`MEAL_TYPE_ID`,`MEAL_NOTES`,`NO_ADULTS`,`NO_KIDS`,
            `NO_PEOPLE_CALC`,`SERVING_SIZE_FACTOR`,`USER_ID`,`UPTIME`,`UPUSER`)
            VALUES (?mealId,?riverDay,?ingId,?tripId,?mealTypeId,?mealNotes,
            ?noAdults,?noKids,?noPeopleCalc,?ssf,?userId,?uTime,?uUser);"

        qry <- DBI::sqlInterpolate(con, sql, mealId = from$MEAL_ID[i],
            riverDay = from$RIVER_DAY[i], ingId = from$INGREDIENT_ID[i],
            tripId = from$TRIP_ID[i], mealTypeId = from$MEAL_TYPE_ID[i],
            mealNotes = from$MEAL_NOTES[i], noAdults = from$NO_ADULTS[i],
            noKids = from$NO_KIDS[i], noPeopleCalc = from$NO_PEOPLE_CALC[i],
            ssf = from$SERVING_SIZE_FACTOR[i], userId = LOCAL$userID,
            uTime = Sys.Date(), uUser = LOCAL$userName)

        dbExecute(con, qry)
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

    sql <- "DELETE FROM xref_trips WHERE USER_ID = ?userId AND MEAL_ID = ?mealId
         AND RIVER_DAY = ?riverDay AND INGREDIENT_ID = ?ingId AND TRIP_ID = ?tripId
        AND MEAL_TYPE_ID = ?mealTypeId;"

    qry <- DBI::sqlInterpolate(con, sql, userId = LOCAL$userID, mealId = df$MEAL_ID,
        riverDay = df$RIVER_DAY, ingId = df$INGREDIENT_ID, tripId = df$TRIP_ID,
        mealTypeId = df$MEAL_TYPE_ID)

    dbExecute(con, "start transaction;")
        dbExecute(con, qry)
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
    recordID <- from$INGREDIENT_ID[i]
    dbIDs <- dbGetQuery(con, "SELECT DISTINCT INGREDIENT_ID FROM lu_ingredients;") %>%
        pull(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){

        sql <- "UPDATE lu_ingredients SET INGREDIENT_ID = ?ingID,
            `INGREDIENT_CATEGORY` = ?ingCat, `INGREDIENT` = ?ing,
            `INGREDIENT_DESCRIPTION` = ?ingDesc, `SERVING_SIZE_DESCRIPTION` = ?ssDesc,
            `SERVING_SIZE_FACTOR` = ?ssF, `STORAGE_DESCRIPTION` = ?storDesc,
            `USER_ID` = ?userID, `UPTIME` = ?uTime,
            `UPUSER` = ?uUser WHERE `INGREDIENT_ID` = ?ingID"

        query <- DBI::sqlInterpolate(con, sql, ingID = from$INGREDIENT_ID[i],
            ingCat = from$INGREDIENT_CATEGORY[i], ing = from$INGREDIENT[i],
            ingDesc = from$INGREDIENT_DESCRIPTION[i], ssDesc = from$SERVING_SIZE_DESCRIPTION[i],
            ssF = from$SERVING_SIZE_FACTOR[i], storDesc = from$STORAGE_DESCRIPTION[i],
            userID = from$USER_ID[i], uTime = Sys.Date(), uUser = LOCAL$userName
        )

        dbExecute(con, query)
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){

        sql <- "INSERT INTO lu_ingredients (`INGREDIENT_ID`,`INGREDIENT_CATEGORY`,
            `INGREDIENT`,`INGREDIENT_DESCRIPTION`,`SERVING_SIZE_DESCRIPTION`,
            `SERVING_SIZE_FACTOR`,`STORAGE_DESCRIPTION`,`USER_ID`,`UPTIME`,
            `UPUSER`) VALUES (?ingID,?ingCat,?ing,?ingDesc,?ssDesc,?ssF,
            ?storDesc,?userID,?uTime,?uUser);"

        qry <- DBI::sqlInterpolate(con, sql, ingID = from$INGREDIENT_ID[i],
            ingCat = from$INGREDIENT_CATEGORY[i], ing = from$INGREDIENT[i],
            ingDesc = from$INGREDIENT_DESCRIPTION[i], ssDesc = from$SERVING_SIZE_DESCRIPTION[i],
            ssF = from$SERVING_SIZE_FACTOR[i], storDesc = from$STORAGE_DESCRIPTION[i],
            userID = from$USER_ID[i], uTime = Sys.Date(), uUser = LOCAL$userName)

        dbExecute(con, qry)
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

    if(length(unique(from$USER_ID)) > 1) stop('Error in upsertXrefIng multiple UserIDs in xref table')
    if(LOCAL$userID != unique(from$USER_ID)) stop('Error in upsertXrefIng user ID does not match.')

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

        sql <- "UPDATE xref_ingredient SET `INGREDIENT_ID` = ?ingId, `MEAL_ID` =
            ?mealId, `INGREDIENT` = ?ing, `MEAL_NAME` = ?mealName, `UPTIME` = ?uTime,
            `UPUSER` = ?uUser, `USER_ID` = ?userId WHERE `MEAL_ID` = ?mealId AND
            `INGREDIENT_ID` = ?ingId;"

        qry <- DBI::sqlInterpolate(con, sql, ingId = from$INGREDIENT_ID[i],
            mealId = from$MEAL_ID[i], ing = from$INGREDIENT[i], mealName = from$MEAL_NAME[i],
            uTime = Sys.Date(), uUser = LOCAL$userName, userId = from$USER_ID[i])

        dbExecute(con, qry)

    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){

        sql <- "INSERT INTO xref_ingredient (`INGREDIENT_ID`,`MEAL_ID`,
            `INGREDIENT`,`MEAL_NAME`,`UPTIME`,`UPUSER`,`USER_ID`) VALUES (?ingId,
            ?mealId, ?ing, ?mealName, ?uTime, ?uUser, ?userId);"

        qry <- DBI::sqlInterpolate(con, sql, ingId = from$INGREDIENT_ID[i],
            mealId = from$MEAL_ID[i], ing = from$INGREDIENT[i], mealName = from$MEAL_NAME[i],
            uTime = Sys.Date(), uUser = LOCAL$userName, userId = from$USER_ID[i])

        dbExecute(con, qry)

    }
    dbExecute(con,"commit;")
}

#' deleteXrefIng
#' @description Deletes a meal from XREF_INGREDIENT in the DB
#' @param con The database connection object
#' @param mealId Numeric. The meal ID to delete from DB
#' @param userId Numeric. The user ID who owns the meal. Must be
#' the current active user and meal must be theirs to delete
#'
#' @noRd
deleteXrefIng <- function(con = con, mealId, userId){
    dbExecute(con, "start transaction;")
    dbExecute(con,
        paste0('DELETE FROM xref_ingredient WHERE USER_ID = ',
            userId, ' AND MEAL_ID = ', mealId, ';'
        )
    )
    dbExecute(con,"commit;")
}

#' delIngXrefIng
#' @description Deletes an ingredient from XREF_INGREDIENT in the DB
#' @param con The database connection object
#' @param mealId Numeric. The meal ID to delete from DB
#' @param ingId Numeric. The ingredient ID to delete from the meal
#' @param userId Numeric. The user ID who owns the meal. Must be
#' the current active user and meal must be theirs to delete
#'
#' @noRd
delIngXrefIng <- function(con = con, mealId, ingId, userId){
    dbExecute(con, "start transaction;")
    dbExecute(con,
              paste0('DELETE FROM xref_ingredient WHERE USER_ID = ',
                     userId, ' AND MEAL_ID = ', mealId,
                     ' AND INGREDIENT_ID = ', ingId, ';'
              )
    )
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

    if(LOCAL$userID != from$USER_ID) stop('Error in upsertMeal user ID does not match.')

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

        sql <- "UPDATE lu_meal SET MEAL_ID = ?mealId, `MEAL_NAME` = ?mealName,
            `MEAL_TYPE` = ?mealType, `MEAL_DESCRIPTION` = ?mealDesc,
            `TOOLS` = ?tools, `INSTRUCTIONS` = ?inst, `UPTIME` = ?uTime,
            `UPUSER` = ?uUser, `USER_ID` = ?userId, `MEAL_TYPE_ID` = ?mTypeId
            WHERE `MEAL_ID` = ?mealId AND `USER_ID`= ?userId;"

        qry <- DBI::sqlInterpolate(con, sql, mealId = from$MEAL_ID[i],
            mealName = from$MEAL_NAME[i], mealType = from$MEAL_TYPE[i],
            mealDesc = from$MEAL_DESCRIPTION[i], tools = from$TOOLS[i],
            inst = from$INSTRUCTIONS[i], uTime = Sys.Date(), uUser = LOCAL$userName,
            userId = from$USER_ID[i], mTypeId = from$MEAL_TYPE_ID[i])

        dbExecute(con, qry)

    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){

        sql <- "INSERT INTO lu_meal (`MEAL_ID`,`MEAL_NAME`,`MEAL_TYPE`,
            `MEAL_DESCRIPTION`,`TOOLS`,`INSTRUCTIONS`,`UPTIME`,`UPUSER`,`USER_ID`,
            `MEAL_TYPE_ID`) VALUES (?mealId,?mealName,?mealType,?mealDesc,?tools,
            ?inst,?uTime,?uUser,?userId,?mTypeId);"

        qry <- DBI::sqlInterpolate(con, sql, mealId = from$MEAL_ID[i],
            mealName = from$MEAL_NAME[i], mealType = from$MEAL_TYPE[i],
            mealDesc = from$MEAL_DESCRIPTION[i], tools = from$TOOLS[i],
            inst = from$INSTRUCTIONS[i], uTime = Sys.Date(), uUser = LOCAL$userName,
            userId = from$USER_ID[i], mTypeId = from$MEAL_TYPE_ID[i])

        dbExecute(con, qry)

    }
    dbExecute(con,"commit;")
}

#' deleteLuMeal
#'
#' @description Deletes a meal from LU_MEAL in the DB
#' @param con The database connection object
#' @param mealId Numeric. The meal ID to delete from DB
#' @param userId Numeric. The user ID who owns the meal. Must be
#' the current active user and meal must be theirs to delete
#'
#' @noRd
deleteLuMeal <- function(con = con, mealId, userId){
    dbExecute(con, "start transaction;")
    dbExecute(con,
        paste0('DELETE FROM lu_meal WHERE USER_ID = ',
            userId, ' AND MEAL_ID = ', mealId, ';'
        )
    )
    dbExecute(con,"commit;")
}

#' delMeal
#' @description Removes a meal from XREF_TRIPS
#' @param session Shiny session object
#' @param id The meal id taken from the delete button ID, or from LOCAL$XREF_TRIPS if no trip loaded
#' @param data The LOCAL reactive values data object
#' @param tripId Character. The trip ID to remove meal from
#' @noRd
#delMeal <- function(session, input, output, id, data){
delMeal <- function(id, data, tripId){
    req(length(tripId) > 0)
    req(length(id) > 0)
    #ns <- session$ns
    LOCAL <- data

    con <- rivConnect()

    dbExecute(con, "start transaction;")

        sql <- "SELECT distinct trip_id, meal_id FROM xref_trips WHERE trip_id = ?tripId
            and meal_id = ?mealId"

        qry <- DBI::sqlInterpolate(con, sql, tripId = tripId,
            mealId = strsplit(id, "_")[[1]][1])

        toKill <- dbGetQuery(con, qry)

        if(nrow(toKill) <= 1){
            xrefT <- data.frame(
                MEAL_ID = 0,
                RIVER_DAY = 1,
                INGREDIENT_ID = 1,
                TRIP_ID = ifelse(length(LOCAL$tripID) == 0, tripId, LOCAL$tripID),
                MEAL_TYPE_ID = 1,
                MEAL_NOTES = 'startup',
                NO_ADULTS = LOCAL$noAdults,
                NO_KIDS = LOCAL$noKids,
                NO_PEOPLE_CALC = LOCAL$noPeopleCalc,
                SERVING_SIZE_FACTOR = 1,
                USER_ID = LOCAL$userID,
                UPTIME = Sys.Date(),
                UPUSER = LOCAL$userName
            )
            #add dummy record bc about to kill last of this meal/trip in xref_trips
            #this is so the trip record is not abandoned in lu_trips if it has no meals
            #Meal 0 stays in xref_trips until more meals are added but is not shown in the UI

            #con <- rivConnect()
            upsertXrefTrips(con = con, from = xrefT, data = LOCAL, row = 1)
            #dbDisconnect(con)
        }

    dbExecute(con, "commit;")

    dbExecute(con, "start transaction;")
        sql <- "DELETE from xref_trips WHERE USER_ID = ?userId AND TRIP_ID = ?tripId
            AND MEAL_ID = ?mealId AND MEAL_TYPE_ID = ?mealTypeId AND RIVER_DAY = ?riverDay;"

        qry <- DBI::sqlInterpolate(con, sql, userId = LOCAL$userID, tripId = tripId,
            mealId = strsplit(id, "_")[[1]][1],
            mealTypeId = getMealTypeID(strsplit(id, "_")[[1]][2]),
            riverDay = strsplit(id, "_")[[1]][3])

        dbExecute(con, qry)
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

#' upsertXrefGear
#' @description Updates or inserts one record in XrefGear
#' @param con The database connection object
#' @param from A dataframe of records that are in the local session
#' LOCAL$XREF_GEAR dataframe and the database needs to have records added or updated based on this
#' @param data The LOCAL reactive values data onject
#' @param row The row number of the `from` dataframe being looped
#'
#' @noRd
upsertXrefGear <- function(con = con, from = xrefG, data = LOCAL, row) {
    LOCAL <- data
    i <- row

    recordID <- from[i,] %>%
        mutate(RECORD_ID = paste(TRIP_ID, GEAR_ID, sep = "_")) %>%
        select(RECORD_ID) %>%
        pull(.)

    dbIDs <- dbGetQuery(con,paste0("SELECT * from xref_gear WHERE USER_ID =",
        LOCAL$userID," AND trip_id ='", from$TRIP_ID[i],"';")) %>%
        mutate(DB_IDS = paste(TRIP_ID, GEAR_ID, sep = "_")) %>%
        select(DB_IDS) %>%
        pull(.)

    dbExecute(con, "start transaction;")
    if(length(dbIDs) > 0 & recordID %in% dbIDs){

        sql <- "UPDATE xref_gear SET `TRIP_ID` = ?tripId, `GEAR_ID` = ?gearId,
            `GEAR_CAT_ID` = ?gearCatId, `GEAR_QTY` = ?gearQty,
            `USER_ID` = ?userId, `UPTIME` = ?uTime, `UPUSER` = ?uUser
            WHERE `TRIP_ID` = ?tripId AND `GEAR_ID` = ?gearId AND
            `USER_ID` = ?userId;"

        qry <- DBI::sqlInterpolate(con, sql, tripId = from$TRIP_ID[i],
            gearId = from$GEAR_ID[i], gearCatId = from$GEAR_CAT_ID[i],
            gearQty = from$GEAR_QTY[i], userId = LOCAL$userID,
            uTime = Sys.Date(), uUser = LOCAL$userName)

        dbExecute(con, qry)
    }

    if(length(dbIDs) == 0 | !recordID %in% dbIDs){

        sql <- "INSERT INTO xref_gear (`TRIP_ID`,`GEAR_ID`,`GEAR_CAT_ID`,
            `GEAR_QTY`,`USER_ID`,`UPTIME`,`UPUSER`)
            VALUES (?tripId,?gearId,?gearCatId,?gearQty,?userId,?uTime,?uUser);"

        qry <- DBI::sqlInterpolate(con, sql, tripId = from$TRIP_ID[i],
            gearId = from$GEAR_ID[i], gearCatId = from$GEAR_CAT_ID[i],
            gearQty = from$GEAR_QTY[i], userId = LOCAL$userID,
            uTime = Sys.Date(), uUser = LOCAL$userName)

        dbExecute(con, qry)
    }
    dbExecute(con,"commit;")


}

#' getMaxGearID
#' @description Gets the maximum INGREDIENT_ID from LU_INGREDIENTS.
#' @noRd
getMaxGearID <- function(){
    con <- rivConnect()
    dbExecute(con, "start transaction;")
    maxGearID <- dbGetQuery(con, 'SELECT max(GEAR_ID) from lu_gear;')
    dbExecute(con,"commit;")
    dbDisconnect(con)
    return(as.integer(maxGearID))
}

#' upsertLuGear
#' @description Adds or updates one record in the LU_GEAR DB table
#'
#'
#' @noRd
upsertLuGear <- function(from = luG, data = LOCAL) {
    LOCAL <- data

    sql <- "INSERT INTO lu_gear (`GEAR_ID`,`GEAR_CAT_ID`,
            `GEAR_NAME`,`GEAR_DESC`,`SERVES`,`DEFAULT_QTY`,`USER_ID`,`UPUSER`,`UPTIME`)
            VALUES (?gearId,?gearCatId,?gearName,?gearDesc,?serves,?defaultQty,
            ?userId,?uUser,?uTime);"

    con <- rivConnect()

    qry <- DBI::sqlInterpolate(con, sql, gearId = from$GEAR_ID,
        gearCatId = from$GEAR_CAT_ID, gearName = from$GEAR_NAME,
        gearDesc = from$GEAR_DESC, serves = from$SERVES,
        defaultQty = from$DEFAULT_QTY,
        userId = from$USER_ID, uUser = from$UPUSER, uTime = from$UPTIME
    )


    dbExecute(con, "start transaction;")
    dbExecute(con, qry)
    dbExecute(con,"commit;")
    dbDisconnect(con)
}

#' deleteXrefGear
#' @description Deletes one record from xref_gear.
#' Designed to be used in a loop or with map() to loop through
#' rows of a dataframe of records to delete from the database
#' @param con The database connection object
#' @param from A dataframe of records that are in the DB but have been removed from
#' the local session LOCAL$XREF_GEAR dataframe. Obtained with anti_join before calling this function
#' @param data The LOCAL reactive values data onject
#' @param row The row number of the `from` dataframe being looped
#' @noRd
deleteXrefGear <- function(con = con, from = toDelete, data = LOCAL, row){
    LOCAL <- data
    i <- row
    df <- from[i,]

    sql <- "DELETE FROM xref_gear WHERE USER_ID = ?userId AND TRIP_ID = ?tripId
        AND GEAR_ID = ?gearId;"

    qry <- DBI::sqlInterpolate(con, sql, userId = LOCAL$userID, tripId = df$TRIP_ID,
                               gearId = df$GEAR_ID)

    dbExecute(con, "start transaction;")
    dbExecute(con, qry)
    dbExecute(con,"commit;")
}

#' deleteXrefGearCat
#' @description Deletes any records of a given gea category ID from xref_gear
#' Only used if user unchecks all items in a category and clicks save
#' @param con The database connection object
#' @param rvObj The LOCAL rv object
#' @param catId The gear category to delete for the given tripId
#' @noRd
deleteXrefGearCat <- function(con, rvObj = LOCAL, catId) {
    LOCAL <- rvObj
    stopifnot(LOCAL$gearCatActive == catId)
    userId <- LOCAL$userID
    tripId <- LOCAL$tripID

    sql <- "DELETE FROM xref_gear WHERE USER_ID = ?userId AND TRIP_ID = ?tripId
        AND GEAR_CAT_ID = ?catId;"

    qry <- DBI::sqlInterpolate(con, sql, userId = userId, tripId = tripId, catId = catId)

    dbExecute(con, "start transaction;")
    dbExecute(con, qry)
    dbExecute(con,"commit;")

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

        if('XREF_GEAR' %in% tables) {
            LOCAL$XREF_GEAR <- dbGetQuery(con,
                paste0('select * from xref_gear where USER_ID in (0,',LOCAL$userID,');')
            )
        }

        if('LU_GEAR' %in% tables) {
            LOCAL$LU_GEAR <- dbGetQuery(con,
                paste0('select * from lu_gear where USER_ID in (0,',LOCAL$userID,');')
            )
        }

    dbExecute(con, "commit;")
    dbDisconnect(con)

    LOCAL$LU_TRIPS <- LOCAL$XREF_TRIPS %>%
        #select(-c(UPUSER, UPTIME)) %>%
        left_join(
            LOCAL$LU_MEAL %>%
                select(c(MEAL_ID,MEAL_NAME,MEAL_DESCRIPTION,TOOLS,INSTRUCTIONS,
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
            # QTY = round(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
            QTY = case_when(
                NO_PEOPLE_CALC * SERVING_SIZE_FACTOR <= 0.5 ~
                    round_any(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR, 0.5, ceiling),
                TRUE ~ ceiling(NO_PEOPLE_CALC * SERVING_SIZE_FACTOR)
            )
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
