#' @title oracle.upload
#' @import dplyr RSQLite DBI svDialogs
#' @description Select and upload data from .db trip files to Oracle database (for DFO use)
#' @export
#'
oracle.upload <- function(oracle.user = oracle.personal.user, oracle.pass = oracle.personal.password,
                          oracle.serv = oracle.personal.server, table.space = "LOBSTER"){


#library(tidyr)

require(ROracle)



## CHOOSE TRIP FILES TO UPLOAD
  dlg_message(paste0("In the following window, select the .db trip files you want to upload to Oracle."))
  trip.files <- choose.files()


for(i in trip.files){
  ## STEP 1 BRING TRIP(S) INTO R

  # Initialize database connection (to local file)
  db <- dbConnect(RSQLite::SQLite(), i)

    fish.query = paste0("SELECT * FROM FISH_INFO")
    fish <- dbSendQuery(db, fish.query)
    fish <- fetch(fish)
    ## sort rows before uploading
    fish <- fish %>% separate(TRAP_ID, into = c("part1", "part2", "num1", "num2"), sep = "_", remove = FALSE, convert = TRUE) %>%
      arrange(num1, num2, as.numeric(TRAP_NO), as.numeric(FISH_NO))
    fish <- fish %>% dplyr::select(-part1,-part2,-num1,-num2)

    trap.query = paste0("SELECT * FROM TRAP_INFO")
    trap <- dbSendQuery(db, trap.query)
    trap <- fetch(trap)
    trap <- trap %>% arrange(as.numeric(SET_NO), as.numeric(TRAP_NO))

    set.query = paste0("SELECT * FROM SET_INFO")
    set <- dbSendQuery(db, set.query)
    set <- fetch(set)
    set <- set %>% arrange(as.numeric(SET_NO))

    trip.query = paste0("SELECT * FROM TRIP_INFO")
    trip <- dbSendQuery(db, trip.query)
    trip <- fetch(trip)

    dbDisconnect(db)


    ## STEP 2: UPLOAD IMPORTED TABLES TO ORACLE

    ## connect to LOBSTER table space using lobster team personal credentials
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <<- ROracle::dbConnect(drv, username = oracle.user, password = oracle.pass, dbname = oracle.serv)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })

    ## check if any of the chosen trips are already uploaded
    checktrip <- paste0("SELECT * FROM ",table.space,".AT_SEA_TRIP_INFO WHERE TRIP_ID = '",trip$TRIP_ID[1], "'", sep = "")
    trip_result <- dbGetQuery(con, checktrip)

    if(nrow(trip_result)>0){
      upload_remaining <- dlgMessage(type = "yesno", message = paste0("Warning! TRIP: ",trip$TRIP_ID[1]," is already in ",table.space,".AT_SEA_TRIP_INFO. Data for this trip will not be uploaded. Do you want to continue uploading the remaining selected trip files?"))
      if(upload_remaining$res %in% "no"){
        return(print("UPLOAD CANCELLED!"))
      }else{next} ## move to next trip file
    }

    ## upload (append) all tables with chosen data
    # dbWriteTable(con, "AT_SEA_TRIP_INFO", trip, append = TRUE, row.names = FALSE)
    # dbWriteTable(con, Id(schema = table.space, table = "AT_SEA_SET_INFO"), set, append = TRUE, row.names = FALSE)
    # dbWriteTable(con, Id(schema = table.space, table = "AT_SEA_TRAP_INFO"), trap, append = TRUE, row.names = FALSE)
    # dbWriteTable(con, Id(schema = table.space, table ="AT_SEA_FISH_INFO"), fish, append = TRUE, row.names = FALSE)

    ### dbWrite table doens't work for nested table shcema (LOBSTER). Need to function build INSERT queries
     insert_rows <- function(con, schema, table, df) {
       # Convert all columns to character to avoid ROracle coercion issues
       df[] <- lapply(df, as.character)

       # Escape single quotes and preserve NAs
       df[] <- lapply(df, function(x) {
         x <- gsub("'", "''", x)         # escape internal quotes
         x[is.na(x)] <- NA               # keep real NAs, not "NA" strings
         x
       })

       cols <- names(df)
       col_list <- paste(cols, collapse = ", ")

       for (i in seq_len(nrow(df))) {
         row <- df[i, ]

         # For each value: wrap in quotes if not NA, else use NULL
         vals <- vapply(row, function(x) {
           if (is.na(x) || x == "") {
             "NULL"
           } else {
             sprintf("'%s'", x)
           }
         }, character(1))

         sql <- sprintf(
           "INSERT INTO %s.%s (%s) VALUES (%s)",
           schema, table, col_list, paste(vals, collapse = ", ")
         )

         DBI::dbExecute(con, sql)
       }

       DBI::dbCommit(con)
       message("✅ Inserted ", nrow(df), " rows into ", schema, ".", table)
     }

     insert_rows(con, table.space, "AT_SEA_TRIP_INFO", trip)
     insert_rows(con, table.space, "AT_SEA_SET_INFO", set)
     insert_rows(con, table.space, "AT_SEA_TRAP_INFO", trap)
     insert_rows(con, table.space, "AT_SEA_FISH_INFO", fish)

    print(paste0("TRIP: ",trip$TRIP_ID[1], " uploaded to ",table.space, " Oracle table space."))

    dbDisconnect(con)

}

  print("UPLOADS COMPLETE")

}

