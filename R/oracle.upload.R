library(svDialogs)
library(RSQLite)
library(dplyr)
library(tidyr)
library(DBI)
library(ROracle)



## CHOOSE TRIP FILES TO UPLOAD
  dlg_message(paste0("In the following window, select the .db trip files you want to upload to Oracle."))
  trip.files <- choose.files()


for(i in trip.files){
  ## STEP 1 BRING TRIP(S) INTO R

  # Initialize database connection
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
      con <<- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })


  }

