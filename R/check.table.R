
#' @title check.table
#' @import dplyr RSQLite
#' @description opens and views SQL tables created by input.trip()
#' @export
check.table <- function(table = "default", dat.dir = dat.dir.global){
  suppressWarnings({
  # Initialize database connection
  db <- dbConnect(RSQLite::SQLite(), paste0(dat.dir,"/INPUT_DATA.db"))

  if(table  %in% "fish"){
    query = paste0("SELECT * FROM FISH_INFO")
    fish <- dbSendQuery(db, query)
    fish <- fetch(fish)
    dbDisconnect(db)
    View(fish)
  }
  if(table %in% "trap"){
    query = paste0("SELECT * FROM TRAP_INFO")
    trap <- dbSendQuery(db, query)
    trap <- fetch(trap)
    dbDisconnect(db)
    View(trap)
  }
  if(table %in% "set"){
    query = paste0("SELECT * FROM SET_INFO")
    set <- dbSendQuery(db, query)
    set <- fetch(set)
    dbDisconnect(db)
    View(set)
  }
  if(table %in% c("trip","default")){
    if(table %in% "default"){
      (print("No table entered, using default = trip. Other options are: set, trap, and fish. Example: check.table('set')   "))
       }
    query = paste0("SELECT * FROM TRIP_INFO")
    trip <- dbSendQuery(db, query)
    trip <- fetch(trip)
    dbDisconnect(db)
    View(trip)
  }


  })

}






