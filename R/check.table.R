
#' @title check.table
#' @import dplyr RSQLite
#' @description opens and views SQL tables created by input.trip()
#' @export
check.table <- function(table = "default", choose.trip = FALSE, dat.dir = dat.dir.global, trip = last.trip){

  if(!table %in% c("default","trip","set","trap","fish")){
    warning("Error: Invalid table name, options are: 'trip','set','trap','fish'")
    return(NULL)
  }

  trip.file = paste0(dat.dir,"/",trip,".db")

  if(choose.trip){
    dlg_message(paste0("In the following window, select the .db trip file you want to check."))
    trip.file <- dlg_open()$res
  }

  suppressWarnings({
  # Initialize database connection
  db <- dbConnect(RSQLite::SQLite(), trip.file)

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






