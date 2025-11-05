
#' @title check.table
#' @import dplyr tidyr RSQLite
#' @description opens and views SQL tables created by input.trip()
#' @export
check.table <- function(table = "default", choose.trip = FALSE,
                        dat.dir = if(exists("dat.dir.global")) dat.dir.global else NULL,
                        trip.file = if(exists("last.trip.file")) last.trip.file else NULL){

  if(!table %in% c("default","trip","set","trap","fish")){
    warning("Error: Invalid table name, options are: 'trip','set','trap','fish'")
    return(NULL)
  }

  if(is.null(trip.file) && !choose.trip){return(print("No Trip File Chosen!"))}


  if(choose.trip){
    dlg_message(paste0("In the following window, select the .db trip file you want to check."))
    trip.file <- dlg_open()$res
    last.trip.file <<- trip.file
  }


  suppressWarnings({
  # Initialize database connection
  db <- dbConnect(RSQLite::SQLite(), trip.file)

  if(table  %in% "fish"){
    query = paste0("SELECT * FROM FISH_INFO")
    fish <- dbSendQuery(db, query)
    fish <- fetch(fish)
    dbDisconnect(db)
    ## sort rows before viewing
    fish <- fish %>% separate(TRAP_ID, into = c("part1", "part2", "num1", "num2"), sep = "_", remove = FALSE, convert = TRUE) %>%
      arrange(num1, num2, as.numeric(TRAP_NO), as.numeric(FISH_NO))
    fish <- fish %>% dplyr::select(-part1,-part2,-num1,-num2)
    View(fish)
  }
  if(table %in% "trap"){
    query = paste0("SELECT * FROM TRAP_INFO")
    trap <- dbSendQuery(db, query)
    trap <- fetch(trap)
    dbDisconnect(db)
    trap <- trap %>% arrange(as.numeric(SET_NO), as.numeric(TRAP_NO))
    View(trap)
  }
  if(table %in% "set"){
    query = paste0("SELECT * FROM SET_INFO")
    set <- dbSendQuery(db, query)
    set <- fetch(set)
    dbDisconnect(db)
    set <- set %>% arrange(as.numeric(SET_NO))
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






