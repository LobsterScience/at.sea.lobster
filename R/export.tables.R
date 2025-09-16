
#' @title export.tables
#' @import dplyr RSQLite svDialogs
#' @description Exports tables created by input.trip() as csv files
#' @export
export.tables <- function(tables = NULL, choose.trip = FALSE, trip = last.trip, trip.dir = dat.dir.global){

  if(is.null(tables)){
    warning("Error: no tables chosen. Options are: 'fish','trap','set','trip','all'. For multiple tables use tables = c('fish','trap'...).",immediate. = T)
    return(NULL)
  }

  if(!any(tables %in% c("all","trip","set","trap","fish"))){
    warning("Error: No valid table names entered, options are: 'trip','set','trap','fish'")
    return(NULL)
  }

  trip.file <- paste0(trip.dir,"/",trip,".db")
  trip.name <- trip

  if(choose.trip){
    dlg_message(paste0("In the following window, select the .db trip file you want to generate ",paste(tables, collapse = ", ")," tables from."))
    trip.file <- dlg_open()$res
    trip.name <- gsub(".db","",basename(trip.file))
  }

  dlg_message("In the following window, choose the directory where you want to store you csv table(s)")
  out.dir <- dlg_dir()$res

  if("all" %in% tables){
    tables <- c("trip","set","trap","fish")
  }

  suppressWarnings({
    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), trip.file)

    if("fish" %in% tables){
      query = paste0("SELECT * FROM FISH_INFO")
      fish <- dbSendQuery(db, query)
      fish <- fetch(fish)
      write.csv(fish, file = paste0(out.dir,"/",trip.name,"_fish.csv"), row.names = F)
    }
    if("trap" %in% tables){
      query = paste0("SELECT * FROM TRAP_INFO")
      trap <- dbSendQuery(db, query)
      trap <- fetch(trap)
      write.csv(trap, file = paste0(out.dir,"/",trip.name,"_trap.csv"),row.names = F)
    }
    if("set" %in% tables){
      query = paste0("SELECT * FROM SET_INFO")
      set <- dbSendQuery(db, query)
      set <- fetch(set)
      write.csv(set, file = paste0(out.dir,"/",trip.name,"_set.csv"),row.names = F)
    }
    if("trip" %in% tables){
      query = paste0("SELECT * FROM TRIP_INFO")
      trip <- dbSendQuery(db, query)
      trip <- fetch(trip)
      write.csv(trip, file = paste0(out.dir,"/",trip.name,"_trip.csv"),row.names = F)
    }

    dbDisconnect(db)
  })

}
