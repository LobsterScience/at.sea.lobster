
#' @title export.tables
#' @import dplyr tidyr RSQLite svDialogs
#' @description Exports tables created by input.trip() as csv files
#' @export
export.tables <- function(tables = NULL, choose.trip = FALSE, merge.tables = FALSE,
                           trip.dir = if(exists("dat.dir.global")) dat.dir.global else NULL,
                        trip.file = if(exists("last.trip.file")) last.trip.file else NULL){

  if(merge.tables){tables = "all"}

  if(is.null(tables)){
    warning("Error: no tables chosen. Options are: 'fish','trap','set','trip','all'. For multiple tables use tables = c('fish','trap'...).",immediate. = T)
    return(NULL)
  }

  if(!any(tables %in% c("all","trip","set","trap","fish"))){
    warning("Error: No valid table names entered, options are: 'trip','set','trap','fish'")
    return(NULL)
  }

  if(is.null(trip.file) && !choose.trip){return(print("No Trip File Chosen!"))}

  if(choose.trip){
    dlg_message(paste0("In the following window, select the .db trip file you want to generate ",paste(tables, collapse = ", ")," tables from."))
    trip.file <- dlg_open()$res
    last.trip.file <<- trip.file
  }

  if(!is.null(trip.file)){
    trip.name <- tools::file_path_sans_ext(basename(trip.file))
  }

  dlg_message("In the following window, choose the directory where you want to store you csv table(s)")
  out.dir <- dlg_dir()$res

  if("all" %in% tables || merge.tables){
    tables <- c("trip","set","trap","fish")
  }

  suppressWarnings({
    # Initialize database connection
    db <- dbConnect(RSQLite::SQLite(), trip.file)

    if("fish" %in% tables){
      query = paste0("SELECT * FROM FISH_INFO")
      fish <- dbSendQuery(db, query)
      fish <- fetch(fish)
      ## sort rows before exporting
      fish <- fish %>% separate(TRAP_ID, into = c("part1", "part2", "num1", "num2"), sep = "_", remove = FALSE, convert = TRUE) %>%
        arrange(num1, num2, as.numeric(TRAP_NO), as.numeric(FISH_NO))
      fish <- fish %>% dplyr::select(-part1,-part2,-num1,-num2)
      if(!merge.tables){
        write.csv(fish, file = paste0(out.dir,"/",trip.name,"_fish.csv"), row.names = F)
      }
    }
    if("trap" %in% tables){
      query = paste0("SELECT * FROM TRAP_INFO")
      trap <- dbSendQuery(db, query)
      trap <- fetch(trap)
      trap <- trap %>% arrange(as.numeric(SET_NO), as.numeric(TRAP_NO))
      if(!merge.tables){
        write.csv(trap, file = paste0(out.dir,"/",trip.name,"_trap.csv"),row.names = F)
      }
    }
    if("set" %in% tables){
      query = paste0("SELECT * FROM SET_INFO")
      set <- dbSendQuery(db, query)
      set <- fetch(set)
      set <- set %>% arrange(as.numeric(SET_NO))
      if(!merge.tables){
        write.csv(set, file = paste0(out.dir,"/",trip.name,"_set.csv"),row.names = F)
      }
    }
    if("trip" %in% tables){
      query = paste0("SELECT * FROM TRIP_INFO")
      trip <- dbSendQuery(db, query)
      trip <- fetch(trip)
      if(!merge.tables){
        write.csv(trip, file = paste0(out.dir,"/",trip.name,"_trip.csv"),row.names = F)
      }
    }

    dbDisconnect(db)

    ### if merging to one table
    if(merge.tables){
      fish <- fish %>% dplyr::select(-TRAP_NO)
      fish_trap <- left_join(fish,trap)
      fish_trap <- fish_trap %>% dplyr::select(-SET_NO)
      fish_trap_set <- left_join(fish_trap, set)
      full.tab <- left_join(fish_trap_set, trip)
      write.csv(full.tab, file = paste0(out.dir,"/",trip.name,"_merged.csv"),row.names = F)
    }

    })

}
