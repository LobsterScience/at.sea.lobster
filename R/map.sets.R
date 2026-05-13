#' @title map.sets
#' @import dplyr RSQLite sf rnaturalearth
#' @description Opens SET_INFO from a trip .db file and plots set coordinates as points.
#' @export
map.sets <- function(choose.trip = FALSE,
                     dat.dir = if(exists("dat.dir.global")) dat.dir.global else NULL,
                     trip.file = if(exists("last.trip.file")) last.trip.file else NULL){

  if(is.null(trip.file) && !choose.trip){
    return(print("No Trip File Chosen!"))
  }

  if(choose.trip){
    dlg_message("In the following window, select the .db trip file you want to map.")
    trip.file <- dlg_open()$res
    last.trip.file <<- trip.file
  }

  suppressWarnings({
    db <- dbConnect(RSQLite::SQLite(), trip.file)
    query <- paste0("SELECT * FROM SET_INFO")
    set <- dbSendQuery(db, query)
    set <- fetch(set)
    dbDisconnect(db)

    if(nrow(set) == 0){
      warning("SET_INFO has no rows to map.")
      return(invisible(NULL))
    }

    ddmm_to_dd <- function(x, west = FALSE){
      x <- suppressWarnings(as.numeric(x))
      deg <- floor(x / 100)
      mins <- x - (deg * 100)
      dd <- deg + mins / 60
      if(west){
        dd <- -dd
      }
      dd
    }

    set <- set %>%
      mutate(
        lat_dd = ddmm_to_dd(LATDDMM, west = FALSE),
        lon_dd = ddmm_to_dd(LONGDDMM, west = TRUE)
      )

    good <- !is.na(set$lat_dd) & !is.na(set$lon_dd)
    if(!any(good)){
      warning("No valid LATDDMM/LONGDDMM coordinates found in SET_INFO.")
      return(invisible(NULL))
    }

    set_sf <- sf::st_as_sf(
      set[good, ],
      coords = c("lon_dd", "lat_dd"),
      crs = 4326,
      remove = FALSE
    )

    world_map <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

    bbox <- sf::st_bbox(set_sf)
    lon_pad <- max((bbox["xmax"] - bbox["xmin"]) * 0.35, 0.3)
    lat_pad <- max((bbox["ymax"] - bbox["ymin"]) * 0.35, 0.3)
    xlim <- c(bbox["xmin"] - lon_pad, bbox["xmax"] + lon_pad)
    ylim <- c(bbox["ymin"] - lat_pad, bbox["ymax"] + lat_pad)

    graticule <- sf::st_graticule(set_sf, lon = pretty(xlim, 6), lat = pretty(ylim, 6))

    plot(sf::st_geometry(world_map), col = "antiquewhite", border = "grey55", xlim = xlim, ylim = ylim, axes = TRUE)
    plot(sf::st_geometry(graticule), add = TRUE, col = "grey85", lty = 3)
    plot(sf::st_geometry(set_sf), add = TRUE, pch = 19, col = "blue")
    box()
    title(main = "SET_INFO set locations", xlab = "Longitude", ylab = "Latitude")

    invisible(set_sf)
  })
}
