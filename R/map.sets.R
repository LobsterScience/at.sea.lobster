#' @title map.sets
#' @import dplyr RSQLite sf maps png
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

    bbox <- sf::st_bbox(set_sf)
    lon_pad <- max((bbox["xmax"] - bbox["xmin"]) * 0.35, 0.3)
    lat_pad <- max((bbox["ymax"] - bbox["ymin"]) * 0.35, 0.3)
    xlim <- c(bbox["xmin"] - lon_pad, bbox["xmax"] + lon_pad)
    ylim <- c(bbox["ymin"] - lat_pad, bbox["ymax"] + lat_pad)


    mapbox_token <- if(exists("mapbox.token", envir = .GlobalEnv)) get("mapbox.token", envir = .GlobalEnv) else NULL

    if(!is.null(mapbox_token) && nzchar(mapbox_token)) {
      map_width <- 900
      map_height <- 700

      center_lon <- mean(xlim)
      center_lat <- mean(ylim)

      lon_range <- max(diff(xlim), 1e-06)
      lat_range <- max(diff(ylim), 1e-06)

      zoom_lon <- log2((map_width * 360) / (lon_range * 512))
      zoom_lat <- log2((map_height * 170) / (lat_range * 512))
      zoom_level <- max(0, min(22, floor(min(zoom_lon, zoom_lat))))

      mapbox_url <- paste0(
        "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/static/",
        center_lon, ",", center_lat, ",", zoom_level,
        "/", map_width, "x", map_height,
        "?access_token=", mapbox_token
      )

      map_img_file <- tempfile(fileext = ".png")
      downloaded <- FALSE
      try(download.file(mapbox_url, map_img_file, mode = "wb", quiet = TRUE), silent = TRUE)
      if(file.exists(map_img_file) && file.info(map_img_file)$size > 0){
        downloaded <- TRUE
      }

      if(downloaded){
        map_img <- png::readPNG(map_img_file)
        plot(NA, xlim = xlim, ylim = ylim, xlab = "Longitude", ylab = "Latitude", axes = TRUE, asp = 1)
        rasterImage(map_img, xlim[1], ylim[1], xlim[2], ylim[2])
      } else {
        warning("MapBox tile download failed; falling back to maps basemap.")
        world_map <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
        plot(sf::st_geometry(world_map), col = "antiquewhite", border = "grey55", xlim = xlim, ylim = ylim, axes = TRUE)
      }
    } else {
      world_map <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
      plot(sf::st_geometry(world_map), col = "antiquewhite", border = "grey55", xlim = xlim, ylim = ylim, axes = TRUE)
    }

    graticule <- sf::st_graticule(set_sf, lon = pretty(xlim, 6), lat = pretty(ylim, 6))

    plot(sf::st_geometry(graticule), add = TRUE, col = "grey85", lty = 3)
    plot(sf::st_geometry(set_sf), add = TRUE, pch = 19, col = "blue")
    box()
    title(main = "SET_INFO set locations", xlab = "Longitude", ylab = "Latitude")

    invisible(set_sf)
  })
}
