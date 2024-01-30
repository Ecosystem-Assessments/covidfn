#' Script to get proximity measures database
#'
#' @export

proximity_measures_database <- function() {
  # Output folders and other objects used
  out <- here::here("data", "pipedat_light", "proximity_measures_database")
  pipedat::chk_create(out)

  # Download data
  pipedat::pipeload(
    urls = "https://ftp.maps.canada.ca/pub/statcan_statcan/Services_Service/proximity_measures_mesures_proximite/proximity_measures_database_en.gdb.zip",
    output = out,
    large = TRUE
  )

  # Import data
  filepath <- here::here(out, "proximity_measures_database_en.gdb")
  dat <- sf::st_read(filepath, layer = "proximity_measures_database", quiet = TRUE)
  # pts <- sf::st_read(filepath, layer = "proximity_measures_database_centroids")

  # Prepare names and template to integrate in grid
  datNames <- colnames(dat)[stringr::str_detect(colnames(dat), "_prox")]
  bb <- sf::st_bbox(dat)
  rt <- raster::raster(
    xmn = bb$xmin, ymn = bb$ymin,
    xmx = bb$xmax, ymx = bb$ymax,
    crs = sf::st_crs(dat)$epsg,
    res = 500
  ) |>
    stars::st_as_stars()

  # Rasterize
  r <- list()
  for (i in seq_len(length(datNames))) {
    r[[i]] <- stars::st_rasterize(dat[, datNames[i]], rt) |>
      pipedat::masteringrid()
  }

  # Export
  out <- here::here(out, "ingrid")
  pipedat::chk_create(out)
  nm <- datNames
  for (i in seq_len(length(r))) pipedat::masterwrite(r[[i]], here::here(out, nm[i]))
}
