#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  # Load federal bioregions
  dat <- pipefn:::basemap$can |>
         sf::st_make_valid()

  # Export data
  out <- here::here("data","aoi")
  chk_create(out)
  sf::st_write(
    obj = dat,
    dsn = here::here(out, "aoi.gpkg"),
    delete_dsn = TRUE,
    quiet = TRUE
  )    
}
