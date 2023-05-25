#' Script to prepare and export data for shiny application
#'
#' @export

app_data <- function() {
  out <- here::here("pubs","app","data")
  # Locations with covid cases and social indicators
  # NOTE: simply copy files
  pipedat::chk_create(here::here(out, "pts"))
  file.copy(
    from = here::here("output","fn_extract","first_nations_location-ce594316.csv"),
    to = here::here(out,"pts","first_nations_location-ce594316.csv")
  )
  
  # Tif files 
  grd <- sf::st_read(here::here("data","aoi","aoi.gpkg"), quiet = TRUE) |>
         sf::st_transform(crs = 3857) |>
         stars::st_rasterize(cellsize = 20000, dy = 20000)
  files <- dir(here::here("data","pipegrid"), full.names = TRUE)
  
  dat <- lapply(files, function(x) {
           stars::read_stars(x) |>
           stars::st_warp(grd)
         })
  pipedat::chk_create(here::here(out, "tifs"))
  for(i in 1:length(dat)) {
    stars::write_stars(dat[[i]], here::here(out, "tifs", basename(files[i])))
  }
     
  # Metadata with filenames   
  files <- list.dirs(here::here("data","pipedat"), full.names = TRUE)
  iid <- stringr::str_detect(files, "ingrid")
  files <- files[iid]

  meta <- list()
  for(i in 1:length(files)) {
    tmp <- dir(paste0(files[i], "/.."), full.names = TRUE, pattern = "yaml") |>
      yaml::read_yaml()  
      
    meta[[i]] <- data.frame(
      filenames = tmp$ingrid$files$filenames,
      names = tmp$ingrid$files$names
    )
  }
  
  meta <- dplyr::bind_rows(meta) |>
    dplyr::mutate(
      tiffiles = paste0(filenames,".tif"),
      dotfiles = stringr::str_replace_all(tiffiles, "-",".")
    ) 
  
  write.csv(meta, file = here::here(out, "metadata.csv"), row.names = FALSE)
}
