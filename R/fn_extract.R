#' Extract values of social vulnerabilities for first nations and inuit communities
#'
#' @export

fn_extract <- function() {
  library(stars)
  dat <- pipedat::importdat(c("ce594316", "621e9a76", "b5433840","92230392"), "format")
  vulnerabilities <- here::here("data","pipegrid") |>
                     dir(full.names = TRUE) |>
                     lapply(stars::read_stars)
  
  # Filter canadian cities from geographical places
  nm <- "canadian_geographical_names-92230392.gpkg"
  dat[[nm]] <- dplyr::filter(
    dat[[nm]],
    Generic.Category == "Populated Place"
  )
  
  # Fix mistake in pipeline 
  # WARNING: pipelines from `pipedat` will have to be corrected
  sf::st_crs(vulnerabilities[[13]]) <- sf::st_crs(vulnerabilities[[12]])
  vulnerabilities[[13]] <- st_set_dimensions(vulnerabilities[[13]], "x", point = FALSE)
  vulnerabilities[[13]] <- st_set_dimensions(vulnerabilities[[13]], "y", point = FALSE)

  # Single object
  vulnerabilities <- do.call("c", vulnerabilities) 

  # Extract values 
  dat <- lapply(dat, function(x) {
    x <- sf::st_transform(x, sf::st_crs(vulnerabilities))
    x <- stars::st_extract(vulnerabilities, x)
  })

  # Export 
  out <- here::here("output","fn_extract")
  chk_create(out)
  for(i in 1:length(dat)){
    sf::st_write(
      dat[[i]], 
      dsn = here::here(out, names(dat)[i]),
      quiet = TRUE,
      overwrite = TRUE,
      append = FALSE
    )
    
    dat[[i]] <- sf::st_drop_geometry(dat[[i]])
    nm <- glue::glue("{tools::file_path_sans_ext(names(dat)[i])}.csv")
    write.csv(
      dat[[i]], 
      file = here::here(out, nm)
    )
  }  
}
