#' Script to get community wellbeing index
#' @export

community_wellbeing_index <- function() {
  # Output folders and other objects used
  out <- here::here("data", "pipedat_light", "community_wellbeing_index")
  pipedat::chk_create(out)

  # Download data
  govcan <- "56578f58-a775-44ea-9cc5-9bf7c78410e6"
  pipedat::pipeload(
    govcan = govcan,
    output = out,
    large = FALSE
  )

  # Import data
  # File names
  files <- dir(out, pattern = "_eng.csv")
  files <- files[!stringr::str_detect(files, "DICT_")]
  years <- substr(files, 5, 8) |>
    as.numeric()
  files <- files[!is.na(years)]
  years <- years[!is.na(years)]

  # Import and adjust column names
  modif <- data.frame(
    from = c(
      "CSD Code",
      "CSD Name",
      "2A Population",
      "Income",
      "Education",
      "Housing",
      "Labour Force Activity",
      "CWB",
      "Community Type",
      "Census Population",
      "GNR"
    ),
    to = c(
      "csd_code",
      "csd_name",
      "population",
      "income",
      "education",
      "housing",
      "labour_force_activity",
      "cwb",
      "community_type",
      "population",
      "gnr"
    )
  )

  dat <- list()
  for (i in 1:length(files)) {
    # -----
    dat[[i]] <- utils::read.csv(
      here::here(out, files[i]),
      header = FALSE,
      skip = 1
    )

    # -----
    colnm <- readLines(here::here(out, files[i]))[1] |>
      stringr::str_split(pattern = ",") |>
      unlist()
    colnm <- gsub(" /(.*)", "", colnm)
    for (j in 1:nrow(modif)) colnm <- gsub(modif$from[j], modif$to[j], colnm)
    colnames(dat[[i]]) <- colnm

    # -----
    dat[[i]] <- dplyr::mutate(
      dat[[i]],
      community_type = gsub(" /(.*)", "", community_type),
      year = years[i]
    )
  }

  dat <- dplyr::bind_rows(dat)

























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
  nm <- datNames
  for (i in seq_len(length(r))) pipedat::masterwrite(r[[i]], here::here(out, nm[i]))
}
