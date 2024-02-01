#' Script to get community wellbeing index
#' @export

community_wellbeing_index <- function() {
  # Output folders and other objects used
  out <- here::here("data", "pipedat_light", "community_wellbeing_index")
  out_raw <- here::here(out, "raw")
  pipedat::chk_create(out_raw)

  # Download data
  govcan <- "56578f58-a775-44ea-9cc5-9bf7c78410e6"
  pipedat::pipeload(
    govcan = govcan,
    output = out_raw,
    large = FALSE
  )

  # Import data
  # File names
  files <- dir(out_raw, pattern = "_eng.csv")
  files <- files[!stringr::str_detect(files, "DICT_")]
  files <- files[!stringr::str_detect(files, "Time_series")]
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
      "census_population",
      "gnr"
    )
  )

  dat <- list()
  for (i in 1:length(files)) {
    # -----
    dat[[i]] <- utils::read.csv(
      here::here(out_raw, files[i]),
      header = FALSE,
      skip = 1
    )

    # -----
    colnm <- readLines(here::here(out_raw, files[i]))[1] |>
      stringr::str_split(pattern = ",") |>
      unlist()
    for (j in seq_len(length(colnm))) {
      nmid <- stringr::str_detect(colnm[j], modif$from)
      colnm[j] <- modif$to[nmid]
    }
    colnames(dat[[i]]) <- colnm

    # -----
    dat[[i]] <- dplyr::mutate(
      dat[[i]],
      year = years[i]
    )
  }
  dat <- dplyr::bind_rows(dat)

  # -----------------------------------
  # WARNING
  # Remove this line to keep all years
  dat <- dplyr::filter(dat, year == 2016)
  # -----------------------------------

  # Keep only relevant coloumns
  dat <- dplyr::select(dat, csd_code, income, education, housing, labour_force_activity, cwb)


  # ---------------------------------------------------------------------------------------------
  # Spatial data
  geo <- pipedat::importdat("5e4be996", "format")
  geo <- geo[[1]] |>
    dplyr::mutate(CSDUID = as.numeric(CSDUID)) |>
    dplyr::select(CSDUID)

  # Join with cwi data
  cwi <- dplyr::left_join(geo, dat, by = c("CSDUID" = "csd_code")) |>
    dplyr::select(-CSDUID)

  # Grid
  datNames <- colnames(sf::st_drop_geometry(cwi))
  bb <- sf::st_bbox(cwi)
  rt <- raster::raster(
    xmn = bb$xmin, ymn = bb$ymin,
    xmx = bb$xmax, ymx = bb$ymax,
    crs = sf::st_crs(cwi)$epsg,
    res = 1000
  ) |>
    stars::st_as_stars()

  # Rasterize
  r <- list()
  for (i in seq_len(length(datNames))) {
    r[[i]] <- stars::st_rasterize(cwi[, datNames[i]], rt) |>
      pipedat::masteringrid()
  }

  # Data list
  name <- stringr::str_replace_all(datNames, "_", " ") |>
    stringr::str_to_sentence()
  df <- data.frame(
    dataset = "Community Wellbeing Index",
    code = "",
    name = name,
    file = datNames
  )


  # ------------------------------------------------------------------------------------
  # Export
  # Vector data
  sf::st_write(cwi, here::here(out, "community_wellbeing_index.gpkg"), quiet = TRUE, append = FALSE)
  pipedat::masterwrite(df, here::here(out, "community_wellbeing_index_list"))

  # Gridded data
  out <- here::here(out, "ingrid")
  pipedat::chk_create(out)
  for (i in seq_len(length(r))) pipedat::masterwrite(r[[i]], here::here(out, datNames[i]))
}
