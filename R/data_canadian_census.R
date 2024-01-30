#' Script to get canadian census data
#'
#' @export

canadian_census <- function() {
  # Outout folder
  out <- here::here("data", "pipedat_light", "canadian_census")
  pipedat::chk_create(out)

  # Dataset
  dataset <- "CA21" # 2021 Census
  geolvl <- "CD" # Census division level
  region <- cancensus::list_census_regions(dataset) |>
    dplyr::filter(
      level == geolvl # &
      # PR_UID == 24 # To remove
    )

  # Indicators
  vector_list <- cancensus::list_census_vectors(dataset) |>
    dplyr::select(vector, label)
  vectors <- data.frame(vector = c(
    "8", # Total Age
    "11", # Age 0 - 14
    "251", # Age 65+
    "543", "548", # One-parent-family households
    "566", # Median income
    "584", # Median government transfers
    "596", # Median COVID-19 benefits
    "1040", # Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)
    "1085", # Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)
    "1142", # Gini index on adjusted household after-tax income
    "1143", # P90/P10 ratio on adjusted household after-tax income
    "4201", "4204", # Indigenous identity for the population in private households
    "4272", "4274", # Dwelling condition
    "4292", "4293", # Housing indicators: unsuitable housing
    "4389", "4401", # Non-canadian citizens
    "4872", "4875", # Visible minority
    "5865", "5868", # No certificate 25 - 64 years
    "6561", # Total occupation
    "6579", # Health occupation
    "6588", # Sales and services
    "6591", # Trades, transport and equipment operators and related occupations
    "6594", # Natural resources, agriculture and related production occupations
    "6597", # Occupations in manufacturing and utilities
    "7632", "7644" # Public transit main mode of commuting to work
  )) |>
    dplyr::mutate(vector = glue::glue("v_CA21_{vector}")) |>
    dplyr::left_join(vector_list, by = "vector")

  # Get data
  dat <- cancensus::get_census(
    dataset = dataset,
    regions = list(CD = region$region),
    vectors = vectors$vector, # If empty, only geometries are returned
    geo_format = "sf", # Set to NA for data.frames only
    resolution = "simplified", # Set to high for high resolution
    quiet = FALSE # Set to TRUE once everything is properly setup
  )

  # Function to locate column of interest
  lc <- function(uid) {
    stringr::str_detect(
      names(dat),
      glue::glue("v_CA21_{uid}:")
    ) |>
      which()
  }

  # Wrapper to use only data column w/o geometries
  d <- function(uid) dat[, lc(uid), drop = TRUE]

  # Transform for proportions where relevant
  dat[, lc("11")] <- d("11") / d("8") # Age 0 - 14
  dat[, lc("251")] <- d("251") / d("8") # Age 65+
  dat[, lc("548")] <- d("548") / d("543") # One-parent-family households
  dat[, lc("4204")] <- d("4204") / d("4201") # Indigenous identity
  dat[, lc("4274")] <- d("4274") / d("4272") # Dwelling condition
  dat[, lc("4293")] <- d("4293") / d("4292") # Unsuitable housing
  dat[, lc("4401")] <- d("4401") / d("4389") # Non-canadian citizens
  dat[, lc("4875")] <- d("4875") / d("4872") # Visible minority
  dat[, lc("5868")] <- d("5868") / d("5865") # No certificate 25 - 64 years
  dat[, lc("6579")] <- d("6579") / d("6561") # Health occupation
  dat[, lc("6588")] <- d("6588") / d("6561") # Sales and services
  dat[, lc("6591")] <- d("6591") / d("6561") # Trades, transport and equipment operators and related occupations
  dat[, lc("6594")] <- d("6594") / d("6561") # Natural resources, agriculture and related production occupations
  dat[, lc("6597")] <- d("6597") / d("6561") # Occupations in manufacturing and utilities
  dat[, lc("7644")] <- d("7644") / d("7632") # Public transit main mode of commuting to work

  # Select only relevant variables
  uid <- glue::glue('_{c("8", "543", "4201", "4272", "4292", "4389", "4872", "5865", "6561", "7632")}:') |>
    lapply(function(x) {
      stringr::str_detect(colnames(dat), x) |>
        which()
    }) |>
    unlist()
  dat <- dat[, -uid]

  # Export
  sf::st_write(dat, here::here(out, "canadian_census.gpkg"), quiet = TRUE, append = FALSE)

  # Prepare names and template to integrate in grid
  datNames <- colnames(dat)[stringr::str_detect(colnames(dat), ":")]
  bb <- sf::st_bbox(dat)
  rt <- raster::raster(
    xmn = bb$xmin, ymn = bb$ymin,
    xmx = bb$xmax, ymx = bb$ymax,
    crs = sf::st_crs(dat)$epsg,
    res = .01
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
  nm <- stringr::str_split(datNames, ":")
  for (i in seq_len(length(nm))) nm[[i]] <- nm[[i]][1]
  for (i in seq_len(length(r))) pipedat::masterwrite(r[[i]], here::here(out, nm[i]))
}
