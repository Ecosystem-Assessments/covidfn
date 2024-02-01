#' Format and prepare data for analyses
#'
#' @export

format_data <- function() {
  # Output
  out <- list()
  out$out <- here::here("data", "data_format")
  out$covid <- here::here(out$out, "covid")
  out$ind <- here::here(out$out, "indicators")
  out$geo <- here::here(out$out, "geo")
  lapply(out, chk_create)

  # --------------------------------------------------------------------------------
  # Covid data
  # Health region data
  library(stars)
  dat <- pipedat::importdat(c("a56e753b"), "format")

  # Geographies
  geo <- dat[["covid_timeline_canada-a56e753b-hr_wgs84.gpkg"]] |>
    dplyr::select(-name_canonical_fr) |>
    sf::st_make_valid()
  geo <- geo |>
    dplyr::mutate(area = units::set_units(sf::st_area(geo), km^2))
  xy <- sf::st_centroid(geo) |>
    sf::st_coordinates()
  colnames(xy) <- c("centroid_lon", "centroid_lat")
  geo <- cbind(geo, xy)

  # Tables
  hr <- dat[["covid_timeline_canada-a56e753b-hr.csv"]]
  covid <- dat[["covid_timeline_canada-a56e753b-CovidTimelineCanada_hr.csv"]]

  # 1. Covid data for a certain period (see pipedat script)
  # 2. Import social vulnerabilities
  # 3. Export values within each HR
  # 4. Export .csv with following structure
  # rows = HR
  # columns: province, health_region, population, cases, deaths, cases_pop, deaths_pop, indicators

  # COVID-19 cumulative cases and deaths for a specific period (manual for now, work in blackbox)
  dates <- list(
    as.Date(c("2020-01-01", "2020-10-31")), # Onset
    as.Date(c("2020-11-01", "2021-04-30")),
    as.Date(c("2021-05-01", "2021-10-31")),
    as.Date(c("2021-11-01", "2022-03-11")) # Omicron
  )

  dat <- list()
  dat$cases <- list()
  dat$deaths <- list()
  for (i in 1:length(dates)) {
    cases <- covid |>
      dplyr::filter(date >= dates[[i]][1] & date <= dates[[i]][2]) |>
      dplyr::group_by(name, sub_region_1) |>
      dplyr::summarize(value = max(value)) |>
      dplyr::ungroup()
    iid <- cases$name == "cases"
    deaths <- cases[!iid, ] |>
      dplyr::select(hruid = sub_region_1, deaths = value) |>
      dplyr::mutate(hruid = as.character(hruid))
    cases <- cases[iid, ] |>
      dplyr::select(hruid = sub_region_1, cases = value) |>
      dplyr::mutate(hruid = as.character(hruid))

    # Join to spatial data and divide by total population
    hr$hruid <- as.character(hr$hruid)

    cases <- dplyr::left_join(geo, cases, by = "hruid") |>
      dplyr::left_join(hr, by = "hruid") |>
      # For covid normalized by population size
      # dplyr::mutate(cases_pop = cases / pop) |>
      dplyr::select(
        region = region.x,
        hruid,
        name_canonical = name_canonical.x,
        population = pop,
        area,
        centroid_lon,
        centroid_lat,
        !!glue::glue("cases_{dates[[i]][1]}_{dates[[i]][2]}") := cases # ,
        # For covid normalized by population size
        # !!glue::glue("cases_pop_{dates[[i]][1]}_{dates[[i]][2]}") := cases_pop
      ) # |>
    # stars::st_rasterize()

    deaths <- dplyr::left_join(geo, deaths, by = "hruid") |>
      dplyr::left_join(hr, by = "hruid") |>
      # For covid normalized by population size
      # dplyr::mutate(deaths_pop = deaths / pop) |>
      dplyr::select(
        region = region.x,
        hruid,
        name_canonical = name_canonical.x,
        population = pop,
        area,
        centroid_lon,
        centroid_lat,
        !!glue::glue("deaths_{dates[[i]][1]}_{dates[[i]][2]}") := deaths # ,
        # For covid normalized by population size
        # !!glue::glue("deaths_pop_{dates[[i]][1]}_{dates[[i]][2]}") := deaths_pop
      ) |>
      sf::st_drop_geometry()
    # stars::st_rasterize()

    dat$cases[[i]] <- sf::st_drop_geometry(cases)
    dat$deaths[[i]] <- sf::st_drop_geometry(deaths)
    # dat[[i]] <- dplyr::left_join(cases, deaths, by = c("region", "health_region", "name_canonical", "population")) #|>
    # sf::st_drop_geometry()
  }

  # Data tables
  cases <- dat$cases |>
    purrr::reduce(
      dplyr::left_join,
      by = c("region", "hruid", "name_canonical", "population", "area", "centroid_lon", "centroid_lat")
    ) |>
    dplyr::mutate(population_density = population / area)
  deaths <- dat$deaths |>
    purrr::reduce(
      dplyr::left_join,
      by = c("region", "hruid", "name_canonical", "population", "area", "centroid_lon", "centroid_lat")
    ) |>
    dplyr::mutate(population_density = population / area)

  # Spatial vectors
  cases_sf <- dplyr::left_join(geo, cases, by = c("region", "hruid", "name_canonical"))
  deaths_sf <- dplyr::left_join(geo, deaths, by = c("region", "hruid", "name_canonical"))
  pipedat::masterwrite(cases_sf, here::here(out$covid, "cases"))
  pipedat::masterwrite(deaths_sf, here::here(out$covid, "deaths"))

  # Spatial rasters
  bb <- sf::st_bbox(geo)
  rt <- raster::raster(
    xmn = bb$xmin, ymn = bb$ymin,
    xmx = bb$xmax, ymx = bb$ymax,
    crs = sf::st_crs(dat)$epsg,
    res = .1
  ) |>
    stars::st_as_stars()

  # Rasterize
  cases_ras <- dplyr::select(cases_sf, dplyr::contains("cases")) |>
    stars::st_rasterize(rt) |>
    pipedat::masteringrid() |>
    merge()
  deaths_ras <- dplyr::select(deaths_sf, dplyr::contains("deaths")) |>
    stars::st_rasterize(rt) |>
    pipedat::masteringrid() |>
    merge()
  pipedat::masterwrite(cases_ras, here::here(out$covid, "cases"))
  pipedat::masterwrite(deaths_ras, here::here(out$covid, "deaths"))

  # Export data tables
  geodat <- cases |>
    dplyr::select(hruid, region, name_canonical, population, area, centroid_lon, centroid_lat, population_density)
  cases <- dplyr::select(cases, hruid, dplyr::contains("cases")) 
  deaths <- dplyr::select(deaths, hruid, dplyr::contains("deaths")) 
  pipedat::masterwrite(cases, here::here(out$covid, "cases"))
  pipedat::masterwrite(deaths, here::here(out$covid, "deaths"))
  pipedat::masterwrite(geodat, here::here(out$geo, "geo"))


  # --------------------------------------------------------------------------------
  # Indicators  data
  pload <- function(path) {
    dat <- dir(path, full.names = TRUE) |>
      lapply(stars::read_stars)
  }

  # Import and move tif files to data_format folder
  p <- here::here("data", "pipedat")
  pl <- here::here("data", "pipedat_light")
  indicators_ras <- c(
    pload(here::here(p, "census_road_network_file_2021-7daa23ee", "ingrid")),
    pload(here::here(p, "open_database_healthcare_facilities-8b0bbc44", "ingrid")),
    pload(here::here(pl, "cchs", "ingrid")),
    pload(here::here(pl, "canadian_census", "ingrid")),
    pload(here::here(pl, "community_wellbeing_index", "ingrid")),
    pload(here::here(pl, "proximity_measures_database", "ingrid"))
  )
  indicators_ras <- do.call("c", indicators_ras)

  # Remove raster on road network
  uid <- names(indicators_ras) == "census_road_network_file_2021-7daa23ee-road_network.tif"
  indicators_ras <- indicators_ras[!uid]

  # Data tables
  indicators <- aggregate(indicators_ras, geo, FUN = mean, na.rm = TRUE, exact = TRUE) |>
    data.frame() |>
    dplyr::select(-geometry) |>
    dplyr::mutate(hruid = geo$hruid)

  # Single band for indicators
  indicators_ras <- merge(indicators_ras)

  # List of indicators
  # Manual for road network and healthcare
  df <- data.frame(
    dataset = c(
      "Census road network 2021",
      rep("Open database on healthcare facilities", 2)
    ),
    code = "",
    name = c(
      "Distance to road network",
      "Distance to critical healthcare facilities",
      "Distance to longterm healthcare facilities"
    ),
    file = c(
      "census_road_network_file_2021-7daa23ee-distance_to_road_network",
      "open_database_healthcare_facilities-8b0bbc44-critical",
      "open_database_healthcare_facilities-8b0bbc44-longterm"
    )
  )

  ind_list <- list(
    df,
    vroom::vroom(here::here(pl, "cchs", "cchs_list.csv")),
    vroom::vroom(here::here(pl, "canadian_census", "census_list.csv")),
    vroom::vroom(here::here(pl, "community_wellbeing_index", "community_wellbeing_index_list.csv")),
    vroom::vroom(here::here(pl, "proximity_measures_database", "proximity_measures_database_list.csv"))
  ) |>
    dplyr::bind_rows()

  # Export
  pipedat::masterwrite(indicators, here::here(out$ind, "indicators"))
  pipedat::masterwrite(indicators_ras, here::here(out$ind, "indicators"))
  pipedat::masterwrite(ind_list, here::here(out$ind, "indicators_list"))
}
