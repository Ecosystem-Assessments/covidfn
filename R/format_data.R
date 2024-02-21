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
    as.Date(c("2021-11-01", "2022-03-11")), # Omicron
    as.Date(c("2020-01-01", "2022-03-11")) # Whole pandemic
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
  colnames(cases) <- stringr::str_replace_all(colnames(cases), "-", "_")
  colnames(deaths) <- stringr::str_replace_all(colnames(deaths), "-", "_")

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

  # Covid list
  files <- c(
    colnames(cases)[stringr::str_detect(colnames(cases), "cases")],
    colnames(deaths)[stringr::str_detect(colnames(deaths), "deaths")]
  )
  name <- files |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_sentence()
  df <- data.frame(
    dataset = "COVID-19 Timeline Canada",
    code = "",
    name = name,
    file = files
  )
  pipedat::masterwrite(df, here::here(out$covid, "covid_list"))


  # --------------------------------------------------------------------------------
  # Indicators data
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

  # Replace "-" for "_"
  names(indicators_ras) <- stringr::str_replace_all(names(indicators_ras), "-", "_")

  # Data tables
  indicators <- aggregate(indicators_ras, geo, FUN = mean, na.rm = TRUE, exact = TRUE) |>
    data.frame() |>
    dplyr::select(-geometry) |>
    dplyr::mutate(hruid = geo$hruid)
  colnames(indicators) <- tools::file_path_sans_ext(colnames(indicators))

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
      "census_road_network_file_2021_7daa23ee_distance_to_road_network",
      "open_database_healthcare_facilities_8b0bbc44_critical",
      "open_database_healthcare_facilities_8b0bbc44_longterm"
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

  # ---------------------------------------------------------------------------------------
  # Group data (Long and ugly, but I would rather have it all here)
  groups <- data.frame(
    file = c(
      "census_road_network_file_2021_7daa23ee_distance_to_road_network",
      "open_database_healthcare_facilities_8b0bbc44_critical",
      "open_database_healthcare_facilities_8b0bbc44_longterm",
      "GENDVHDI_poor_fair_perceived_health_percent",
      "GENDVMHI_poor_fair_perceived_mental_health_percent",
      "HWTDGBCC_overweight_obese_percent",
      "CCC_015_asthma_percent",
      "CCC_030_copd_percent",
      "CCC_065_high_blood_pressure_percent",
      "CCC_075_high_blood_cholesterol_lipids_percent",
      "CCC_085_heart_disease_percent",
      "CCC_095_diabetes_percent",
      "CCC_130_cancer_percent",
      "CCC_200_anxiety_disorder_percent",
      "SLPG005_hours_sleep_mean",
      "SMK_005_daily_smoker_percent",
      "SMK_020_more_100_cigarettes_lifetime_percent",
      "TALDVUSE_alternative_tobacco_product_usage_percent",
      "ALCDVTTM_regular_drinker_percent",
      "ALWDVLTR_increased_long_term_risk_due_to_drinking_percent",
      "ALWDVSTR_increased_short_term_risk_due_to_drinking_percent",
      "PAA_015_adult_active_transportation_hours_mean",
      "PAA_045_adult_sports_fitness_recreational_activities_hours_mean",
      "PAA_080_adult_other_physical_activities_minutes_mean",
      "PAA_105_adult_physical_activities_vigorous_intensity_minutes_mean",
      "PAADVTRV_adult_active_transportation_minutes_mean",
      "PAADVREC_adult_recreational_physical_activities_minutes_mean",
      "PAADVOTH_adult_other_physical_activities_minutes_mean",
      "PAADVMVA_adult_moderate_to_vigorous_physical_activities_minutes_mean",
      "PAADVVIG_adult_vigorous_physical_activities_minutes_mean",
      "PAADVVOL_adult_volume_of_weekly_activity_mean",
      "PAADVACV_adult_physically_active_below_recommended_level_from_CPAG_percent",
      "PAADVDYS_adult_physically_active_days_mean",
      "PAYDVTTR_youth_active_transportation_minutes_mean",
      "PAYDVMNS_youth_physical_activities_minutes_mean",
      "PAYDVDPG_youth_days_meeting_exceeding_physical_activity_guidelines_mean",
      "PAYDVVIG_youth_vigorous_physical_activity_mean",
      "FLU_005_seasonal_flu_shot_lifetime_percent",
      "LBFDGHPW_total_hours_work_week_mean",
      "FSCDVAFS_adult_moderately_severely_food_insecure_percent",
      "FSCDVCFS_child_moderately_severely_food_insecure_percent",
      "FSCDVHFS_household_moderately_severely_food_insecure_percent",
      "v_CA21_11",
      "v_CA21_251",
      "v_CA21_548",
      "v_CA21_566",
      "v_CA21_584",
      "v_CA21_596",
      "v_CA21_1040",
      "v_CA21_1085",
      "v_CA21_1142",
      "v_CA21_1143",
      "v_CA21_4204",
      "v_CA21_4274",
      "v_CA21_4293",
      "v_CA21_4401",
      "v_CA21_4875",
      "v_CA21_5868",
      "v_CA21_6579",
      "v_CA21_6588",
      "v_CA21_6591",
      "v_CA21_6594",
      "v_CA21_6597",
      "v_CA21_7644",
      "income",
      "education",
      "housing",
      "labour_force_activity",
      "cwb",
      "employment_prox",
      "pharma_prox",
      "childcare_prox",
      "health_prox",
      "grocery_prox",
      "educpri_prox",
      "educsec_prox",
      "library_prox",
      "park_prox",
      "transit_prox"
    ),
    group = c(
      "Geography",
      "Geography",
      "Geography",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Health",
      "Behaviour",
      "Behaviour",
      "Behaviour",
      "Behaviour",
      "Behaviour",
      "Behaviour",
      "Behaviour",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Adult physical activity",
      "Youth physical activity",
      "Youth physical activity",
      "Youth physical activity",
      "Youth physical activity",
      "Health",
      "Behaviour",
      "Nutrition",
      "Nutrition",
      "Nutrition",
      "Population descriptor",
      "Population descriptor",
      "Absolute deprivation",
      "Absolute deprivation",
      "Absolute deprivation",
      "Absolute deprivation",
      "Absolute deprivation",
      "Absolute deprivation",
      "Relative deprivation",
      "Relative deprivation",
      "Race and ethnicity",
      "Built environment",
      "Built environment",
      "Race and ethnicity",
      "Race and ethnicity",
      "Human capital",
      "Occupations",
      "Occupations",
      "Occupations",
      "Occupations",
      "Occupations",
      "Geography",
      "Wellbeing",
      "Wellbeing",
      "Wellbeing",
      "Wellbeing",
      "Wellbeing",
      "Geography",
      "Geography",
      "Geography",
      "Geography",
      "Geography",
      "Geography",
      "Geography",
      "Geography",
      "Geography",
      "Geography"
    )
  )

  ind_list <- dplyr::left_join(ind_list, groups, by = "file")
  stopifnot(!any(is.na(ind_list$group)))
  pipedat::masterwrite(ind_list, here::here(out$ind, "indicators_list"))
}
