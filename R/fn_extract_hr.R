#' Extract values of social vulnerabilities for Health Regions across Canada
#'
#' @export

fn_extract_hr <- function() {
  # Health region data
  library(stars)
  dat <- pipedat::importdat(c("a56e753b"), "format")
  geo <- dat[["covid_timeline_canada-a56e753b-hr_wgs84.gpkg"]]
  hr <- dat[["covid_timeline_canada-a56e753b-hr.csv"]]
  covid <- dat[["covid_timeline_canada-a56e753b-CovidTimelineCanada_hr.csv"]]

  # DONE 1. Covid data for a certain period (see pipedat script)
  # 2. Import social vulnerabilities
  # 3. Export values within each HR
  # 4. Export .csv with following structure
  # rows = HR
  # columns: province, health_region, population, cases, deaths, cases_pop, deaths_pop, indicators

  # COVID-19 cumulative cases and deaths for a specific period (manual for now, work in blackbox)
  dates <- as.Date(c("2021-10-01", "2022-03-31"))
  cases <- covid |>
    dplyr::filter(date >= dates[1] & date <= dates[2]) |>
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
    dplyr::mutate(cases_pop = cases / pop) |>
    dplyr::select(
      region = region.x,
      health_region = hruid,
      name_canonical = name_canonical.x,
      population = pop,
      cases = cases,
      cases_pop
    ) # |>
  # stars::st_rasterize()

  deaths <- dplyr::left_join(geo, deaths, by = "hruid") |>
    dplyr::left_join(hr, by = "hruid") |>
    dplyr::mutate(deaths_pop = deaths / pop) |>
    dplyr::select(
      region = region.x,
      health_region = hruid,
      name_canonical = name_canonical.x,
      population = pop,
      deaths = deaths,
      deaths_pop
    ) |>
    sf::st_drop_geometry()
  # stars::st_rasterize()

  covid <- dplyr::left_join(cases, deaths, by = c("region", "health_region", "name_canonical", "population"))

  # Import indicators of social vulnerability
  vulnerabilities <- here::here("data", "pipegrid") |>
    dir(full.names = TRUE) |>
    lapply(stars::read_stars)

  # Fix mistake in pipeline
  # WARNING: pipelines from `pipedat` will have to be corrected
  sf::st_crs(vulnerabilities[[13]]) <- sf::st_crs(vulnerabilities[[12]])
  vulnerabilities[[13]] <- st_set_dimensions(vulnerabilities[[13]], "x", point = FALSE)
  vulnerabilities[[13]] <- st_set_dimensions(vulnerabilities[[13]], "y", point = FALSE)

  # Subset (quick and dirty for now)
  nm <- c(
    "housing_acceptability_canada-175ec912-acceptable_housing.tif",
    "housing_acceptability_canada-175ec912-dwelling_condition.tif",
    "housing_acceptability_canada-175ec912-housing_suitability.tif",
    "open_database_healthcare_facilities-8b0bbc44-critical.tif",
    "open_database_healthcare_facilities-8b0bbc44-longterm.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-gini_index_adj_household_total_income.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-low_income_cutoffs_aftertax_percent.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-low_income_measure_aftertax_percent.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-p90p10_ratio_ajd_household_aftertax_income.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-percent_children_one_parent_family.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-percent_government_transfers.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-percent_indigenous_identity.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-percent_no_certificate_diploma_degree.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-percent_parent_one_parent_family.tif",
    "canadian_vulnerabilities_census-8671c3e4-census_division-unemployment_rate.tif",
    "census_road_network_file_2021-7daa23ee-distance_to_road_network.tif"
  )

  # Single object
  vulnerabilities <- do.call("c", vulnerabilities)
  vulnerabilities <- vulnerabilities[nm]

  # Extract mean value of each indicator for each health region
  ind <- aggregate(vulnerabilities, covid, FUN = mean, na.rm = TRUE, exact = TRUE) |>
    data.frame() |>
    dplyr::select(-geometry)

  # Join with covid data
  covid <- cbind(covid, ind)
  colnames(covid) <- gsub(".tif", "", colnames(covid))

  # Export
  out <- here::here("output", "covid_data")
  chk_create(out)
  sf::st_write(covid, dsn = here::here(out, "covid.gpkg"))
  covid <- sf::st_drop_geometry(covid)
  write.csv(covid, file = here::here(out, "covid.csv"), row.names = FALSE)
}
