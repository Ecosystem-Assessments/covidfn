#' Wave rates within each Health Regions in Canada
#'
#' @export

out_rates <- function() {
  dat <- pipedat::importdat(c("a56e753b"), "format")
  geo <- dat[["covid_timeline_canada-a56e753b-hr_wgs84.gpkg"]]
  hr <- dat[["covid_timeline_canada-a56e753b-hr.csv"]]
  covid <- dat[["covid_timeline_canada-a56e753b-CovidTimelineCanada_hr.csv"]]

  # Calculate rates
  # Growth rate = (Present value - Past value)/ Past Value * 100
  dat <- covid |>
    dplyr::arrange(name, sub_region_1, date) |>
    dplyr::group_by(name, sub_region_1) |>
    dplyr::mutate(
      growth = value_daily / dplyr::lag(value_daily),
      id = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(cols = viridis::viridis(max(id))[id])


  # Libraries
  library(ggplot2)

  # Filter names
  don <- dat |>
    dplyr::filter(name == "cases")

  # Plot
  don |>
    ggplot(aes(x = date, y = growth)) +
    geom_line() +
    facet_wrap(~sub_region_1)
}
