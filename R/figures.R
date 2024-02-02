#' Correlations between COVID-19 cases and social vulnerabilities
#'
#' @export

figures <- function() {
  # ===========================================================================
  # Output
  out <- here::here("figures")
  chk_create(out)

  # Input
  input <- here::here("data", "data_format")

  # Data
  cases <- vroom::vroom(here::here(input, "covid", "cases.csv"))
  deaths <- vroom::vroom(here::here(input, "covid", "deaths.csv"))
  indicators <- vroom::vroom(here::here(input, "indicators", "indicators.csv"))

  dat <- list(cases, deaths, indicators) |>
    purrr::reduce(dplyr::left_join, by = "hruid") |>
    dplyr::select(-hruid)

  # Histograms
  histograms(dat, out)

  # Autocorrelation
  indicators |>
    dplyr::select(-hruid) |>
    autocorrelation(out)
}


# ===========================================================================
# Helpers
histograms <- function(dat, out) {
  name <- list(
    vroom::vroom(here::here(input, "covid", "covid_list.csv")),
    vroom::vroom(here::here(input, "indicators", "indicators_list.csv"))
  ) |>
    dplyr::bind_rows()

  dat <- dat |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::rename(file = name) |>
    dplyr::group_by(file) |>
    dplyr::mutate(value = (value / max(value)) * 100) |>
    dplyr::left_join(name[, c("file", "name")], by = "file")

  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(binwidth = 10) +
    ggplot2::facet_wrap(~name, scales = "free")

  ggplot2::ggsave(
    filename = "histograms.png",
    plot = p,
    device = "png",
    path = out,
    width = 100,
    height = 50,
    units = "cm"
  )
}

# ------------------------------------------------------------
autocorrelation <- function(dat, out) {
  name <- vroom::vroom(here::here(input, "indicators", "indicators_list.csv"))
  name$name <- stringr::str_replace(name$name, "((?:\\S*\\s){9}.*?)\\s", "\\$1\n")
  name$name <- stringr::str_replace(name$name, "\\\\", "")
  dat <- cor(dat) |>
    round(2)
  ndat <- ncol(dat)
  nm <- data.frame(file = colnames(dat)) |>
    dplyr::left_join(name, by = "file")
  cols <- abs(dat) * 100
  cols <- viridis::mako(101, direction = -1)[cols + 1]
  cols <- matrix(data = cols, ncol = ndat)

  png(
    here::here(out, "autocorrelation.png"),
    res = 300,
    width = (ndat * 10) + 500,
    height = (ndat * 10) + 500,
    units = "mm",
    pointsize = 10
  )
  par(
    mar = c(1, 1, 1, 1)
  )
  graphicsutils::plot0(x = c(-6, ndat), y = c(-6, ndat))
  for (j in seq_len(ndat)) {
    text(x = j, y = 0, adj = c(1, .5), labels = nm$name[j], srt = 90)
    text(x = 0, y = j, adj = c(1, .5), labels = nm$name[j])
    for (i in seq_len(ndat)) {
      points(x = j, y = i, pch = 22, bg = cols[j, i], col = "#00000000", cex = 9)
      text(x = j, y = i, labels = dat[j, i], col = "#ffffff", cex = 1.5)
    }
  }
  dev.off()
}
