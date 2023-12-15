#' Correlations between COVID-19 cases and social vulnerabilities
#'
#' @export

out_base_stats <- function() {
  out <- here::here("output", "covid_data")
  covid <- read.csv("output/covid_data/covid.csv")

  # --------------------------------------------------
  # Histograms
  # --------------------------------------------------
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::group_by(name) |>
    dplyr::mutate(value = (value / max(value)) * 100)

  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(binwidth = 10) +
    ggplot2::facet_wrap(~name, scales = "free")

  ggplot2::ggsave(
    filename = "histograms.png",
    plot = p,
    device = "png",
    path = out,
    width = 50,
    height = 50,
    units = "cm"
  )

  # --------------------------------------------------
  # Data transformations
  # --------------------------------------------------
  trs <- c(
    "cases_pop",
    "deaths_pop",
    "distance_critical_healthcare",
    "distance_longterm_healthcare",
    "distance_road_network",
    "dwelling_condition",
    "housing_suitability",
    "percent_children_one_parent_family",
    "percent_indigenous_identity",
    "percent_no_certificate_diploma_degree",
    "population"
  )
  uid <- lapply(trs, function(x) stringr::str_detect(colnames(covid), x)) |>
    data.frame() |>
    rowSums() |>
    as.logical()
  covid[, uid] <- apply(covid[, uid], 2, function(x) log(x + 1))

  # --------------------------------------------------
  # Histograms - transformed data
  # --------------------------------------------------
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::group_by(name) |>
    dplyr::mutate(value = (value / max(value)) * 100)

  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(binwidth = 10) +
    ggplot2::facet_wrap(~name, scales = "free")

  ggplot2::ggsave(
    filename = "histograms_transformed.png",
    plot = p,
    device = "png",
    path = out,
    width = 50,
    height = 50,
    units = "cm"
  )

  # --------------------------------------------------
  # Scale indicator data
  # --------------------------------------------------
  covid$longitude <- (covid$longitude - min(covid$longitude)) / diff(range(covid$longitude))
  trs <- c("region", "health_region", "name_canonical", "population", "cases", "death")
  uid <- lapply(trs, function(x) stringr::str_detect(colnames(covid), x)) |>
    data.frame() |>
    rowSums() |>
    as.logical()
  covid[, !uid] <- apply(covid[, !uid], 2, function(x) x / max(x))


  # --------------------------------------------------
  # Indicator autocorrelation
  # --------------------------------------------------
  ind <- covid[, !uid]
  dat <- cor(ind) |>
    round(2)
  ndat <- ncol(dat)
  nm <- colnames(dat)
  cols <- abs(dat) * 100
  cols <- viridis::mako(101, direction = -1)[cols]
  cols <- matrix(data = cols, ncol = ndat)

  png(
    here::here(out, "autocorrelation.png"),
    res = 300,
    width = (ndat * 10) + 100,
    height = (ndat * 10) + 100,
    units = "mm",
    pointsize = 10
  )
  par(
    mar = c(1, 1, 1, 1)
  )
  graphicsutils::plot0(x = c(-6, ndat), y = c(-6, ndat))
  for (j in seq_len(ndat)) {
    text(x = j, y = 0, adj = c(1, .5), labels = nm[j], srt = 90)
    text(x = 0, y = j, adj = c(1, .5), labels = nm[j])
    for (i in seq_len(ndat)) {
      points(x = j, y = i, pch = 22, bg = cols[j, i], col = "#00000000", cex = 7)
      text(x = j, y = i, labels = dat[j, i], col = "#ffffff")
    }
  }
  dev.off()


  # --------------------------------------------------
  # Correlations
  # --------------------------------------------------
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical) |>
    as.matrix() |>
    cor(method = "spearman") |>
    as.data.frame()

  out <- here::here("output", "covid_data")
  write.csv(dat, file = here::here(out, "correlations.csv"))

  # Cases
  uid <- stringr::str_detect(colnames(dat), "cases_pop")
  cases <- dat[, uid]
  cases <- round(cases, 4)
  write.csv(cases, file = here::here(out, "correlations_cases.csv"))

  # Deaths
  uid <- stringr::str_detect(colnames(dat), "deaths_pop")
  deaths <- dat[, uid]
  deaths <- round(deaths, 4)
  write.csv(deaths, file = here::here(out, "correlations_deaths.csv"))

  # --------------------------------------------------
  # Scatterplots
  # --------------------------------------------------
  # Make this ugly, use two nested loops
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical, -population)

  # Cases and deaths per population size
  cases <- stringr::str_detect(colnames(dat), "cases_pop")
  death <- stringr::str_detect(colnames(dat), "deaths_pop")

  # Indicators
  ind <- stringr::str_detect(colnames(dat), "cases") +
    stringr::str_detect(colnames(dat), "deaths")
  ind <- !as.logical(ind)

  # Rows & columns
  cols <- c(which(cases), which(death))
  rows <- which(ind)
  nm <- colnames(dat)

  png(
    here::here(out, "scatterplots.png"),
    res = 300,
    width = length(cols) * 75,
    height = length(rows) * 75,
    units = "mm",
    pointsize = 10
  )
  par(
    mfrow = c(length(rows), length(cols)),
    mar = c(4, 4, 2, 2)
  )

  for (j in rows) {
    for (i in cols) {
      plot(
        x = dat[, j],
        y = dat[, i],
        pch = 21,
        cex = 1,
        col = "#46a6ac",
        bg = "#46a6ac",
        xlab = nm[j],
        ylab = nm[i]
      )
    }
  }
  dev.off()

  # --------------------------------------------------
  # Linear regressions
  # --------------------------------------------------
  nM <- length(rows) * length(cols)
  mods <- data.frame(
    x = character(nM),
    y = character(nM),
    coef = numeric(nM),
    p = numeric(nM),
    r2 = numeric(nM)
  )

  k <- 1
  for (j in 1:length(rows)) {
    for (i in 1:length(cols)) {
      ev <- lm(dat[, cols[i]] ~ dat[, rows[j]])
      sev <- summary(ev)
      mods$x[k] <- nm[rows[j]]
      mods$y[k] <- nm[cols[i]]
      mods$coef[k] <- sev$coefficients[2, "Estimate"]
      mods$p[k] <- sev$coefficients[2, "Pr(>|t|)"]
      mods$r2[k] <- sev$adj.r.squared
      k <- k + 1
    }
  }

  tmp <- dplyr::mutate(mods,
    coef = round(coef, 4),
    p = round(p, 4),
    r2 = round(r2, 4)
  ) |>
    tidyr::pivot_wider(
      # id_cols = c("x","y"),
      names_from = c("y"),
      values_from = c("coef", "p", "r2")
    )

  write.csv(tmp, here::here(out, "regression.csv"), row.names = FALSE)
}
