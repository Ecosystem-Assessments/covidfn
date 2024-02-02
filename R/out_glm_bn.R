#' General linear models with negative binomial distribution
#'
#' @export

out_glm_nb <- function() {
  # ===========================================================================
  # Output
  out <- here::here("output")
  chk_create(out)

  # Input
  input <- here::here("data", "data_format")

  # Data & ordering (precautionary)
  tmpload <- function(path) {
    vroom::vroom(path) |>
      dplyr::arrange(hruid)
  }
  cases <- tmpload(here::here(input, "covid", "cases.csv"))
  deaths <- tmpload(here::here(input, "covid", "deaths.csv"))
  indicators <- tmpload(here::here(input, "indicators", "indicators.csv"))
  geo <- tmpload(here::here(input, "geo", "geo.csv"))

  # Check all hruid are the same
  stopifnot(all(cases$hruid == indicators$hruid))
  stopifnot(all(deaths$hruid == indicators$hruid))
  stopifnot(all(geo$hruid == indicators$hruid))

  # Remove hruid from datasets
  remid <- function(dat) dplyr::select(dat, -hruid)
  cases <- remid(cases)
  deaths <- remid(deaths)
  indicators <- remid(indicators)
  geo <- remid(geo)

  # Scale indicators data
  indicators <- apply(indicators, 2, function(x) x / max(x, na.rm = TRUE))

  # Statistical analyses
  cases_mods <- glm_bn_run(
    y = cases,
    x = indicators,
    off_data = geo[, "population", drop = FALSE],
    univariate = TRUE
  ) |>
    dplyr::mutate(outcome = "Cases")
  deaths_mods <- glm_bn_run(
    y = deaths,
    x = indicators,
    off_data = geo[, "population", drop = FALSE],
    univariate = TRUE
  ) |>
    dplyr::mutate(outcome = "Deaths")
  mods <- dplyr::bind_rows(cases_mods, deaths_mods)

  # Add names
  name <- list(
    vroom::vroom(here::here(input, "covid", "covid_list.csv")),
    vroom::vroom(here::here(input, "indicators", "indicators_list.csv"))
  ) |>
    dplyr::bind_rows()
  mods <- dplyr::left_join(mods, name[, c("file", "name")], by = c("y" = "file")) |>
    dplyr::rename(y_name = name) |>
    dplyr::left_join(name[, c("file", "name")], by = c("x" = "file")) |>
    dplyr::rename(x_name = name)

  # Export
  pipedat::masterwrite(mods, here::here(out, "statistics_glm_nb"))

  # Figures
  load_all()
  glm_bn_figs(mods, "Cases")
  glm_bn_figs(mods, "Deaths")
}

# ------------------------------------------------------------
make_formula <- function(y, x, off = NULL, univariate = TRUE) {
  ny <- colnames(y)
  nx <- colnames(x)

  if (univariate) {
    formulas <- lapply(ny, function(x) {
      if (is.null(off)) f <- glue::glue("{x} ~ {nx}")
      if (!is.null(off)) f <- glue::glue("{x} ~ {nx} + offset({off})")
      data.frame(
        y = x,
        x = nx,
        formulas = f
      )
    }) |>
      dplyr::bind_rows()
  }

  if (!univariate) {
    z <- paste(nx, collapse = " + ")
    formulas <- lapply(ny, function(x) {
      glue::glue("{x} ~ {z}") |>
        formula()
    })
  }

  formulas
}

# ------------------------------------------------------------
glm_bn <- function(formulas, dat) {
  # Statistical models
  stats <- lapply(formulas$formulas, function(x) {
    try(
      MASS::glm.nb(
        formula = formula(x),
        data = dat,
        link = log
      )
    )
  })

  # Summaries
  mods <- data.frame(
    y = formulas$y,
    x = formulas$x,
    formulas = formulas$formulas,
    coefficients = NA,
    p_value = NA,
    R2 = NA,
    AIC = NA
  )

  for (i in seq_len(nrow(mods))) {
    tmp <- summary(stats[[i]])
    if (inherits(tmp, "summary.negbin")) {
      mods$coefficients[i] <- coef(tmp)[2]
      mods$p_value[i] <- tmp$coefficients[2, "Pr(>|z|)"]
      mods$R2[i] <- (tmp$null.deviance - tmp$deviance) / tmp$null.deviance
      mods$AIC[i] <- tmp$aic
    }
  }

  # Return
  mods
}

# ------------------------------------------------------------
glm_bn_run <- function(y, x, off_data = NULL, univariate = TRUE) {
  # Formulas
  if (!is.null(off_data)) {
    stopifnot(ncol(off_data) == 1)
    off <- glue::glue("log({colnames(off_data)})")
  } else {
    off <- NULL
  }
  formulas <- make_formula(y, x, off, univariate)

  # Data
  dat <- cbind(y, x, off_data)

  # Models
  glm_bn(formulas, dat)
}


# ------------------------------------------------------------
glm_bn_figs <- function(mods, type) {
  mods$x_name <- stringr::str_replace(mods$x_name, "((?:\\S*\\s){9}.*?)\\s", "\\$1\n")
  mods$y_name <- stringr::str_replace(mods$y_name, "((?:\\S*\\s){9}.*?)\\s", "\\$1\n")
  mods$x_name <- stringr::str_replace(mods$x_name, "\\\\", "")
  mods$y_name <- stringr::str_replace(mods$y_name, "\\\\", "")

  # Prepare table for figures
  tmp <- mods |>
    dplyr::filter(outcome == type) |>
    dplyr::mutate(
      coefficients = round(coefficients, 4),
      p_value = ifelse(p_value > 0.001, round(p_value, 3), "<0.001"),
      R2 = round(R2, 4),
      sig = p_value <= 0.05
    ) |>
    add_colors() |>
    dplyr::group_by(y) |>
    dplyr::group_split()

  # Figure parameters
  nRow <- nrow(tmp[[1]])
  nCol <- length(tmp)
  nmRow <- tmp[[1]]$x_name
  nmCol <- unique(dplyr::bind_rows(tmp)$y_name)
  xG <- .3
  xG2 <- xG * 1.35
  yG <- .5
  yG2 <- yG * .9

  # Figure
  out <- here::here("figures")
  chk_create(out)
  png(
    here::here(out, glue::glue("glm_nb_{stringr::str_to_lower(type)}.png")),
    res = 300,
    width = (nCol * 80) + 500,
    height = (nRow * 10) + 500,
    units = "mm",
    pointsize = 10
  )
  par(
    mar = c(1, 1, 1, 2)
  )
  graphicsutils::plot0(x = c(-1, nCol + 1), y = c(0, nRow + 2))
  text(x = seq_len(nCol), y = nRow + 2, adj = .5, labels = nmCol, font = 2, cex = 1.25)
  text(x = .5, y = seq_len(nRow), adj = c(1, .5), labels = nmRow, font = 2, cex = 1.25)
  for (j in 1:length(tmp)) {
    text(
      x = c(j - xG, j, j + xG), y = rep(nRow + 1, 3),
      labels = c("Coefficients", "p value", "R2"), adj = .5, cex = 1.25
    )
    for (k in seq_len(nRow)) {
      rect(j - xG2, k - yG2, j + xG2, k + yG2, col = tmp[[j]]$cols[k], border = "#00000000")
      text(x = j - xG, y = k, adj = .5, labels = tmp[[j]]$coefficients[k], font = tmp[[j]]$sig[k], cex = 1.25)
      text(x = j, y = k, adj = .5, labels = tmp[[j]]$p_value[k], font = tmp[[j]]$sig[k], cex = 1.25)
      text(x = j + xG, y = k, adj = .5, labels = tmp[[j]]$R2[k], font = tmp[[j]]$sig[k], cex = 1.25)
    }
  }
  dev.off()
}



# ------------------------------------------------------------
# Colors function
add_colors <- function(dat) {
  # Color palettes
  red <- "#744242"
  blue <- "#036e95"
  pal1 <- colorRampPalette(c(graphicsutils::lighten(red, 80), graphicsutils::darken(red, 50)))
  pal2 <- colorRampPalette(c(graphicsutils::lighten(blue, 80), graphicsutils::darken(blue, 50)))

  # Remove NAs
  dat$coefficients[is.na(dat$coefficients)] <- 0

  # Colors
  dat <- dat |>
    dplyr::mutate(
      neg = coefficients < 0,
      tmp = abs(coefficients),
      tmp = round((tmp / max(tmp, na.rm = TRUE)) * 100, 0)
    )
  dat$cols <- NA
  dat$cols[dat$neg] <- paste0(pal1(101)[dat$tmp[dat$neg] + 1], "88")
  dat$cols[!dat$neg] <- paste0(pal2(101)[dat$tmp[!dat$neg] + 1], "88")
  dat$cols[!dat$sig] <- "#00000000"
  dat |>
    dplyr::mutate(sig = sig + 1) |>
    dplyr::select(-neg, -tmp)
}
