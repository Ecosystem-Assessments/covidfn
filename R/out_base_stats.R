#' Correlations between COVID-19 cases and social vulnerabilities
#'
#' @export

out_base_stats <- function() {
  # ===========================================================================
  # Setup
  out <- here::here("output", "covid_data")
  covid <- read.csv("output/covid_data/covid.csv")

  # ===========================================================================
  # Raw data exploration
  histograms(covid, out, "histograms")
  scatterplots(covid, out)
  autocorrelation(covid, out)
  correlations(covid, out)

  # ===========================================================================
  # Statistical exploration: outome / population ~ indicators
  # Using Gaussian linear regressions
  dat <- data_transformation(covid)
  histograms(dat, out, "histograms_transformed")
  dat <- scale_indicators(dat)
  correlations_heatmap("cases", TRUE, out, "cases_prop")
  correlations_heatmap("deaths", TRUE, out, "deaths_prop")
  linear_regressions(dat, out)
  # multiple_regressions(dat, out)

  # ===========================================================================
  # Statistical exploration: outome ~ indicators
  # Using Generalized linear models with poisson distribution
  # Population as an offset variable
  dat <- scale_indicators(covid)
  correlations_heatmap("cases", FALSE, out, "cases")
  correlations_heatmap("deaths", FALSE, out, "deaths")
  glm_regressions(dat, out)
}

# ------------------------------------------------------------
histograms <- function(covid, out, nm) {
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::group_by(name) |>
    dplyr::mutate(value = (value / max(value)) * 100)

  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(binwidth = 10) +
    ggplot2::facet_wrap(~name, scales = "free")

  ggplot2::ggsave(
    filename = glue::glue("{nm}.png"),
    plot = p,
    device = "png",
    path = out,
    width = 50,
    height = 50,
    units = "cm"
  )
}

# ------------------------------------------------------------
scatterplots <- function(covid, out) {
  # Make this ugly, use two nested loops
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical)

  # Cases and deaths per population size
  cases <- stringr::str_detect(colnames(dat), "cases")
  death <- stringr::str_detect(colnames(dat), "deaths")

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
}


# ------------------------------------------------------------
autocorrelation <- function(covid, out) {
  # Make this ugly, use two nested loops
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical)

  # Cases and deaths per population size
  cases <- stringr::str_detect(colnames(dat), "cases")
  death <- stringr::str_detect(colnames(dat), "deaths")

  # Indicators
  ind <- stringr::str_detect(colnames(dat), "cases") +
    stringr::str_detect(colnames(dat), "deaths")
  ind <- !as.logical(ind)

  # Autocorrelation
  ind <- dat[, ind]
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
}



# ------------------------------------------------------------
data_transformation <- function(covid) {
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

  return(covid)
}

# ------------------------------------------------------------
scale_indicators <- function(covid) {
  covid$longitude <- (covid$longitude - min(covid$longitude)) / diff(range(covid$longitude))
  trs <- c("region", "health_region", "name_canonical", "cases", "death")
  uid <- lapply(trs, function(x) stringr::str_detect(colnames(covid), x)) |>
    data.frame() |>
    rowSums() |>
    as.logical()
  covid[, !uid] <- apply(covid[, !uid], 2, function(x) x / max(x))
  covid
}

# ------------------------------------------------------------
correlations <- function(covid, out) {
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical) |>
    as.matrix() |>
    cor(method = "spearman") |>
    as.data.frame()
  write.csv(dat, file = here::here(out, "correlations.csv"))
}

# ------------------------------------------------------------
correlations_heatmap <- function(type, prop = FALSE, out, nm) {
  dat <- read.csv(here::here(out, "correlations.csv"))
  rownames(dat) <- dat$X
  dat <- dplyr::select(dat, -X)

  # Identify y variables
  uid <- stringr::str_detect(colnames(dat), type)
  pop <- stringr::str_detect(colnames(dat), "_pop")
  if (prop) uid <- (uid + pop) == 2
  if (!prop) uid <- (uid + !pop) == 2

  # Identify indicators
  ind <- stringr::str_detect(colnames(dat), "cases") +
    stringr::str_detect(colnames(dat), "deaths")
  ind <- !as.logical(ind)

  # Correlations matrix
  dat <- dat[ind, uid] |>
    round(2)
  write.csv(dat, file = here::here(out, glue::glue("correlations_{nm}.csv")))

  # Color palettes
  red <- "#744242"
  blue <- "#036e95"
  pal1 <- colorRampPalette(c(graphicsutils::lighten(red, 80), graphicsutils::darken(red, 50)))
  pal2 <- colorRampPalette(c(graphicsutils::lighten(blue, 80), graphicsutils::darken(blue, 50)))

  # Colors
  nCol <- ncol(dat)
  nRow <- nrow(dat)
  nmCol <- colnames(dat)
  nmRow <- rownames(dat)
  cols <- tmp <- dat * 100
  sup <- which(cols >= 0, arr.ind = TRUE)
  inf <- which(cols < 0, arr.ind = TRUE)
  for (i in 1:nrow(sup)) cols[sup[i, 1], sup[i, 2]] <- pal2(101)[tmp[sup[i, 1], sup[i, 2]] + 1]
  for (i in 1:nrow(inf)) cols[inf[i, 1], inf[i, 2]] <- pal1(101)[abs(tmp[inf[i, 1], inf[i, 2]]) + 1]

  # Figure
  png(
    here::here(out, glue::glue("correlations_{nm}.png")),
    res = 300,
    width = (nCol * 10) + 100,
    height = (nRow * 10) + 100,
    units = "mm",
    pointsize = 10
  )
  par(
    mar = c(1, 1, 1, 1)
  )
  graphicsutils::plot0(x = c(-6, nCol), y = c(-6, nRow))
  text(x = seq_len(nCol), y = 0, adj = c(1, .5), labels = nmCol, srt = 90)
  text(x = 0, y = seq_len(nRow), adj = c(1, .5), labels = nmRow)
  for (j in seq_len(nCol)) {
    for (i in seq_len(nRow)) {
      points(x = j, y = i, pch = 22, bg = cols[i, j], col = "#00000000", cex = 7)
      text(x = j, y = i, labels = dat[i, j], col = "#ffffff")
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

  # Colors
  dat <- dat |>
    dplyr::mutate(
      neg = coef < 0,
      tmp = abs(coef),
      tmp = round((tmp / max(tmp)) * 100, 0)
    )
  dat$cols <- NA
  dat$cols[dat$neg] <- paste0(pal1(101)[dat$tmp[dat$neg] + 1], "88")
  dat$cols[!dat$neg] <- paste0(pal2(101)[dat$tmp[!dat$neg] + 1], "88")
  dat$cols[!dat$sig] <- "#00000000"
  dat |>
    dplyr::mutate(sig = sig + 1) |>
    dplyr::select(-neg, -tmp)
}

# ------------------------------------------------------------
linear_regressions <- function(covid, out) {
  # Doing linear regressions of the type:
  # outcomes / population size ~ social indicators
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

  # Data.frame to store results
  nM <- length(rows) * length(cols)
  mods <- data.frame(
    x = character(nM),
    y = character(nM),
    coef = numeric(nM),
    p = numeric(nM),
    r2 = numeric(nM)
  )

  # Run loop
  out2 <- here::here(out, "conditions")
  pipedat::chk_create(out2)
  k <- 1
  for (j in 1:length(rows)) {
    for (i in 1:length(cols)) {
      # Model
      ev <- lm(dat[, cols[i]] ~ dat[, rows[j]])
      sev <- summary(ev)
      mods$x[k] <- nm[rows[j]]
      mods$y[k] <- nm[cols[i]]
      mods$coef[k] <- sev$coefficients[2, "Estimate"]
      mods$p[k] <- sev$coefficients[2, "Pr(>|t|)"]
      mods$r2[k] <- sev$adj.r.squared

      # Conditions
      png(
        here::here(out2, glue::glue("{nm[rows[j]]}-{nm[cols[i]]}.png")),
        res = 300,
        width = 150,
        height = 150,
        units = "mm",
        pointsize = 10
      )
      par(mfrow = c(2, 2))
      plot(ev)
      dev.off()

      # Iterator
      k <- k + 1
    }
  }

  # Full model tables
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
  write.csv(tmp, here::here(out, "lm_gaussian.csv"), row.names = FALSE)

  # Prepare table for figures
  tmp <- mods |>
    dplyr::mutate(
      coef = round(coef, 4),
      p = round(p, 4),
      r2 = round(r2, 4),
      sig = p <= 0.1
    ) |>
    dplyr::mutate(type = stringr::str_detect(y, "cases")) |>
    dplyr::group_by(type) |>
    dplyr::group_split() |>
    lapply(function(x) {
      add_colors(x) |>
        dplyr::select(-type) |>
        dplyr::group_by(y) |>
        dplyr::group_split()
    })

  # Figures
  type <- c("deaths", "cases")
  nRow <- nrow(tmp[[1]][[1]])
  nCol <- length(tmp[[1]])
  nmRow <- tmp[[1]][[1]]$x
  xG <- .3
  xG2 <- xG * 1.35
  yG <- .5
  yG2 <- yG * .9

  for (i in seq_len(length(type))) {
    nmCol <- unique(dplyr::bind_rows(tmp[[i]])$y)
    png(
      here::here(out, glue::glue("lm_gaussian_{type[i]}.png")),
      res = 300,
      width = (nCol * 80) + 100,
      height = (nRow * 10) + 100,
      units = "mm",
      pointsize = 10
    )
    par(
      mar = c(1, 1, 1, 2)
    )
    graphicsutils::plot0(x = c(-1, nCol + 1), y = c(0, nRow + 2))
    text(x = seq_len(nCol), y = nRow + 2, adj = .5, labels = nmCol)
    text(x = 0, y = seq_len(nRow), adj = c(1, .5), labels = nmRow)
    for (j in 1:length(tmp[[i]])) {
      text(x = c(j - xG, j, j + xG), y = rep(nRow + 1, 3), labels = c("coef", "p", "R2"), adj = .5)
      for (k in seq_len(nRow)) {
        rect(j - xG2, k - yG2, j + xG2, k + yG2, col = tmp[[i]][[j]]$cols[k], border = "#00000000")
        text(x = j - xG, y = k, adj = .5, labels = tmp[[i]][[j]]$coef[k], font = tmp[[i]][[j]]$sig[k])
        text(x = j, y = k, adj = .5, labels = tmp[[i]][[j]]$p[k], font = tmp[[i]][[j]]$sig[k])
        text(x = j + xG, y = k, adj = .5, labels = tmp[[i]][[j]]$r2[k], font = tmp[[i]][[j]]$sig[k])
      }
    }
    dev.off()
  }

  # Multiple parameters models
  fml_cases <- make_formula(colnames(dat), "cases", TRUE)
  fml_death <- make_formula(colnames(dat), "deaths", TRUE)
  # for(i in seq_len(length(fml_cases)) {
  # Model
  ev <- lm(fml_cases[[1]], data = dat)
  sev <- summary(ev)
  mods$x[k] <- nm[rows[j]]
  mods$y[k] <- nm[cols[i]]
  mods$coef[k] <- sev$coefficients[2, "Estimate"]
  mods$p[k] <- sev$coefficients[2, "Pr(>|t|)"]
  mods$r2[k] <- sev$adj.r.squared

  # }
}


# ------------------------------------------------------------
glm_regressions <- function(covid, out) {
  # Make this ugly, use two nested loops
  dat <- covid |>
    dplyr::select(-region, -health_region, -name_canonical)

  # Cases and deaths per population size
  cases <- stringr::str_detect(colnames(dat), "cases")
  death <- stringr::str_detect(colnames(dat), "deaths")
  pop <- stringr::str_detect(colnames(dat), "_pop")
  cases <- (cases + !pop) == 2
  death <- (death + !pop) == 2

  # Indicators
  ind <- stringr::str_detect(colnames(dat), "cases") +
    stringr::str_detect(colnames(dat), "deaths")
  ind <- !as.logical(ind)
  ind[which(colnames(dat) == "population")] <- FALSE

  # Rows & columns
  cols <- c(which(cases), which(death))
  rows <- which(ind)
  nm <- colnames(dat)
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
      ev <- glm(dat[, cols[i]] ~ dat[, rows[j]], family = poisson, offset = log(dat$population))
      sev <- summary(ev)
      mods$x[k] <- nm[rows[j]]
      mods$y[k] <- nm[cols[i]]
      mods$coef[k] <- coef(ev)[2]
      mods$p[k] <- sev$coefficients[2, "Pr(>|z|)"]
      mods$r2[k] <- (ev$null.deviance - ev$deviance) / ev$null.deviance
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
  write.csv(tmp, here::here(out, "glm_poisson.csv"), row.names = FALSE)

  # Prepare table for figures
  tmp <- mods |>
    dplyr::mutate(
      coef = round(coef, 4),
      p = round(p, 4),
      r2 = round(r2, 4),
      sig = p <= 0.1
    ) |>
    dplyr::mutate(type = stringr::str_detect(y, "cases")) |>
    dplyr::group_by(type) |>
    dplyr::group_split() |>
    lapply(function(x) {
      add_colors(x) |>
        dplyr::select(-type) |>
        dplyr::group_by(y) |>
        dplyr::group_split()
    })

  # Figures
  type <- c("deaths", "cases")
  nRow <- nrow(tmp[[1]][[1]])
  nCol <- length(tmp[[1]])
  nmRow <- tmp[[1]][[1]]$x
  xG <- .3
  xG2 <- xG * 1.35
  yG <- .5
  yG2 <- yG * .9

  for (i in seq_len(length(type))) {
    nmCol <- unique(dplyr::bind_rows(tmp[[i]])$y)
    png(
      here::here(out, glue::glue("glm_{type[i]}.png")),
      res = 300,
      width = (nCol * 80) + 100,
      height = (nRow * 10) + 100,
      units = "mm",
      pointsize = 10
    )
    par(
      mar = c(1, 1, 1, 2)
    )
    graphicsutils::plot0(x = c(-1, nCol + 1), y = c(0, nRow + 2))
    text(x = seq_len(nCol), y = nRow + 2, adj = .5, labels = nmCol)
    text(x = 0, y = seq_len(nRow), adj = c(1, .5), labels = nmRow)
    for (j in 1:length(tmp[[i]])) {
      text(x = c(j - xG, j, j + xG), y = rep(nRow + 1, 3), labels = c("coef", "p", "R2"), adj = .5)
      for (k in seq_len(nRow)) {
        rect(j - xG2, k - yG2, j + xG2, k + yG2, col = tmp[[i]][[j]]$cols[k], border = "#00000000")
        text(x = j - xG, y = k, adj = .5, labels = tmp[[i]][[j]]$coef[k], font = tmp[[i]][[j]]$sig[k])
        text(x = j, y = k, adj = .5, labels = tmp[[i]][[j]]$p[k], font = tmp[[i]][[j]]$sig[k])
        text(x = j + xG, y = k, adj = .5, labels = tmp[[i]][[j]]$r2[k], font = tmp[[i]][[j]]$sig[k])
      }
    }
    dev.off()
  }
}

# ------------------------------------------------------------
make_formula <- function(y, x) {
  z <- paste(x, collapse = " + ")
  formula(glue::glue("{y} ~ {z}"))
}

# ------------------------------------------------------------
models <- function(type, prop) {
  # List of models to test
}


make_formula <- function(nm, outcome, prop) {
  # Wrapper to build formulas
  mk_fml <- function(y, x) {
    z <- lapply(x, function(x) paste(x, collapse = " + "))
    lapply(y, function(y) formula(glue::glue("{y} ~ {z}")))
  }

  # Dependent variables
  cases <- stringr::str_detect(nm, "cases")
  death <- stringr::str_detect(nm, "deaths")
  pop <- stringr::str_detect(nm, "_pop")
  cases_lm <- (cases + pop) == 2
  death_lm <- (death + pop) == 2
  cases_glm <- (cases + !pop) == 2
  death_glm <- (death + !pop) == 2


  # List of dependent variables
  y <- list(
    cases_lm = nm[cases_lm],
    death_lm = nm[death_lm],
    cases_glm = nm[cases_glm],
    death_glm = nm[death_glm]
  )

  # List of models to test
  x <- list(
    c(
      "gini_index_adj_household_total_income",
      "low_income_cutoffs_aftertax_percent",
      "percent_no_certificate_diploma_degree",
      "housing_suitability",
      "distance_critical_healthcare",
      "longitude",
      "latitude",
      "unemployment_rate",
      "percent_indigenous_identity"
    )
  )

  # Build models
  if (outcome == "cases" & prop) {
    return(mk_fml(y = y$cases_lm, x))
  }
  if (outcome == "cases" & !prop) {
    return(mk_fml(y = y$cases_glm, x))
  }
  if (outcome == "deaths" & prop) {
    return(mk_fml(y = y$death_lm, x))
  }
  if (outcome == "deaths" & !prop) {
    return(mk_fml(y = y$death_glm, x))
  }
}





# # Some manual exploration
# y <- "cases_2021.11.01_2022.03.11"
# x <- c(
#   "gini_index_adj_household_total_income",
#   "low_income_cutoffs_aftertax_percent",
#   "percent_no_certificate_diploma_degree",
#   "housing_suitability",
#   "distance_critical_healthcare",
#   "longitude",
#   "latitude",
#   "percent_indigenous_identity", "population"
# )
# z <- paste(x, collapse = " + ")
# fml <- formula(glue::glue("{y} ~ {z}"))
# ev <- glm(fml, data = dat, family = poisson) # , offset = log(dat$population))
# # summary(ev)
# (ev$null.deviance - ev$deviance) / ev$null.deviance
# DescTools::PseudoR2(ev, which = "all")

# ev <- glm(dat[, cols[i]] ~ dat[, rows[j]], family = poisson, offset = log(dat$population))
# sev <- summary(ev)
# mods$x[k] <- nm[rows[j]]
# mods$y[k] <- nm[cols[i]]
# mods$coef[k] <- coef(ev)[2]
# mods$p[k] <- sev$coefficients[2, "Pr(>|z|)"]
# mods$r2[k] <- (ev$null.deviance - ev$deviance) / ev$null.deviance
