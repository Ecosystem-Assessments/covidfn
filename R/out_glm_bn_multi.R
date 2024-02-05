#' General linear models with negative binomial distribution - Multivariate models
#'
#' @export

out_glm_nb_multi <- function() {
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

  # Step-wise & within-group indicator removal based on correlation and AIC
  indicators <- select_indicators(
    y = cbind(cases, deaths),
    x = indicators,
    off_data = geo[, "population", drop = FALSE],
    threshold = 0.8
  )

  # Stepwise model selection for each outcome during each period
  mods <- list()
  for (i in seq_len(length(indicators))) {
    mods[[i]] <- stepwise_glm_nb(
      y = y[, names(indicators)[i]],
      x = indicators[[i]],
      off_data = geo[, "population", drop = FALSE]
    )
  }
}


# ------------------------------------------------------------
stepwise_glm_nb <- function(y, x, off_data = NULL) {
  # Full model formula
  stopifnot(ncol(y) == 1)
  f <- glue::glue("{colnames(y)} ~ .")
  if (!is.null(off_data)) {
    stopifnot(ncol(off_data) == 1)
    f <- glue::glue("{f} + log({colnames(off_data)})")
  } else {
    off <- NULL
  }

  # Dataset
  dat <- cbind(y, x, off_data)

  # Full model
  null_model <- MASS::glm.nb(
    formula = formula(f),
    data = dat,
    link = log
  )

  # Stepwise model selection
  mod <- MASS::stepAIC(
    null_model,
    direction = "both",
    trace = FALSE
  )

  # Summaries
  tmp <- summary(mod)
  mods <- list()
  mods$coef <- coef(tmp)
  mods$summary <- data.frame(
    y = colnames(y),
    R2 = (tmp$null.deviance - tmp$deviance) / tmp$null.deviance,
    AIC = tmp$aic
  )

  # Return
  mods
}

# ------------------------------------------------------------
# Function to test which variables can be removed within each indicator groups
select_indicators <- function(y, x, off_data = NULL, threshold = 0.8) {
  # Indicators list
  ind_list <- vroom::vroom(here::here(input, "indicators", "indicators_list.csv")) |>
    dplyr::group_by(group) |>
    dplyr::group_split()

  # Groups of indicators
  ind_group <- list()
  for (i in seq_len(length(ind_list))) ind_group[[i]] <- x[, ind_list[[i]]$file, drop = FALSE]

  # Test groups and identify indicators to remove
  rem <- lapply(ind_group, function(x) {
    test_group(y, x, off_data, threshold)
  })
  rem <- dplyr::bind_rows(rem)

  # Create an indicator table for each y variable
  indicators <- rem |>
    dplyr::group_by(outcome) |>
    dplyr::group_split() |>
    lapply(function(z) {
      x[, !colnames(x) %in% z$remove]
    })
  names(indicators) <- colnames(y)

  # Return
  indicators
}

# ------------------------------------------------------------
test_group <- function(y, ind_group, off_data = NULL, threshold = 0.8) {
  if (ncol(ind_group) > 1) {
    # Assess correlation between all indicators
    cor_indicators <- combn(colnames(ind_group), 2) |>
      t() |>
      data.frame()
    colnames(cor_indicators) <- c("V1", "V2")

    # Identify correlations above threshold
    cor_indicators <- cor_indicators |>
      dplyr::mutate(
        cor = apply(cor_indicators, 1, function(x) cor(ind_group[, x[1]], ind_group[, x[2]])),
        cor = abs(cor) > threshold
      ) |>
      dplyr::filter(cor) |>
      dplyr::select(-cor)

    # Identify which variable to remove from group for each y variable
    if (nrow(cor_indicators) > 0) {
      rem <- list()
      for (i in seq_len(ncol(y))) {
        rem[[i]] <- test_mods(
          y = y[, i, drop = FALSE],
          x = ind_group,
          off_data = off_data,
          cor_indicators = cor_indicators
        )
      }
      rem <- dplyr::bind_rows(rem)
    } else {
      rem <- NULL
    }
  } else {
    rem <- NULL
  }

  # Return
  rem
}


# ------------------------------------------------------------
test_mods <- function(y, x, off_data = NULL, cor_indicators) {
  # Test models and identify which variables to remove
  rem <- character(0)
  while (nrow(cor_indicators) > 0) {
    try(
      mods <- glm_bn_run(
        y = y,
        x = x[, as.character(cor_indicators[1, ])],
        off_data = off_data,
        univariate = TRUE
      )
    )
    # Identify and remove unsuccessful models
    if (any(is.na(mods$AIC))) {
      rem <- c(rem, mods$x[is.na(mods$AIC)])
    } else {
      mods <- mods |>
        dplyr::group_by(y) |>
        dplyr::filter(AIC == max(AIC, na.rm = TRUE))
      rem <- c(rem, mods$x)
    }
    cor_indicators <- dplyr::filter(
      cor_indicators,
      !V1 %in% rem &
        !V2 %in% rem
    )
  }

  # Return dataframe with column names to remove
  data.frame(
    remove = rem,
    outcome = colnames(y)
  )
}
