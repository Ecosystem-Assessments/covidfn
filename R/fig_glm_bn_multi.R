#' General linear models with negative binomial distribution - Multivariate models - Figures
#'
#' @export

fig_glm_bn_multi <- function() {
  out <- here::here("figures")
  out2 <- here::here(out, "glm_bn_multi")
  chk_create(out)
  chk_create(out2)
  r2 <- pipedat::masterload(here::here("output", "glm_bn_multivariate_r2.csv"))
  coefs <- pipedat::masterload(here::here("output", "glm_bn_multivariate_coefficients.csv"))
  cases <- pipedat::masterload(here::here("data", "data_format", "covid", "cases.tif"))
  deaths <- pipedat::masterload(here::here("data", "data_format", "covid", "deaths.tif"))

  # Figure params
  res <- 300
  width <- 300
  height <- 300

  # Spatial figures
  plot_spat <- function(dat) {
    # Basemap
    aoi <- sf::st_read(
      "data/pipedat/covid_timeline_canada-a56e753b/format/covid_timeline_canada-a56e753b-hr_wgs84.gpkg",
      quiet = TRUE
    )
    canada <- sf::st_read("data/basemap/canada_full.gpkg", quiet = TRUE)
    pr <- sf::st_read(
      "data/pipedat/covid_timeline_canada-a56e753b/format/covid_timeline_canada-a56e753b-pt_wgs84.gpkg",
      quiet = TRUE
    )

    # Data
    p <- quantile(
      dat[[1]],
      probs = seq(0, 1, by = .1),
      na.rm = TRUE
    )
    dat <- split(dat)

    # Names
    nm <- names(dat)

    for (i in seq_len(length(dat))) {
      png(
        here::here(out2, glue::glue("spatial_{nm[i]}.png")),
        res = res,
        width = width,
        height = height,
        units = "mm",
        pointsize = 10
      )
      image(
        dat[i],
        # col = viridis::magma(sum(p > 0)),
        col = viridis::magma(length(p) - 1),
        breaks = p,
        axes = FALSE,
        main = NULL
      )
      plot(sf::st_geometry(aoi), lwd = .5, border = "#bbbbbb", col = "#00000000", add = TRUE)
      plot(sf::st_geometry(pr), lwd = 1.25, border = "#bbbbbb", col = "#00000000", add = TRUE)
      plot(sf::st_geometry(canada), lwd = 1.25, border = "#bbbbbb", col = "#00000000", add = TRUE)
      dev.off()
    }
  }
  plot_spat(cases)
  plot_spat(deaths)

  # Coefficient tables
  plot_mod <- function(mods) {
    # Indicators list
    ind_list <- vroom::vroom(here::here("data", "data_format", "indicators", "indicators_list.csv"))

    # Prepare table for figures
    tmp <- mods |>
      dplyr::mutate(
        coefficients = round(Estimate, 4),
        p_value = ifelse(`Pr(>|z|)` > 0.001, round(`Pr(>|z|)`, 3), "<0.001"),
        sig = p_value <= 0.05
      ) |>
      add_colors() |>
      dplyr::left_join(ind_list, by = c("x_name" = "file")) |>
      dplyr::mutate(
        name = ifelse(is.na(name), x_name, name),
        # group = ifelse(is.na(group), "Intercept", group),
        name = stringr::str_replace(name, "((?:\\S*\\s){7}.*?)\\s", "\\$1\n"),
        name = stringr::str_replace(name, "\\\\", "")
      ) |>
      dplyr::arrange(y, desc(group), name) |>
      dplyr::group_by(y) |>
      dplyr::group_split()

    # Figure parameters
    nRow <- lapply(tmp, nrow) |>
      unlist() |>
      max()
    nCol <- 4
    xG <- .5
    yG <- .5
    lG <- .5

    # Figure
    for (i in seq_len(length(tmp))) {
      nm <- unique(tmp[[i]]$y)
      groups <- tmp[[i]] |>
        dplyr::select(group) |>
        dplyr::mutate(id = seq_len(dplyr::n())) |>
        dplyr::group_by(group) |>
        dplyr::summarise(
          line = min(id) - lG,
          pos = mean(id)
        ) |>
        dplyr::ungroup()

      png(
        here::here(out2, glue::glue("models_{nm}.png")),
        res = 300,
        width = width,
        height = height,
        units = "mm",
        pointsize = 10
      )
      par(
        mar = c(0, 1, 0, 1)
      )
      xlim <- c(.25, nCol + 1)
      graphicsutils::plot0(x = xlim, y = c(-nRow - 2, 2))
      x <- c(1, 3.5, 4, 4.5)

      # Table titles & model summary
      text(x = mean(xlim), y = 2.5, labels = "Generalized Linear Models", adj = .5, cex = 1.25, font = 2)
      text(
        x = mean(xlim), y = 1.9, adj = .5, cex = 1,
        labels = "Negative binomial distribution - offset = log(population size)"
      )
      text(x = mean(xlim) / 2, y = 1, labels = glue::glue("Number of parameters: {nrow(tmp[[i]])}"), adj = .5, font = 2)
      text(x = mean(xlim), y = 1, labels = latex2exp::TeX(glue::glue("McFadden's pseudo-$R^2$: {round(r2$R2[i],2)}"), bold = TRUE), adj = .5)
      text(x = 3 * (mean(xlim) / 2), y = 1, labels = glue::glue("AIC: {round(r2$AIC[i],2)}"), adj = .5, font = 2)

      # Column names
      text(x = x[1], y = 0, labels = "Group", adj = .5, cex = 1, font = 2)
      text(x = x[2], y = 0, labels = "Indicators", adj = c(1, .5), cex = 1, font = 2)
      text(x = x[3], y = 0, labels = "Coefficients", adj = .5, cex = 1, font = 2)
      text(x = x[4], y = 0, labels = "p value", adj = .5, cex = 1, font = 2)

      # Groups
      text(x = rep(x[1], nrow(groups)), y = -groups$pos, adj = .5, labels = groups$group, font = 1, cex = 1)
      for (k in -seq_len(nrow(groups))) {
        lines(x = c(x[1] - xG, x[4] + xG / 2), y = rep(-groups$line[-k], 2), col = "#00000066")
      }
      lines(x = c(x[1] - xG, x[4] + xG / 2), y = rep(0 + lG, 2), col = "#000000", lwd = 1.5)
      lines(x = c(x[1] - xG, x[4] + xG / 2), y = rep(-nrow(tmp[[i]]) - lG, 2), col = "#000000", lwd = 1.5)

      # Rest of text
      for (k in -seq_len(nrow(tmp[[i]]))) {
        # rect(1, k - yG, nCol, k + yG, col = tmp[[i]]$cols[-k], border = "#00000000")
        text(x = x[2], y = k, adj = c(1, .5), labels = tmp[[i]]$name[-k], font = 1, cex = 1)
        text(x = x[3], y = k, adj = .5, labels = tmp[[i]]$coefficients[-k], font = tmp[[i]]$sig[-k], cex = 1)
        text(x = x[4], y = k, adj = .5, labels = tmp[[i]]$p_value[-k], font = tmp[[i]]$sig[-k], cex = 1)
      }
      dev.off()
    }
  }
  plot_mod(coefs)

  # ----------------------------------------------------------------------------------------------------------------
  # Functions
  nm_title <- function(img, chr) {
    wd <- magick::image_info(img)$width * .5
    # ht <- magick::image_info(img)$height * .00001
    magick::image_annotate(
      img,
      chr,
      # location = glue::glue("+{wd}+0"),
      gravity = "north",
      size = 75,
      font = "Palatino",
      style = "Italic",
      weight = 2000,
      # decoration = "underline",
      color = NULL,
    )
  }

  nm_sub <- function(img, chr) {
    wd <- magick::image_info(img)$width * .5
    magick::image_annotate(
      img,
      chr,
      location = "+0+100",
      gravity = "north",
      size = 75,
      font = "Palatino",
      style = "Italic",
      weight = 200,
      color = "#494949",
    )
  }

  # Combine everything with image magick
  img <- list()
  for (i in seq_len(nrow(r2))) {
    # Images
    i1 <- magick::image_read(here::here(out2, glue::glue("spatial_{r2$y[i]}.png")))
    i2 <- magick::image_read(here::here(out2, glue::glue("models_{r2$y[i]}.png")))

    # Border to spatial image
    i1 <- magick::image_resize(i1, "50%x50%")
    wd <- (magick::image_info(i2)$width - magick::image_info(i1)$width) / 2
    i1 <- magick::image_border(i1, glue::glue("{wd}x150"), color = "#ffffff")

    # Combine
    img[[i]] <- magick::image_append(c(i1, i2), stack = TRUE)

    # Add title & border
    name <- stringr::str_split(r2$y[i], "_") |>
      unlist()
    img[[i]] <- nm_title(img[[i]], glue::glue("Number of {name[1]}")) |>
      nm_sub(glue::glue("{name[2]}-{name[3]}-{name[4]} to {name[5]}-{name[6]}-{name[7]}")) |>
      magick::image_border("1x1", color = "#000000")
  }

  # Cases
  cases <- list(
    magick::image_append(c(img[[1]], img[[2]]), stack = FALSE),
    magick::image_append(c(img[[3]], img[[4]]), stack = FALSE)
  )
  cases <- magick::image_append(c(cases[[1]], cases[[2]]), stack = TRUE)
  magick::image_write(cases, here::here(out, "glm_nb_multi_cases.png"))
  rm(cases)
  gc()

  # deaths
  deaths <- list(
    magick::image_append(c(img[[5]], img[[6]]), stack = FALSE),
    magick::image_append(c(img[[7]], img[[8]]), stack = FALSE)
  )
  deaths <- magick::image_append(c(deaths[[1]], deaths[[2]]), stack = TRUE)
  magick::image_write(deaths, here::here(out, "glm_nb_multi_deaths.png"))
  rm(deaths)
  gc()
}
