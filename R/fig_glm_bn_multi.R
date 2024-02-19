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
    p <- quantile(
      dat[[1]],
      probs = seq(0, 1, by = .1),
      na.rm = TRUE
    )
    dat <- split(dat)

    # Names
    nm <- names(dat)
    name <- stringr::str_split(nm, "_") |>
      lapply(function(x) {
        glue::glue("Number of {x[1]}\n{x[2]}-{x[3]}-{x[4]} to {x[5]}-{x[6]}-{x[7]}")
      })

    for (i in seq_len(length(dat))) {
      png(
        here::here(out2, glue::glue("spatial_{nm[i]}.png")),
        res = res,
        width = width,
        height = height,
        units = "mm",
        pointsize = 10
      )
      plot(
        dat[i],
        col = viridis::viridis(sum(p > 0)),
        breaks = p,
        axes = TRUE,
        main = name[[i]]
      )
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
        mar = c(1, 1, 1, 2)
      )
      graphicsutils::plot0(x = c(0, nCol + 1), y = c(-nRow - 2, 0))
      x <- c(1, 3.5, 4, 4.5)

      # Column names
      text(x = x[1], y = 0, labels = "Group", adj = .5, cex = 1.25, font = 2)
      text(x = x[2], y = 0, labels = "Indicators", adj = c(1, .5), cex = 1.25, font = 2)
      text(x = x[3], y = 0, labels = "Coefficients", adj = .5, cex = 1.25, font = 2)
      text(x = x[4], y = 0, labels = "p value", adj = .5, cex = 1.25, font = 2)

      # Groups
      text(x = rep(x[1], nrow(groups)), y = -groups$pos, adj = .5, labels = groups$group, font = 1, cex = 1)
      for (k in -seq_len(nrow(groups))) {
        lines(x = c(x[1] - xG, x[4] + xG / 2), y = rep(-groups$line[-k], 2), col = "#00000066")
      }

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

  # # ----------------------------------------------------------------------------------------------------------------
  # # Combine everything with image magick
  # img <- list()
  # for (i in seq_len(nrow(r2))) {
  #   i1 <- magick::image_read(here::here(out2, glue::glue("spatial_{r2$y[i]}.png")))
  #   i2 <- magick::image_read(here::here(out2, glue::glue("models_{r2$y[i]}.png")))
  #   img[[i]] <- magick::image_append(c(i1, i2), stack = TRUE)
  # }

  # # Cases
  # cases <- list(
  #   magick::image_append(c(img[[1]], img[[2]]), stack = FALSE),
  #   magick::image_append(c(img[[3]], img[[4]]), stack = FALSE)
  # )
  # cases <- magick::image_append(c(cases[[1]], cases[[2]]), stack = TRUE)
  # magick::image_write(cases, here::here(out, "glm_nb_multi_cases.png"))
  # rm(cases)
  # gc()
}
