#' Figure of study area
#'
#' @export

fig_aoi <- function() {
  # ------------------------------------------------------------------------
  # Graph principal
  out <- here::here("figures", "aoi")
  chk_create(out)
  global_parameters()
  png(
    here::here(out, "aoi.png"),
    res = param$figures$resolution,
    width = param$figures$width,
    height = param$figures$height,
    units = "mm",
    pointsize = param$figures$pointsize
  )

  # ------------------
  aoi <- sf::st_read(
    "data/pipedat/covid_timeline_canada-a56e753b/format/covid_timeline_canada-a56e753b-hr_wgs84.gpkg",
    quiet = TRUE
  )
  canada <- sf::st_read("data/basemap/canada_full.gpkg", quiet = TRUE)
  pr <- sf::st_read(
    "data/pipedat/covid_timeline_canada-a56e753b/format/covid_timeline_canada-a56e753b-pt_wgs84.gpkg",
    quiet = TRUE
  )

  # ------------------
  # Labels
  labs <- c("Canadian provinces/territories and health regions")

  # ------------------
  bbox <- sf::st_bbox(aoi)

  # ------------------
  # Provinces colors
  x <- as.factor(aoi$region)
  nx <- levels(x) |> length()
  aoi$cols <- viridis::mako(nx, alpha = .5)[x]

  # ------------------
  yGap <- 1
  par(family = "serif", mar = c(.5, .5, .5, .5))
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin - yGap, bbox$ymax - yGap))
  plot(
    sf::st_geometry(aoi),
    lwd = .25,
    border = "#000000",
    col = aoi$cols,
    add = TRUE
  )
  plot(sf::st_geometry(pr), lwd = .75, border = "#000000", col = "#00000000", add = TRUE)
  plot(sf::st_geometry(canada), lwd = .75, border = "#000000", col = "#00000000", add = TRUE)

  # # Legend
  # xR <-
  # # Provinces & territories
  # lines(x = c())
  # # Health regions

  # ---------------------------
  dev.off()
}
