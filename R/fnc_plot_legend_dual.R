# =================================================================
#' @rdname plot_legend
#' @export
plot_legend_dual <- function(
    rMax,
    pal1 = NULL,
    pal2 = NULL,
    cexMain = .75,
    cexSub = .5,
    minUp = .055,
    mainTitle = NULL,
    subTitle = NULL,
    n = 5,
    colText = "#dedede") {
  # Legends
  # Palette
  if (class(pal1) == "character") {
    pal1 <- colorRampPalette(pal1)
    pal2 <- colorRampPalette(pal2)
  }

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .015 * xR # minimum left side to write
  yinit <- ymax - minUp * yR # minimum upper side to write
  ygap <- .04 * yR
  xgap <- .014 * xR
  ybarUp <- yinit - ygap / 2 - .0041 * yR
  ybarDn <- yinit - ygap - ygap / 2 + .0041 * yR

  # Divide in two
  wdPal <- .17 * xR
  xmid <- xinit + (wdPal) / 2
  xend <- xinit + wdPal

  # Palette 2 (negative)
  x <- seq(from = xinit, to = xmid, by = .0003 * xR)
  z <- data.frame(
    y1 = ybarUp,
    y2 = ybarDn,
    x1 = x[1:length(x) - 1],
    x2 = x[2:length(x)],
    col = pal2(length(x) - 1),
    stringsAsFactors = F
  )
  for (k in 1:nrow(z)) {
    polygon(
      x = c(z$x1[k], z$x2[k], z$x2[k], z$x1[k], z$x1[k]),
      y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
      col = z$col[k],
      border = z$col[k]
    )
  }

  # Palette 1 (positive)
  x <- seq(from = xmid, to = xend, by = .0003 * xR)
  z <- data.frame(
    y1 = ybarUp,
    y2 = ybarDn,
    x1 = x[1:length(x) - 1],
    x2 = x[2:length(x)],
    col = pal1(length(x) - 1),
    stringsAsFactors = F
  )
  for (k in 1:nrow(z)) {
    polygon(
      x = c(z$x1[k], z$x2[k], z$x2[k], z$x1[k], z$x1[k]),
      y = c(z$y1[k], z$y1[k], z$y2[k], z$y2[k], z$y1[k]),
      col = z$col[k],
      border = z$col[k]
    )
  }

  # Add axis
  x <- seq(from = xinit, to = xinit + wdPal, length.out = n)
  lines(x = c(xinit, xinit + wdPal), y = rep(z$y2[1], 2), col = colText)
  for (i in 1:n) lines(x = rep(x[i], 2), y = c(z$y2[1], z$y2[1] - .003 * yR), col = colText)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Labels
  lab <- round(c(-rMax, -rMax / 2, 0, rMax / 2, rMax))
  text(
    x = x,
    y = rep(z$y2[1] - .01 * yR, n),
    labels = lab,
    cex = cexSub * .75,
    adj = c(1, 1),
    srt = 45,
    col = colText
  )

  # Add titles
  yText <- ybarUp + .025 * yR

  # Add sub text
  if (!is.null(subTitle)) {
    text(
      x = xinit,
      y = yText,
      labels = latex2exp::TeX(subTitle, italic = TRUE),
      cex = cexSub,
      adj = c(0, 1),
      col = colText
    )
    yText <- yText + .035 * yR
  }

  # Add main title
  if (!is.null(mainTitle)) {
    text(
      x = xinit,
      y = yText,
      labels = mainTitle,
      cex = cexMain,
      font = 2,
      adj = c(0, 1),
      col = colText
    )
  }
}
