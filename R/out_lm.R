#' Explore individual variables with covid data
#'
#' @export

out_lm <- function() {
  fun <- function(xy, name) {
    # Columns to use
    ## WARNING: these will change if the data changes
    message("WARNING: Column ids will change if integrated datasets from pipedat are updated. Modify code accordingly")
    xR <- c(3,4,5,6,7,8,9,10,11,12,13,97,98,99,101,102,103,104) # Vuln
    yn <-  36:41 # Covid 15:54
    
    # Data prep
    xyz <- sf::st_coordinates(xy)
    xy <- sf::st_drop_geometry(xy) |>
          cbind(xyz)
    
    # Names of social vulnerabilities
    nm <- colnames(xy) |> stringr::str_replace(".tif","")
    
    # Date range for covid
    nmyf <- dplyr::first(nm[yn])
    nmyl <- dplyr::last(nm[yn])
    
    nmy <- ifelse(
      length(yn) > 1,
      glue::glue(
        "Covid cases / population - {substr(nmyf, 60, 66)} to {substr(nmyl, 60, 66)}"
      ),
      glue::glue("Covid cases / population - {substr(nmyf, 60, 66)}")
    )
    
    # Covid values
    if (length(yn) > 1) {
      y <- rowSums(xy[, yn], na.rm = TRUE)
    } else {
      y <- xy[,yn]
    }
    
    # Output
    output <- here::here("figures","lm",name)
    pipedat::chk_create(output)
    
    # Linear models & scatterplots
    r2 <- list()
    for(i in seq_len(length(xR))) {  
      xn <- xR[i]
      y <- y
      x <- xy[, xn]
      ev <- lm(y ~ x)
      r2[[i]] <- summary(ev)$adj.r.squared

      # Individual plots
      png(
        here::here(output, glue::glue("lm_{i}.png")), 
        res = param$figures$resolution, 
        width = param$figures$width, 
        height = param$figures$height, 
        units = "mm", 
        pointsize = 10
      )  
      
      plot(
        x = x, 
        y = y, 
        xlab = nm[xn], 
        ylab = nmy,
        pch = 21,
        col = "#1262b1",
        cex = .75,
        main = glue::glue("R2 = {round(r2[[i]], 4)}")
      )
      
      dev.off()
    }

    # R2 graph
    png(
      here::here(output, glue::glue("r2.png")), 
      res = param$figures$resolution, 
      width = param$figures$width, 
      height = param$figures$height, 
      units = "mm", 
      pointsize = 10
    )

    par(mar = c(5,30,0,0))
    graphicsutils::plot0(ylim = c(0,length(xR)), xlim = c(0,0.5))
    axis(1)
    mtext(side = 1, "R2", line = 3)
    for(i in seq_len(length(xR))) {
      points(y = i, x = r2[[i]], pch = 21, col = "#1262b1", cex = .75)
    }
    mtext(nm[xR], side = 2, at = seq_len(length(xR)), las = 2, cex = .75)

    dev.off()  
  }
  
  fun(
    sf::st_read("output/fn_extract/canadian_geographical_names-92230392.gpkg"), 
    name = "canadian_geographical_names"
  )
  fun(
    sf::st_read("output/fn_extract/first_nations_location-ce594316.gpkg"), 
    name = "first_nations_location"
  )
}



# xy <- sf::st_read("output/fn_extract/first_nations_location-ce594316.gpkg") 
# xy <- sf::st_read("output/fn_extract/canadian_geographical_names-92230392.gpkg") 
# xyz <- sf::st_coordinates(xy)
# xy <- sf::st_drop_geometry(xy) |>
#       cbind(xyz)
# 
# y <- xy[,15]
# # y <- xy[,16]
# x <- as.matrix(xy[, c(3, 6, 7, 9, 10, 13, 18, 19, 21, 22, 23, 24)])
# x <- as.matrix(xy[, c(9, 24)])
# x <- log(as.matrix(xy[, c(9)])+1)
# ev <- lm(y ~ x)
# summary(ev)
# 
# dat <- xy[,c(9,24)] |>
#        na.omit()
# 
# 
# 
# 
# colnames(xy)
# 
# plot(xy[,15] ~ xy[,3])
# plot(xy[,15] ~ xy[,4])
# plot(xy[,15] ~ xy[,5])
# plot(xy[,15] ~ xy[,6])
# plot(xy[,15] ~ xy[,7])
# plot(xy[,15] ~ xy[,8])
# plot(xy[,15] ~ xy[,9])
# plot(xy[,15] ~ xy[,10])
# plot(xy[,15] ~ xy[,11])
# plot(xy[,15] ~ xy[,12])
# plot(xy[,15] ~ xy[,13])
# plot(xy[,15] ~ xy[,14])
# plot(xy[,15] ~ xy[,15])
# plot(xy[,15] ~ xy[,16])
# plot(xy[,15] ~ xy[,17])
# plot(xy[,15] ~ xy[,18])
# plot(xy[,15] ~ xy[,19])
# plot(xy[,15] ~ xy[,20])
# plot(xy[,15] ~ xy[,21])
# plot(xy[,15] ~ xy[,22])
# plot(xy[,15] ~ xy[,23])
# plot(xy[,15] ~ xy[,24])
# 
# 
# 
# dat <- xy[,c(15,9,23,24)] |>
#        na.omit() 
# dat$X2 <- dat$X^2
# dat <-as.matrix(dat) 
# 
# ev <- lm(dat[,1] ~ dat[, 2:5])
# summary(ev)
# 
# ev <- lm(dat[,1] ~ dat[,2] + dat[,3]^2, dat[,4])
# 
# 