#' Animation of covid cases and deaths in Canada
#'
#' @export

fig_covid <- function() {
  # Load covid data 
  dat <- pipedat::importdat("a56e753b", "ingrid")
  cases <- dat[stringr::str_detect(names(dat), "cases")]
  # deaths <- dat[stringr::str_detect(names(dat), "deaths")]
  
  # Ranges 
  rg <- function(dat, rnd) {
    lapply(dat, function(x) max(x[[1]], na.rm = TRUE)) |> 
    unlist() |> 
    max() |>
    round(rnd)
  }
  mxc <- rg(cases, 0)
  # mxd <- rg(deaths, 2)
  
  # Plot function
  plid <- function(dat, rang) {
    # Period 
    nm <- names(dat)
    y <- substr(nm, nchar(nm)-10, nchar(nm)-7) 
    m <- substr(nm, nchar(nm)-5, nchar(nm)-4) 

    image(
      dat, 
      col = viridis::viridis(100),
      main = glue::glue("Number of covid cases / population - {y}-{m}")
    )
  }

  # Make gif
  animation::saveGIF({
    for(i in seq_len(length(cases))) {
      par(family = "serif")
      plid(cases[[i]])
    }
  }, movie.name = "essai.gif")  
}