## Get years or months from input period
getY <- function(x) format(x, "%Y")
getM <- function(x) format(x, "%m")

## Identify columns to summarize
idCols <- function(dat, y, per) {
  year <- getY(per)
  month <- getM(per)
  nm <- colnames(dat)
  
  # Locate relevant columns 
  cvd <- stringr::str_detect(nm, "covid")
  yvar <- stringr::str_detect(nm, tolower(y))
  beg <- stringr::str_detect(nm, paste0(year[1],"_",month[1]))
  end <- stringr::str_detect(nm, paste0(year[2],"_",month[2]))
  rg <- which(cvd & yvar & beg):which(cvd & yvar & end)
  
  # Summarize column and return
  rowSums(dat[,rg], na.rm = TRUE)
}

# Get relevant covid data
geoCovid <- function(dat, y, per) {
  year <- getY(per)
  month <- getM(per)
  
  # Locate relevant columns 
  yvar <- stringr::str_detect(dat, tolower(y))
  beg <- stringr::str_detect(dat, paste0(year[1],"_",month[1]))
  end <- stringr::str_detect(dat, paste0(year[2],"_",month[2]))
  rg <- which(yvar & beg):which(yvar & end)
  
  dat <- lapply(dat[rg], function(x) {
    stars::read_stars(paste0("data/tifs/",x))
  }) 
  do.call("c",dat) |>
    stars::st_redimension()|>
    stars::st_apply(c("x","y"), sum, na.rm = TRUE) #|>
    # dplyr::filter(sum > 0)
       
}
