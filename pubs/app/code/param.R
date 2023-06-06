# Libraries
library(shiny)
library(leaflet)
library(leafem)

# Metadata 
meta <- read.csv("data/metadata.csv")

# Filter metadata 
indicators <- dplyr::filter(
  meta, 
  !stringr::str_detect(filenames, "covid_timeline_canada")
)

covid <- dplyr::filter(
  meta, 
  stringr::str_detect(filenames, "covid_timeline_canada")
)
