#' Pipeline to execute full project
#'
#' @export

pipeline <- function() {
  # Update global parameters
  global_parameters()
  
  # Get area of interest & basemaps
  get_aoi()
  get_basemap()

  # Integrate data 
  library(pipedat)
  pipedat::pipeflow()
  
  # Extract values under points
  fn_extract()
  
  # Explore relationships between covid and social vulnerabilities
  out_lm()
  out_cluster()
  
  # Figures 
  fig_aoi()
  fig_ingrid()
  
  # Report and publications 
  render_frontpate()
  render_report()
  # render_webinar()
}