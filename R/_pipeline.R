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

  # A bit of a change of pace here, I do not want to keep developing pipedat just yet in light of our current work
  canadian_census()
  proximity_measures_database()
  cchs()
  community_wellbeing_index()

  # Prepare data for analysis
  format_data()

  # Analyses & figures
  figures()
  out_glm_nb()
  out_glm_nb_multi()


  out_extract_hr()
  out_base_stats()

  # # Extract values under points
  # fn_extract()

  # # Explore relationships between covid and social vulnerabilities
  # out_lm()
  # out_cluster()

  # # Figures
  # fig_aoi()
  # fig_ingrid()

  # Report and publications
  render_frontpate()
  render_report()
  # render_webinar()
}
