#' Simple script to copy figures for use in publications
#'
#' @export

fig_ingrid <- function() {
  # ------------------------------------------------------------------------
  # Graph principal
  out <- here::here("pubs","figures")
  pipedat::chk_create(out)
  file.copy(
    from = here::here("figures","pipedat","ingrid"),
    to = out,
    recursive = TRUE
  )
}
