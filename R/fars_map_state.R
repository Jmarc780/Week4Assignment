#' Title: fars_map_state
#' Description: year is converted to a filename with make_filename function.
#' Usage: fars_summarize_years(years)
#' Arguments: state.num &  year. Both integer values.
#' @param state.num
#' @param year
#' Values: DF with monthly  accidents for row and years for col
#' @return fars_map_state
#' @examples
#' \dontrun{
#'    fars_map_state(1, 2013)
#' }
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points

#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
