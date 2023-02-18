#' Title: fars_summarize_read
#' Description: The data for the input is read using fars_read_years function
#' The dataframes are then unioned using a bind_rows function from dplyr.
#' Produced df is grouped by the year and MONTH.
#' The total number of records is then summarized with summarize function
#' The results are represented with months as rows and years
#' as columns using the spread fnction from tidyr.
#' An error message pops up if file does not exist
#' Usage: fars_summarize_years(years)
#' Arguments: years.
#' @param years
#' Value: A data frame with crashes for each year and month. An error message occurs if file or input does not exist.
#' @return fars_summarize_years
#' @examples
#' \dontrun{
#'    fars_summarize_years(2013)
#' }
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

