#' Title: fars_read_years
#' Description: For each input, this function will call the make_filename function,
#' which produces the files name for the info in that year,
#' then loads the generated filename into R using the fars_read function,
#' and then filters the data to just month and years as the cols.
#' If a file with a year doesn't exist, an error message is produced.
#' Usage: fars_read_years(years)
#' Arguments: years. the years to find files to bring into R. Can be single or multiple values.
#' @param years
#' Value: This function returns the dataframes for files that exist for the years inputted. message of error will occur if it does not exist.
#' @return fars_read_years
#' @examples
#' \dontrun{
#' fars_read_years(years=c(2013,2015))
#' fars_read_years(c(2014,2015)) }
#' @importFrom dplyr mutate select
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

