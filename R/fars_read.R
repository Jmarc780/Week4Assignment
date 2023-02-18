#' Title: fars_read
#' Description: This is a simple function that first checks to see if there is an actual file name that is entered

#' as an input. If the file name provided does not exist, then the function stops, and displays a message
#' that indicates the file does not exist. If the file does exist, than the file is read into R via
#' the readr package. The package is than converted to a datafram using the dplyr package.
#' Usage: fars_read(filename)
#' Arguments: filename
#' @param filename
#' Value: If the file exists, this produces a dataframe. If the file doesn't exist it, an error message comes through.
#' @return fars_read
#' @examples
#' \dontrun{
#'    fars_read("accident_2013.csv.bz2")
#' }
#'
#'
#' # A tibble: 30,202 x 50
#' STATE ST_CASE VE_TOTAL VE_FORMS PVH_INVL  PEDS PERNOTMVIT PERMVIT PERSONS COUNTY  CITY   DAY MONTH  YEAR
#' <dbl>   <dbl>    <dbl>    <dbl>    <dbl> <dbl>      <dbl>   <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#'  1     1   10001        1        1        0     0          0       8       8    115     0     6     1  2013
#'  2     1   10002        2        2        0     0          0       2       2     55  1670     3     1  2013
#'  3     1   10003        1        1        0     0          0       1       1     89  1730     6     1  2013
#'  4     1   10004        1        1        0     0          0       3       3     73   350     6     1  2013
#'  5     1   10005        2        2        0     0          0       3       3    125  3050     6     1  2013

#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
