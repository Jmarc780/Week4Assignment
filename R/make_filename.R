#' Title: make_filename
#' Description: This is a function that takes an input in the form of a four digit value, a year, such as 2017.
#' The function then converts the input to an integer and holds it in a variable called year
#' It then takes the year variable and applies it to the string accident_%d.csv.bz2 as accident_2017.csv.bz2
#' when the function is called for the year 2017.
#' Usage: make_filename(year)
#' Arguments: year. A string or integer providing the correct year to use in the file name
#' @param year
#'  Value: This function produces the filename a particular year. A warning message comes if the input cannot be converted to an integer.
#' @return make_filename
#' @examples
#' \dontrun{
#'    make_filename(2017)
#' }
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
