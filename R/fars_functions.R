fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' This function reads data in `CSV` format into R. 
#' It first checks if the file already exists in the working directory. If not, you get an error message. 
#' Otherwise, if the CSV file exists, it is read into R, and then converts the data to a tidy `tibble` format.
#'
#' @param filename A string with the full name of a file in CSV format
#' 
#' @return This function returns a data frame in tibble format.
#' 
#' @examples
#' fars_read("accident_2013.csv")
#'
#' @import From readr read_csv

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' This function makes a file name. 
#' First, it converts the input year to an integer.  It must be input without quotes. 
#' Then the year is added into a string that represents the main pattern of the file name. 
#' 
#' @param A number representing the desired year.
#' 
#' @return This function returns a character vector using the main string and the year inputted.
#' 
#' @examples
#' make_filename(2013)

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

#' This function reads `MONTH` and `year`, from the input files, and then stores them as `tibbles` in a `list.` 
#' It utilizes the function `make_filename.`
#'
#' @param years A vector or sequence of numbers, which represents the years to be analyzed. If this function returns an error, a year number is invalid.
#' 
#' @return A list with the data frames in tibble format for the selected years.
#' The list has the length of the input vector and each data frame contains only the selected columns.
#' 
#' @examples
#' fars_read_years(2013:2015)
#' 
#' @import From dplyr mutate, dplyr select

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' This function summarizes the data by year and month for the `tibbles` imported. 
#' It utilizes the function `fars_read_years` from before.
#'  
#' @param years A vector  or sequence of numbers, which represents the years to be analyzed.
#' 
#' @return This function returns a data frame in tibble format with months as rows and years as columns, in which the data are the counts of accidents.
#' 
#' @examples
#' fars_summarize_years(2013:2015)
#' 
#' @import From dplyr bind_rows, dplyr group_by, dplyr summarize, tidyr spread 

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

#' This function plots a map with the locations of the data for the selected state and year. 
#' It calls the functions `make_filename` and `fars_read` from within.
#'  
#' @param state.num A number representing the FARS code of the desired state. If the function returns an error, the state number is invalid.
#'
#' @param year A number representing the desired year.
#' 
#' @return This function returns a map with the locations of accidents for the used year and state.
#' 
#' @examples
#' fars_summarize_years(2013:2015)
#' 
#' @importFrom dplyr filter, maps map, graphics points
