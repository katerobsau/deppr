#' Get dates for Schaake shuffle
#'
#' This function identifies sequential observations that do not contain missing
#' data and could be used in the Schaake shuffle.
#'
#' @param datetime_vec A vector of dates corresponding to all the observations.
#' These should be in the date form of `"\%Y-\%m-\%d \%H:\%M:\%S"`.
#'
#' @param window The length of the forecast window. This input must be specified
#' as a lubridate object. For example, `hours(48)` or `days(2)`.
#'
#' @param init_times A vector containing strings with the forecast
#' initialisation times. For example, `c("00")` or `c("00", "12")`.
#' These strings are converted to numeric variables internally by the function.
#'
#' @param by The timestep of your weather forecast,
#' eg `"hours"`, `"6 hours"`, `"days"`. This argument can by any increment that
#' can be used to step between two date objects in `seq()`.
#'
#' @return A vector of dates of the form `"\%Y-\%m-\%d \%H:\%M:\%S"`
#' that correspond to historical dates that can be used in the Schaake shuffle.
#'
#' @details
#'
#' When there is missing data some sequences of dates will not be suitable
#' for use in the Schaake shuffle. Individual users may choose to interpolate the
#' missing values. For simplicity, this function identifies suitable dates that
#' do not contain missing values.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#'
#' Clark, Martyn, et al. "The Schaake shuffle: A method for reconstructing
#' space–time variability in forecasted precipitation and temperature fields."
#' Journal of Hydrometeorology 5.1 (2004): 243-262.
#'
#' @examples
#' datetime_vec = seq(as.POSIXct("2023-01-01 00:00:00"), as.POSIXct("2023-01-10 12:00:00"), by = "hours")
#' datetime_vec[days(datetime_vec) == 5] = NA
#' get_schaake_shuffle_dates(datetime_vec, days(1), init_times = "00")
#' get_schaake_shuffle_dates(datetime_vec, days(2), init_times = "00")
#' get_schaake_shuffle_dates(datetime_vec, days(1), init_times = c("00", "12"))
#' get_schaake_shuffle_dates(datetime_vec, days(1), init_times = c("00", "12"), by = "6 hours")
#'
get_schaake_shuffle_dates <- function(datetime_vec, window, init_times, by = "hours", ...){

  missing_datetimes <- get_missing_datetimes(datetime_vec, ...)
  warning("Developer reminder: Add in a condition if there are no missing dates")

  all_bad_datetimes <- get_all_bad_datetimes(missing_datetimes,
                                             window = window,
                                             init_times = init_times,
                                             ... )

  good_schaake_datetimes <- get_good_schaake_datetimes(datetime_vec,
                                                       init_times = init_times,
                                                       all_bad_datetimes,
                                                       ... )

  return(good_schaake_datetimes)

}
