#' Gets schaake shuffle dates
#'
#' This function looks at the past observations and identifies dates that do not
#' contain missing data and could be used in the schaake shuffle.
#'
#' @param obs_datetime - a vector of dates corresponding to the observations.
#' These should be in the date form of `"\%Y-\%m-\%d \%H:\%M:\%S"`.
#'
#' @param window - the length of the forecast window. This object must be specified
#' as a lubridate object. For example, `hours(48)` or `days(2)`.
#'
#' @param init_times - a vector containing strings with the forecast
#' initialisation times. For example, `c("00")` or `c("00", "12")`.
#' These strings are converted to numeric variables internally by the function.
#'
#' @return a vector of dates of the form `"\%Y-\%m-\%d \%H:\%M:\%S"` that
#' are suitable for use in the Schaake shuffle.
#'
#' @details
#'
#' When there is missing data in the observations these dates are not suitable to be used
#' in the Schaake shuffle template. Individual users
#' may choose to interpolate the missing values. For simplicity however, this
#' function identifies any dates and initialisation times that would contain
#' missing observations, and returns a vector of only suitable dates for
#' consideration.
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
#'
#'
get_schaake_shuffle_dates <- function(obs_datetime, window, init_times, ...){

  missing_datetimes <- get_missing_datetimes(obs_datetime,
                                             ... )

  # add in a condition if there are no missing dates

  all_bad_datetimes <- get_all_bad_datetimes(missing_datetimes,
                                             window = window,
                                             init_times = init_times,
                                             ... )

  good_schaake_datetimes <- get_good_schaake_datetimes(obs_datetime,
                                                       init_times = init_times,
                                                       all_bad_datetimes,
                                                       ... )

  return(good_schaake_datetimes)

}
