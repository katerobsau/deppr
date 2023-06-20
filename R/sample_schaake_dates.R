#' Samples dates for schaake shuffle
#'
#' For a given date, this function samples dates within a surrounding window across different years.
#' These dates can be used in the Schaake shuffle to generate a dependence template
#' that is based on climatology.
#'
#' @param num_draws number of dates to samples
#' @param dates vector of all possible dates where there are suitable observations. This vector will be cross-referenced with the climate window
#' @param date_val date of the observation for which a similar climatology is required
#' @param window integer that gives the radius of the date window, \code{date_val} +- \code{window} (unit is days)
#'
#' @return a vector of length \code{num_draws} that gives the sampled dates
#'
#' @details This function assumes the window of interest is in given in number of days.
#' The dates input is for handling dates with missing observations.
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
#' date_val = lubridate::as_date("2019-01-01")
#' dates = seq(lubridate::as_date("2018-01-01"), lubridate::as_date("2020-01-01"), by = "days")
#'
#' set.seed(1)
#' sampled_dates <- sample_schaake_dates(num_draws = 3, dates = dates, date_val = date_val,  window = 7)
#' sampled_dates
#'
#' set.seed(1)
#' new_dates = setdiff(dates, sampled_dates) %>% lubridate::as_date()
#' new_sampled_dates <- sample_schaake_dates(num_draws = 3, dates = new_dates, date_val = date_val, window = 7)
#' new_sampled_dates
#'
#' insufficient_dates <- sample_schaake_dates(num_draws = 100, dates = new_dates, date_val = date_val, window = 7)
#'
#' @export
sample_schaake_dates <- function(num_draws, dates, date_val, window = 7){

  window_dates = seq(date_val - window, date_val + window, by = "days")

  years = lubridate::year(dates) %>% unique()

  all_window_dates <- lapply(years, function(year, window_dates){
    lubridate::year(window_dates) <- year
    return(window_dates)
  }, window_dates = window_dates) %>%
    do.call("c", .) %>%
    intersect(dates) %>%
    lubridate::as_date()

  # check sufficient dates for sampling
  if(length(all_window_dates) < num_draws){
    warning(paste("Insufficent dates around", date_val, "for sampling, so returning NAs"))
    return(rep(NA, num_draws))
  }
  sampled_dates = sample(all_window_dates, num_draws)

  return(sampled_dates)

}
