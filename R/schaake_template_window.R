schaake_template_window <- function(n_members, historical_dates, date_val, window = 7){
  # from 'sample_schaake_dates'
  # n_members = the number of draws / ensemble members
  # historical_dates = the dates from which to draw
  # date_val = the date that we need a template for
  # window = the size of the window from which to draw


  window_dates = seq(date_val - window, date_val + window, by = "days")

  years = lubridate::year(historical_dates) %>% unique()
  all_window_dates <- lapply(years, function(year, window_dates){
    lubridate::year(window_dates) <- year
    return(window_dates)
  }, window_dates = window_dates) %>%
    do.call("c", .) %>%
    intersect(historical_dates) %>%
    lubridate::as_date()

  # check sufficient dates for sampling
  if(length(all_window_dates) < n_members){
    warning(paste("Insufficent dates around", date_val, "for sampling, so returning NAs"))
    return(rep(NA, n_members))
  }
  sampled_dates = sample(all_window_dates, n_members)

  return(sampled_dates)


}
