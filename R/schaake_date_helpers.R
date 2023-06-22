get_missing_datetimes <- function(datetime_vec, by = "hours", ...){

  warning("Reminder: function needs further development")

  # need to handle if there is missingness
  # handle different entry forms, eg. has NAs
  # need to code for different step times

  all_datetimes = seq(min(datetime_vec, na.rm = TRUE),
                      max(datetime_vec, na.rm = TRUE),
                      by = by)

  missing_datetimes = setdiff(as.character(all_datetimes),
                              as.character(datetime_vec)) %>%
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S", ...)

  return(missing_datetimes)

}

get_nearby_invalid_times <-function(datetime,
                                    window,
                                    init_times = "00",
                                    return_type = "date",
                                    by = "hours"){

  # eg. window = days(2)

  if(!(return_type %in% c("char", "date")))
    error("Invalid return_type specificied")

  bad_window = seq(datetime - window, datetime, by = by)
  bad_init_times = bad_window[which(hour(bad_window) %in% as.numeric(init_times))]

  if(return_type == "char")
    return(as.character(bad_init_times))

  if(return_type == "date")
    return(bad_init_times)

}

get_all_bad_datetimes <- function(missing_datetimes,
                                  window,
                                  init_times = "00",
                                  by = "hours",
                                  ...){

  all_bad_datetimes = sapply(missing_datetimes,
                             get_nearby_invalid_times,
                             init_times = init_times,
                             window = window,
                             return_type = "char",
                             by = by) %>%
    as.list() %>% #not great way of dealing with this robustly
    do.call("c", .) %>%
    unique()

  if(all(as.numeric(init_times) == 0)){
    all_bad_datetimes = as.POSIXct(all_bad_datetimes, format = "%Y-%m-%d", ... )
  }else{
    all_bad_datetimes = as.POSIXct(all_bad_datetimes, format = "%Y-%m-%d %H:%M:%S", ... )
  }

  return(all_bad_datetimes)

}

get_good_schaake_datetimes <- function(datetime_vec,
                                       init_times,
                                       all_bad_datetimes,
                                       by = "hours",
                                       ...){

  #assumes initiated on the hour

  all_datetimes = seq(min(datetime_vec, na.rm = TRUE),
                      max(datetime_vec, na.rm = TRUE), by = by)

  init_datetimes = all_datetimes[which(hour(all_datetimes) %in% as.numeric(init_times))]

  good_schaake_datetimes = setdiff(as.character(init_datetimes),
                                   as.character(all_bad_datetimes))

  if(all(as.numeric(init_times) == 0)){

    good_schaake_datetimes = as.POSIXct(good_schaake_datetimes,
                                        format = "%Y-%m-%d", ... ) |> sort()

  }else{

    good_schaake_datetimes = as.POSIXct(good_schaake_datetimes,
                                        format = "%Y-%m-%d %H:%M:%S", ... ) |> sort()

  }

  return(good_schaake_datetimes)

}
