get_missing_datetimes <- function(obs_datetime, ...){

  all_datetimes = seq(min(obs_datetime), max(obs_datetime), by = "hours")
  missing_datetimes = setdiff(as.character(all_datetimes),
                              as.character(obs_datetime)) %>%
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S", ...)

  return(missing_datetimes)

}

get_nearby_invalid_times <-function(datetime,
                                    window,
                                    init_times = "00",
                                    return_type = "date"){

  # eg. window = days(2)

  if(!(return_type %in% c("char", "date")))
    error("Invalid return_type specificied")

  bad_window = seq(datetime - window, datetime, by = "hours")
  bad_init_times = bad_window[which(hour(bad_window) %in% as.numeric(init_times))]

  if(return_type == "char")
    return(as.character(bad_init_times))

  if(return_type == "date")
    return(bad_init_times)

}

get_all_bad_datetimes <- function(missing_datetimes,
                                  window,
                                  init_times = "00", ...){

  all_bad_datetimes = sapply(missing_datetimes,
                             get_nearby_invalid_times,
                             init_times = init_times,
                             window = window,
                             return_type = "char") %>%
    do.call("c", .)

  if(all(as.numeric(init_times) == 0)){
    all_bad_datetimes = as.POSIXct(all_bad_datetimes, format = "%Y-%m-%d", ... )
  }else{
    all_bad_datetimes = as.POSIXct(all_bad_datetimes, format = "%Y-%m-%d %H:%M:%S", ... )
  }

  return(all_bad_datetimes)

}

get_good_schaake_datetimes <- function(obs_datetime,
                                       init_times,
                                       all_bad_datetimes, ...){

  #assumes initiated on the hour

  all_datetimes = seq(min(obs_datetime), max(obs_datetime), by = "hours")

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


# missing_datetimes <- get_missing_datetimes(obs_datetime,
#                                            tz = "UTC")
#
# # get_nearby_invalid_times(missing_datetimes[1], window = days(2), init_times = c("00"))
# # get_nearby_invalid_times(missing_datetimes[1], window = days(2), init_times = c("00"), return_type = "char")
#
# all_bad_datetimes <- get_all_bad_datetimes(missing_datetimes,
#                                            window = days(2),
#                                            init_times = c("00" , "12"),
#                                            tz = "UTC")
#
# good_schaake_datetimes <- get_good_schaake_datetimes(obs_datetime,
#                                                      init_times = c("00", "12"),
#                                                      all_bad_datetimes,
#                                                      tz = "UTC")
