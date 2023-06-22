#' Shuffles a post-processed forecast to restore dependence using observations from past dates
#' that had similar forecasts
#'
#' @param forecast is a matrix where the columns correspond to members with the unrealistic dependence structure, and the rows are a space or time dimension
#' @param template is a matrix where the columns correspond to the template for dependence structure, and the rows are a space or time dimension
#'
#' @return a matrix where the post-processed forecast with dependence based on
#' the template
#'
#' @details
#' For a forecast that has been sampled from a post-processed forecast distribution
#' the members need to be reshuffled to restore a meaningful dependence structure
#' in space, time or both.
#'
#' This function uses a template to reshuffle the sample ensemble members. The
#' template can be one of climatology, as in the Schaake shuffle, or can be from
#' the raw ensemble forecast, as in empirical copula coupling.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#'
#' Sim Schaake paper
#'
#' @examples
#'
#' num_stations = 2
#' num_lead_times = 4
#' forecast_dim = num_stations*num_lead_times
#' num_members = 3

#' forecast = rnorm(forecast_dim*num_members) |>
#'                matrix(forecast_dim, num_members)
#'
#' forecast_list = list(forecast - 1, forecast + 0.5, forecast + 1, forecast + 2)
#' obs_list = list(rnorm(forecast_dim) - 1, rnorm(forecast_dim) + 0.5,
#'                      rnorm(forecast_dim) + 1, rnorm(forecast_dim) + 2)
#'
#' template = get_sim_schaake_template(forecast, forecast_list, obs_list)
#'
get_sim_schaake_template <- function(forecast, forecast_list, obs_list){
# members = cols
# forecast dimension = number of rows

warning("Add checks for dimension consistency")
warning("Add checks for sufficient dates")

warning("Forecasts with multiple variables should be standardised")

  if(!is.list(forecast_list)){
    error("forecast_list must be a list object")
  }


  get_marginal_sd <- function(forecast){
    sd_vec = apply(forecast, 1, sd)
    return(sd_vec)
  }

  num_members = ncol(forecast)
  forecast_dim = nrow(forecast)

  forecast_means = rowMeans(forecast)
  past_forecast_means = lapply(forecast_list, rowMeans)
  mean_diff = matrix(0, length(forecast_list), forecast_dim)
  for(i in 1:length(forecast_list)){
    mean_diff[i,] = (forecast_means - past_forecast_means[[i]])^2}
  mean_contrib = rowSums(mean_diff)/forecast_dim

  forecast_sd = get_marginal_sd(forecast)
  past_forecast_sd = lapply(forecast_list, get_marginal_sd)
  sd_diff = mean_diff*0
  for(i in 1:length(forecast_list))
    sd_diff[i,] = (forecast_sd - past_forecast_sd[[i]])^2
  sd_contrib = rowSums(sd_diff)/forecast_dim

  similarity_criterion = sqrt(mean_contrib + sd_contrib)

  retain_dates = which(rank(similarity_criterion) <= num_members)

  template = obs_list[retain_dates] |>
    unlist() |>
    matrix(byrow = FALSE, nrow = forecast_dim)

  return(template)

}
