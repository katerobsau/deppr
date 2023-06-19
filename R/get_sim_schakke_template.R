#' forecast = rnorm(6) |> matrix(3, 2)
#' @examples
#' past_forecast_1 = rnorm(6) |> matrix(3, 2)
#' past_forecast_2 = rnorm(6, mean = 0.5) |> matrix(3, 2)
#' past_forecast_3 = rnorm(6, mean = 1) |> matrix(3, 2)
#' past_forecast_list = list(forecast_1, forecast_2, forecast_3)
#' template = get_sim_schaake_template(forecast, past_forecast_list)
#' all(template == forecast_1)
#'
get_sim_schaake_template <- function(forecast, forecast_list){
# members = cols
# forecast dimension = number of rows

warning("Forecasts with multiple variables should be standardised")

  if(!is.list(forecast_list)){
    error("forecast_list must be a list object")
  }

  get_marginal_sd <- function(forecast){
    sd_vec = apply(forecast, 1, sd)
    return(sd_vec)
  }

  forecast_dim = nrow(forecast)

  forecast_means = rowMeans(forecast)
  matrix_means = lapply(forecast_list, rowMeans) |>
    unlist() |>
    matrix(byrow = TRUE, nrow = forecast_dim)

  forecast_sd = get_marginal_sd(forecast)
  matrix_sd = lapply(forecast_list, get_marginal_sd) |>
    unlist() |>
    matrix(byrow = TRUE, nrow = forecast_dim)

  mean_diff = (matrix_means -
                    matrix(rep(forecast_means, times = nrow(matrix_means)),
                           byrow = TRUE, nrow = forecast_dim))

  sd_diff = (matrix_sd -
               matrix(rep(forecast_sd, times = nrow(matrix_sd)),
                      byrow = TRUE, nrow = forecast_dim))

  similarity_criterion = (1/forecast_dim*rowSums(mean_diff^2) +
                            1/forecast_dim*rowSums(sd_diff^2)) |>
    sqrt()

  template = forecast_list[[which.min(similarity_criterion)]]

  return(template)

}
