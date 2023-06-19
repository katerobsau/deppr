get_sim_schaake_criteria <- function(forecast, forecast_list){
# members = rows

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
                      byrow = TRUE, nrow = forecast_dim))^2

  sim_schaake_criteria = (1/forecast_dim*rowSums(mean_diff^2) +
                            1/forecast_dim*rowSums(sd_diff^2)) |>
    sqrt()

  template_ind = which.min(sim_schaake_criteria)

  template = forecast_list[[template_ind]]

  return(template)

}
