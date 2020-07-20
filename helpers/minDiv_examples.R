# -----------------------------------------------------------------------------

# First example

L = 5 # Lead times
M = 50
N = 500

# ensemble forecast
# M members by L lead times
X = matrix(rnorm(M*L), M, L)

# climate replicates
# N dates by L lead times
H_n = matrix(rnorm(N*L), N, L)
H_n_1 = H_n - 1

min_div <- get_min_div(X = X, H_n = H_n, M = M)
min_div_1 <- get_min_div(X = X, H_n = H_n_1, M = M)

# -----------------------------------------------------------------------------

# Second example

set.seed(1)

current_date = lubridate::as_date(Sys.time())
lead_times = paste(c(6, 12, 18, 24), ":0:0", sep = "") %>%
  lubridate::hms() %>%
  lubridate::as.duration()

ens_meta <- expand.grid(
  FORECAST_DATE = current_date,
  LOCATION = c("S1", "S2", "P1"),
  LEAD_TIME = lead_times,
  INIT_TIME = 0) %>%
  as.data.frame() %>%
  dplyr::mutate(ELEMENT = substr(LOCATION,1,1))

pp_sim = matrix(rnorm(M*nrow(ens_meta)), nrow(ens_meta), M) %>%
  as.data.frame() %>%
  setNames(paste("SIM", 1:50, sep = ""))

ensemble_data <- bind_cols(ens_meta, pp_sim)

N_dates = seq(0, -N) + current_date

obs_df <- expand.grid(
  FORECAST_DATE = N_dates,
  LOCATION = c("S1", "S2", "P1"),
  LEAD_TIME = lead_times,
  INIT_TIME = 0) #%>%
  # dplyr::mutate(OBS_DATE = FORECAST_DATE + LEAD_TIME)

obs_vals = matrix(rnorm(nrow(obs_df)), nrow(obs_df), 1) %>%
  as.data.frame() %>%
  setNames("OBS")

obs_data <- bind_cols(obs_df, obs_vals) %>%
  dplyr::mutate(FORECAST_DATE = paste("Hist", FORECAST_DATE, sep = "_")) %>%
  tidyr::pivot_wider(names_from = FORECAST_DATE, values_from = OBS)

# For each day, need to pick a set of historical days
# > date is the current_date
# > corresponding ensemble data for that date is ensemble_data

all_data <- ensemble_data %>%
  dplyr::left_join(obs_data, by = c("LOCATION", "LEAD_TIME"))

X_mat = all_data %>%
  dplyr::select(starts_with("SIM")) %>%
  as.matrix()

H_mat = all_data %>%
  dplyr::select(starts_with("Hist")) %>%
  as.matrix()

min_div1 <- get_minimum_divergence(X_mat[1,], H_mat[1,])
min_div2 <- get_minimum_divergence(X_mat[2,], H_mat[2,])
total_div <- get_total_divergence(X_mat[1:2,], H_mat[1:2,])
min_div1 + min_div2
total_div

total_div <- get_total_divergence(X_mat, H_mat)
total_div1 <- get_total_divergence(X_mat, H_mat + 1)
total_div2 <- get_total_divergence(X_mat, H_mat*0.9)
total_div
total_div1
total_div2

# -----------------------------------------------------------------------------

backward_selection_mindiv <- function(backward_num, X_mat, H_mat){

  total_div_minus_j = rep(NA, ncol(H_mat))
  for(j in 1:ncol(H_mat))
    total_div_minus_j[j] <- get_total_divergence(X_mat = X_mat, H_mat = H_mat[,-j])

  rank_vec = rank(total_div_minus_j)
  eliminate_dates = which(rank_vec > backward_num)
  H_mat_reduced = H_mat[ , -eliminate_dates]

  return(H_mat_reduced)

}

backward_seq <- c(420, 340, 270, 210, 160, 120, 90, 70, 60)
stepwise_backward_selection_mindiv <- funtion(backward_seq, X_mat, H_mat){

  H_mat_reduced = H_mat
  names(H_mat_reduced) = paste("C", 1:ncol(H_mat), sep = "")
  for(i in 1:length(backward_seq)){

    print(dim(H_mat_reduced))

    H_mat_reduced <- backward_selection_mindiv(
      backward_num = backward_seq[i],
      X_mat = X_mat,
      H_mat = H_mat_reduced)

  }

  return(H_mat_reduced)
}



# -----------------------------------------------------------------------------

