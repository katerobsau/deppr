# -----------------------------------------------------------------------------
library(magrittr)
library(tidyverse)

get_eps_j = function(x_vec, y_val, sgn){
  if(sgn == "+") eps_j = sum(pmax(x_vec - y_val, 0))
  if(sgn == "-") eps_j = sum(pmax(y_val - x_vec, 0))
  return(eps_j)
}

get_div_sum <- function(y, x, M, N = length(y)){
  
  y = sort(y)
  eps_j_pos <- sapply(y, get_eps_j, x_vec = x, sgn = "+")
  eps_j_neg <- sapply(y, get_eps_j, x = x, sgn = "-")
  eps_j_delta <- eps_j_neg - eps_j_pos
  alpha_j = ((1:length(y)) - 0.5)/N

  # term one 
  div_term = 2/N*sum(eps_j_pos) + 2/N*sum(alpha_j*eps_j_delta)
  return(div_term)

}

get_min_div <- function(X, H_n, M, ...){
  
  if(nrow(H_n) != nrow(X)) stop("Matrix dimensions do not match")
  
  num_dates = nrow(H_n)
  div_vec = rep(NA, num_dates)
  for(i in 1:num_dates) 
    div_vec[i] <- get_div_sum(y = H_n[i,], x = X[i,], M = M)
  
  total_div = sum(div_vec)
  
  return(total_div)
  
}

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

X = all_data %>%
  dplyr::select(starts_with("SIM")) %>%
  as.matrix()

H_n = all_data %>%
  dplyr::select(starts_with("Hist")) %>%
  as.matrix()

min_div <- get_min_div(X, H_n, M)


