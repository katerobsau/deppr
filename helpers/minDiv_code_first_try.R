# -----------------------------------------------------------------------------
library(magrittr)

get_eps_j = function(x_vec, y_val, sgn){
  if(sgn == "+") eps_j = sum(pmax(x_vec - y_val, 0))
  if(sgn == "-") eps_j = sum(pmax(y_val - x_vec, 0))
  return(eps_j)
}

get_div_term1 <- function(y, x, M, N = length(y)){

  eps_j_pos <- sapply(y, get_eps_j, x_vec = x, sgn = "+")
  eps_j_neg <- sapply(y, get_eps_j, x = x, sgn = "-")
  eps_j_delta <- eps_j_neg - eps_j_pos
  alpha_j = ((1:length(y)) - 0.5)/N

  div_term1 = 2/N*sum(eps_j_pos) + 2/N*sum(alpha_j*eps_j_delta)

}

get_min_div <- function(X, H_n, M, N = nrow(H_n)){
  min_div <- lapply(1:ncol(H_n), function(i){
    x = X[,i] 
    y = sort(H_n[,i])
    div_val <- get_div_term1(y = y, x = x, M = M, N)
    return(div_val)
  }) %>% 
    unlist() %>% 
    sum()
  return(min_div)
}

# -----------------------------------------------------------------------------
set.seed(1)

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
