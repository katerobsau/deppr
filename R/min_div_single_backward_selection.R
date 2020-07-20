single_backward_selection_mindiv <- function(backward_num, X_mat, H_mat){

  total_div_minus_j = rep(NA, ncol(H_mat))
  for(j in 1:ncol(H_mat))
    total_div_minus_j[j] <- get_total_divergence(X_mat = X_mat, H_mat = H_mat[,-j])

  rank_vec = rank(total_div_minus_j)
  eliminate_dates = which(rank_vec > backward_num)
  H_mat_reduced = H_mat[ , -eliminate_dates]

  return(H_mat_reduced)

}
