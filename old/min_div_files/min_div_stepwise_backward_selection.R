stepwise_backward_selection_mindiv <- function(X_mat, H_mat, backward_seq){

  H_mat_reduced = H_mat
  names(H_mat_reduced) = paste("C", 1:ncol(H_mat), sep = "")
  for(i in 1:length(backward_seq)){

    print(dim(H_mat_reduced))

    H_mat_reduced <- backward_selection_mindiv(
      X_mat = X_mat,
      H_mat = H_mat_reduced,
      backward_num = backward_seq[i])

  }

  return(H_mat_reduced)
}
