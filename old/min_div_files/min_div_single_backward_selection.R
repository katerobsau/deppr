#' Backward Selection for Minimum Divergence Schaake Shuffle
#'
#' @description Does stuff
#'
#' @param X_mat a matrix where rows correspond to different locations and lead times
#' for a given ensemble forecast. The columns correspond to the number of members.
#' @param H_mat a matrix of historical observations. The rows and columns of this
#' matrix are arranged in such a way that the observational trajectories are
#' consistent with the locations and lead times of the rows in \code{X_mat}.
#' @param backward_num the number of dates to retain after backward selection
#'
#' @details details about stuff
#'
#' @return A reduced version of the matrix for \code{H_mat}
#' with \code{backward_num} columns.
#'
#' @seealso \code{\link{get_mindiv_epsilon}}
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#' Scheuerer, Michael, et al.
#' "A method for preferential selection of dates in the
#' Schaake shuffle approach to constructing spatiotemporal
#' forecast fields of temperature and precipitation."
#' Water Resources Research 53.4 (2017): 3029-3046.
#'
#' @examples
#'
#' @export
#'
#' X_mat = matrix(rnorm(20), nrow = 2)
#' H_mat = matrix(rnorm(40), nrow = 2)
#' H_mat[,19] = H_mat[,19] + 5
#' H_mat[,20] = H_mat[,20] + 10
#' min_row1 = get_minimum_divergence(X_mat[1,], H_mat[1,])
#' min_row2 = get_minimum_divergence(X_mat[2,], H_mat[2,])
#' total_div = get_total_divergence(X_mat, H_mat)
#' total_div == (min_row1 + min_row2)
#' H_mat_reduced = backward_selection_mindiv(X_mat = X_mat, H_mat = H_mat, backward_num = 19)
#' all(H_mat[, 1:19] == H_mat_reduced)
#' H_mat_reduced = backward_selection_mindiv(X_mat = X_mat, H_mat = H_mat, backward_num = 18)
#' all(H_mat[, 1:18] == H_mat_reduced)
backward_selection_mindiv <- function(X_mat, H_mat, backward_num){

  if(ncol(H_mat) < backward_num)
    stop("Too few historical observations for backward selection, reducce backward_num")

  num_dates = ncol(H_mat)
  total_div_minus_j = rep(NA, num_dates)

  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cl)

  total_div_minus_j <- foreach(j = 1:num_dates, .packages = c("depPPR")) %dopar% {
    get_total_divergence(X_mat = X_mat, H_mat = H_mat[,-j])
  } %>% unlist()

  parallel::stopCluster(cl)

  rank_cols = rank(total_div_minus_j)
  keep_cols <- which(rank_cols > num_dates - backward_num)
  H_mat_reduced = H_mat[ ,keep_cols]

  return(H_mat_reduced)

}
