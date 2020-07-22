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
#' X = matrix(rnorm(100), nrow = 2)
#' H = matrix(rnorm(40), nrow = 2)
#' backward_selection_mindiv(backward_num = 19, X_Mat = X, H_mat = H)
backward_selection_mindiv <- function(backward_num, X_mat, H_mat){

  total_div_minus_j = rep(NA, ncol(H_mat))

  # cl <- makeCluster(detectCores() - 1)
  # doParallel::registerDoParallel(cl)
  #
  # foreach(j = 1:ncol(H_mat), .packages = c("depPPR")) %dopar% {
  #   total_div_minus_j[j] <- get_total_divergence(X_mat = X_mat, H_mat = H_mat[,-j])
  # }

  for(j in 1:ncol(H_mat))
    total_div_minus_j[j] <- get_total_divergence(X_mat = X_mat, H_mat = H_mat[,-j])

  rank_vec = rank(total_div_minus_j)
  eliminate_dates = which(rank_vec > backward_num)
  H_mat_reduced = H_mat[ , -eliminate_dates]

  return(H_mat_reduced)

}
