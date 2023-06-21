#' Thins dates for Minimum Divergence Schaake shuffling
#'
#' @description This function takes a set observational trajectories, comparing these
#' trajectories with those of the ensemble forecast. Trajectories with
#' observations falling outside the range of the post-processed ensemble are
#' eliminated systematically eliminated. Dates are eliminated systematically
#' in order based on how many observations fall outside the ensemble trajectory,
#' until the dates are sufficiently thinned.
#'
#' @param Pred_mat a matrix where rows correspond to different locations and lead times
#' for a given ensemble forecast. The columns give the lower and upper predictive
#' intervals for the marginal distribution.
#' @param H_mat a matrix of historical observations. The rows and columns of this
#' matrix are arranged in such a way that the observational trajectories are
#' consistent with the locations and lead times of the rows in \code{X_mat}.
#' @param retain_N the number of dates to retain after thinning
#'
#' @details This function is used to thin the number of observational trajectories
#' used in minimum divergence schaake shuffling. This initial screening of dates
#' greatly reduced the computational time needed to estimate the total
#' divergence and perform backward selection, see Scheuerer et al. (2017).
#'
#' @return A reduced version of the matrix for \code{H_mat}
#' with \code{retain_N} columns.
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
#' #WRITE AN EXAMPLE
#' @export
#'
thin_dates <- function(Pred_mat, H_mat, retain_N){

  outside_H <- apply(H_mat, 2, function(col, Pred_mat){
    min_col = Pred_mat[,1]
    max_col = Pred_mat[,2]
    outside_bool = (col < min_col) + (col > max_col)
    return(outside_bool)
  }, Pred_mat = Pred_mat)

  total_outside <- colSums(outside_H)
  keep_cols <- order(total_outside)[1:retain_N]

  H_mat_reduced = H_mat[,keep_cols]

  return(H_mat_reduced)

}
