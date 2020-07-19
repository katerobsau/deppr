#' Total Minimum Divergence
#'
#' @description This function computes the
#' minimum divergence between an ensemble forecast and a corresponding
#' set of historical observations. The sum of the minimum divergence over all
#' locations and lead times is then returned.
#'
#' @param X_mat a matrix where rows correspond to different locations and lead times
#' for a given ensemble forecast. The columns correspond to the number of members.
#' @param H_mat a matrix of historical observations. The rows and columns of this
#' matrix are arranged in such a way that the observational trajectories are
#' consistent with the locations and lead times of the rows in \code{X_mat}.
#'
#' @details This function operates on the two input matrices row-wise, calculating
#' the minimum divergence for a given location and lead time. The sum over all
#' locations and lead times is then returned.
#'
#' @return a numeric value giving the total divergence
#'
#' @seealso \code{\link{get_minimum_divergence}}
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
#' num_members = 50
#' x = dnorm(seq(-3,3, length.out = num_members))
#' print("FINISH EXAMPLE")
#'
#' @export
#'
get_total_divergence <- function(X_mat, H_mat){

  if(nrow(H_mat) != nrow(X_mat)) stop("Number of rows does not match")

  num_dates = nrow(H_mat)
  div_vec = rep(NA, num_dates)
  for(i in 1:num_dates)
    div_vec[i] <- get_minimim_divergence(y = H_mat[i,], x = X_mat[i,])

  total_div = sum(div_vec)

  return(total_div)

}
