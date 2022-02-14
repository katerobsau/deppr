#' Minimum Divergence
#'
#' @description This function calculates the minimum divergence between an
#' ensemble forecast and a corresponding set of historical observations.
#'
#' @param x_vec a vector of giving values of an ensemble forecast.
#' The length of the vector corresponds
#' to the number of ensemble members.
#' @param hist_vec a vector of historical observations.
#'
#' @details Let the forecast distribution
#' be given by \eqn{F^{\mathcal{f}}(x)}. For a set of historical dates,
#' \eqn{\mathcal{H}}, let the empirical distribution function that corresponds to the
#' observations of the dates in \eqn{\mathcal{H}} be
#' given by \eqn{F^{\mathcal{H}}(x)}. The minimum divergence is
#' \deqn{\Delta^{\mathcal{H}} = \int \left(F^{\mathcal{H}}(x) - F^{\mathcal{f}}(x) \right)^2 \text{dx}}
#'
#' Computation of this minimum divergence term is greatly simplified using
#' Appendix A of Scheuerer et al. (2017).
#'
#' @return A value for the minimum divergence
#'
#' @seealso \code{\link{get_total_divergence}} and \code{\link{get_mindiv_epsilon}}.
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
get_minimum_divergence <- function(x_vec, hist_vec){

  y = sort(hist_vec)
  num_obs = length(y)
  eps_j_pos <- sapply(y, get_mindiv_epsilon, x_vec = x_vec, sgn = "+")
  eps_j_neg <- sapply(y, get_mindiv_epsilon, x_vec = x_vec, sgn = "-")
  eps_j_delta <- eps_j_neg - eps_j_pos
  alpha_j = ((1:length(y)) - 0.5)/num_obs

  # Term one (see Appendix A of Scheurer et al. 2020)
  div_term = 2/num_obs*sum(eps_j_pos) + 2/num_obs*sum(alpha_j*eps_j_delta)
  return(div_term)

}
