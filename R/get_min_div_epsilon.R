#' Minimum Divergence Helper
#'
#' @description This function is a helper that computes terms needed to
#' estimate the minimum divergence between an ensemble forecast
#' and a set of historical observations.
#'
#' @param y_val a numeric value of an historical observation
#' @param x_vec a vector of giving values of an ensemble forecasts.
#' The length of the vector corresponds
#' to the number of ensemble members.
#' @param sgn is one of the characters, "+" or "-".
#'
#' @details This function calculates the difference between an
#' historical observation and the members of an ensemble forecast.
#' This difference is then passed to a sign function. The ensemble average
#' of the evaluated sign function is returned.
#'
#' The sign functions is mathematically written as $f(x) = (x)_+$. This
#' is equivalent operation to $f(x) = max(x, 0)$.
#'
#' Let $M$ be the number of members, then if the \code{sgn} is "+"
#' $ \epsilon_+ = \frac{1}{M} \Sum_{i = 1}^M (y - x_i)_+ $,
#' otherwise, if \code{sgn} is "-"
#' $ \epsilon_- = \frac{1}{M} \Sum_{i = 1}^M (x_i - y)_+ $,
#'
#' These epsilon terms are further described in Scheuerer et al. (2017).
#' They are computed separately for computational efficiency.
#'
#' @return a value for $\epsilon_+$ (or $\epsilon_-$).
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
#' num_members = 10
#' x = dnorm(seq(-3,3, length.out = num_members))
#' get_mindiv_epsilon(x_vec = x, y_val = -3, sgn = "+")
#' get_mindiv_epsilon(x_vec = x, y_val = -3, sgn = "-")
#' get_mindiv_epsilon(x_vec = x, y_val = 3, sgn = "-")
#' get_mindiv_epsilon(x_vec = x, y_val = 3, sgn = "-")
#'
#' @export
#'
get_mindiv_epsilon = function(x_vec, y_val, sgn){
  if(sgn == "-") eps_j = sum(pmax(x_vec - y_val, 0))/length(x_vec)
  if(sgn == "+") eps_j = sum(pmax(y_val - x_vec, 0))/length(x_vec)
  return(eps_j)
}
