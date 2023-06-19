#' Creates a vector of quantiles
#'
#' This function produces qunatiles for ECC-R (random qunatiles),
#' ECC-Q (uniform  quantiles) or ECC-S (jittered quantiles).
#'
#' @param n_members number of members in the ensemble (must be an integer)
#' @param method one of the characters \code{'random'}, \code{'equally_spaced'}, \code{'Q1'},
#'  or \code{'S'}. This character corresponds to the desired sampling method.
#'  There are two types of equi-spaced quantiles, 'equally_spaced' and 'Q1'.
#'
#' @return a vector of sample quantiles
#'
#' @details If the \code{method} is \code{'random'} then quantiles are randomly sampled.
#' If the \code{method} is \code{'equally_spaced'} then qunatiles are equally sampled.
#' #' If the \code{method} is \code{'Q1'} then quantiles are equally sampled (Bröck et al. 2012)
#' If the \code{method} is \code{'S'} then the quantiles are jittered.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#'
#' Schefzik, Roman, Thordis L. Thorarinsdottir, and Tilmann Gneiting.
#' "Uncertainty quantification in complex simulation models using ensemble
#' copula coupling." Statistical science 28.4 (2013): 616-640.
#'
#' Hu, Yiming, et al. "A stratified sampling approach for improved sampling
#' from a calibrated ensemble forecast distribution." Journal of Hydrometeorology
#' 17.9 (2016): 2405-2417.
#'
#' Bröck et al. 2012
#'
#' @seealso \code{\link{get_quantiles}}
#'
#' @examples
#' get_quantiles(n_members = 3, method = "random")
#' get_quantiles(n_members = 4, method = "equally_spaced")
#' get_quantiles(n_members = 4, method = "equally_spaced_shift")
#' get_quantiles(n_members = 5, method = "jittered")
#'
get_quantiles <- function(n_members, method){

  quantiles <- switch(method,
                      random = runif(n_members),
                      equally_spaced = (1:n_members)/(n_members + 1),
                      jittered = (1:n_members - 1)/n_members + runif(n_members)/n_members,
                      equally_spaced_shift = (1:n_members - 0.5)/n_members)

  return(quantiles)

}

