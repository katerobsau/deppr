#' Creates a vector of quantiles
#'
#' This function produces the qunatilesf or For ECC-Q (uniform  quantiles) or
#' ECC-S (jittered quantiles).
#'
#' @param m number of members in the ensemble (must be an integer)
#' @param ecc_type one of the characters \code{'Q'} or \code{'S'}.
#' This character corresponds to the desired ECC sampling method
#'
#' @return a vector of sample quantiles
#'
#' @details If the \code{ecc_type} is \code{'Q'} then qunatiles are equally sampled.
#' If the \code{ecc_type} is \code{'S'} then the quantiles are jittered.
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
#' @seealso \code{\link{sample_ecc_members}}
#'
#' @examples
#' get_ecc_quantiles(3, "Q")
#' get_ecc_quantiles(5, "S")
#'
get_ecc_quantiles <- function(m, ecc_type){
  quantiles <- switch(ecc_type,
                      Q = (1:m)/(m + 1),
                      S = sapply(1:m, function(i){runif(1)/m + (i-1)/m})
  )
  return(quantiles)
}

