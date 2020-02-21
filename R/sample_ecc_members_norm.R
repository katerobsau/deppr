#' Samples ensemble members from a normal distribution
#'
#' This functions samples ensembe members from a normal distribution using
#' the parameters provided. The type of sampling can be one of ECC-R (random),
#' ECC-Q (uniform  quantiles) or ECC-S (random quantiles).
#'
#' @param num_members number of members in the ensemble (must be an integer)
#' @param pars a data frame with named columns \code{mu} and \code{sigma}
#' @param draw_type one of the characters, \code{'R'}, \code{'Q'} or \code{'S'}.
#' This character corresponds to the desired ECC sampling method
#'
#' @return a matrix where the rows give the ensemble members sampled using
#' the paremeters given by the the corresponding row of \code{pars}.
#'
#' @author Kate Saunders
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
#' @examples
#'
#' pars <- data.frame(mu = c(1,2), sigma  = c(1,1))
#' sample_ecc_members_norm(5,  pars,  'R')
#' sample_ecc_members_norm(5,  pars,  'Q')
#' sample_ecc_members_norm(5,  pars,  'S')
#'
#'@export
sample_ecc_members_norm <- function(num_members, pars, draw_type = "R"){

  if(draw_type == "R")
    sampled_members <- mapply(rnorm, mean = pars$mu, sd = pars$sigma,
                            MoreArgs = list(n = num_members))

  if(draw_type == "Q"){
    quantiles <- (1:num_members) / (num_members + 1)
    sampled_members <- mapply(qnorm, mean = pars$mu, sd = pars$sigma,
                            MoreArgs = list(p = quantiles))
  }

  if(draw_type == "S"){
    quantiles <- sapply(1:num_members,
                        function(i){runif(1)/num_members + (i-1)/num_members})
    sampled_members <- mapply(qnorm, mean = pars$mu, sd = pars$sigma,
                            MoreArgs = list(p = quantiles))
  }

  sampled_members <- sampled_members %>% t()

  return(sampled_members)

}
