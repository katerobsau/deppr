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
#' @examples
#'
#' pars <- data.frame(mu = c(1,2), sigma  = c(1,1))
#' sample_ecc_members_norm(5,  pars,  'R')
#' sample_ecc_members_norm(5,  pars,  'Q')
#' sample_ecc_members_norm(5,  pars,  'S')
#'
#'@export
sample_ecc_members_norm <- function(num_members, function_name, pars,
                                    ecc_type){

  simulated_members <- apply(pars, 1, function(row, n, ecc_type){

    par_list = as.list(row)
    if(ecc_type == "R") par_list$n = n
    if(ecc_type == "Q") par_list$p = get_ecc_quantiles(n, "Q")
    if(ecc_type == "S") par_list$p = get_ecc_quantiles(n, "S")

    simulated_values = do.call(what = function_name, args = par_list)

    return(simulated_values)

  }, n = num_members, ecc_type) %>% t()

  return(simulated_members)

}

get_ecc_quantiles <- function(m, ecc_type){
  quantiles <- switch(ecc_type,
    Q = (1:m)/(m + 1),
    S = sapply(1:m, function(i){runif(1)/m + (i-1)/m})
  )
  return(quantiles)
}
get_ecc_quantiles(3, "S")
get_ecc_quantiles(3, "S")
