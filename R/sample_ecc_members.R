#' Samples ensemble members from a normal distribution
#'
#' This functions samples ensembe members from the provided distribution function.
#' The type of sampling can be one of ECC-R (random),
#' ECC-Q (uniform  quantiles) or ECC-S (jittered quantiles).
#'
#' @param num_members number of members in the ensemble (must be an integer)
#' @param function_type a function to simulate the members from
#' @param pars a data frame with named columns corresponding to parameters
#' @param ecc_type one of the characters, \code{'R'}, \code{'Q'} or \code{'S'}.
#' This character corresponds to the desired ECC sampling method
#'
#' @return a matrix where the columns give the sampled ensemble members
#'
#' @details If the \code{ecc_type} is \code{'R'} then the function_type should
#' be for random sampling of quantiles, ie. \code{`rnorm`}. If the
#' \code{ecc_type} is \code{'Q'} or \code{'S'} then the function_type should
#' be for quantile sampling, ie \code{`qnorm`}.
#'
#' This function uses \code{\link{get_ecc_quantiles}} for quantile sampling.
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
#' @seealso \code{\link{get_ecc_quantiles}} and \code{\link{reorder_members}}
#'
#' @examples
#'
#' num_members = 5
#' pars <- data.frame(mean = 0, sd = 1)
#' sample_ecc_members(num_members,  rnorm, pars, 'R')
#' sample_ecc_members(num_members, qnorm,  pars,  'Q')
#' sample_ecc_members(num_members, qnorm,  pars, 'S')
#'
#' pars <- data.frame(mean = c(0,10), sd = c(1,1))
#' sample_ecc_members(num_members,  rnorm, pars, 'R')
#'
#' num_members = 4
#' pars <- data.frame(rate = c(1,2,3))
#' sample_ecc_members(num_members, rexp,  pars, 'R')
#'
#' @export
sample_ecc_members <- function(num_members, function_type, pars,
                               ecc_type){

#     if(substr(as.character(function_type), 1,1) =="r" & ecc_type != 'R')
#       warning("Check: When the function_type starts with r, the ecc_type is commonly R")
#
#     if(substr(function_type, 1,1) ==  "q" & !(ecc_type %in% c('Q', 'S')))
#       warning("Check: When the function_type starts with q, the ecc_type is commonly Q or S")

# also like to add flexibility so the function name can change

  simulated_members <- apply(pars, 1, function(row, n, ecc_type, ...){

    par_list = as.list(row)
    if(ecc_type == "R") par_list$n = n
    if(ecc_type == "Q") par_list$p = get_ecc_quantiles(n, "Q")
    if(ecc_type == "S") par_list$p = get_ecc_quantiles(n, "S")

    simulated_values = do.call(what = function_type, args = par_list)

    return(simulated_values)

    }, n = num_members, ecc_type) %>% t()

  return(simulated_members)

}
