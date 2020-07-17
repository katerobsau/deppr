#' Performs empirical copula coupling
#'
#' Reshuffles univariate forecasts so that the multivariate forecast inherits the
#' dependence structure the raw ensemble.
#'
#' @param X_raw is a matrix where the columns correspond to the raw ensemble members
#' @param Y_forecast is a matrix where the entries correspond to the univariate forecasts
#'
#' @return a matrix where the columns corresponds to the post-processed
#' multivariate forecasts
#'
#' @details Univariate forecasts are generated using \code{sample_ecc_members_norm()}.
#' These univariate forecasts are then reshuffled using a dependence template
#' inherited from the raw ensemble, \code{X_raw}. This
#' process is known as Empircal Copula Coupling (ECC).
#' The type of ECC depends on the  sampling type used to generate the members in
#' \code{sample_ecc_members_norm}.
#'
#' This function is functionally the same as \code{schaake_shuffle()}, but to be
#' consistent with the formulation in the orginal papers has different inputs.
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
#'@examples
#' # code for this example was based on the function vs_sample()
#' # in the scoringRules package
#'
#' d <- 3  # number of dimensions
#' m <- 5  # number of ensemble members

#' mu0 <- rep(0, d)
#' mu <- rep(1, d)
#' S0 <- S <- diag(d)
#' S[S==0] <- 0.1
#' S0[S0==0] <- 0.2
#'
#' # generate samples from multivariate normal distributions
#' obs <- drop(mu0 + rnorm(d) %*% chol(S0))
#' raw_ensemble <- replicate(m, drop(mu + rnorm(d) %*% chol(S)))
#'
#' pars = data.frame(mean = mu0, sd = rep(1, d))
#' draw_type = 'R'
#' univariate_forecast <- sample_ecc_members(num_members = m,  rnorm, pars,  draw_type)
#'
#' apply_ecc_template(raw_ensemble, univariate_forecast)
#'
#' @export
apply_ecc_template <- function(X_raw, Y_forecast){

  if(any(is.na(X_raw))) stop("X_raw should not contain missing values")
  if(any(is.na(Y_forecast))) stop("Y_forecast should not contain missing values")
  if(!all(dim(X_raw) == dim(Y_forecast))) stop("the dimensions of X and Y should match")

  X_rank <- rank_members(X_raw)
  Y_sort <- sort_members(Y_forecast)
  Y_reordered <- reorder_members(Y_sort, X_rank)

  return(Y_reordered)

}
