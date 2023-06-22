#' Performs the schaake shuffle
#'
#' For a forecast that has been subject to univariate post-processing the members
#' need to be reshuffled to so that the forecast has the  correct dependence structure.
#' This function does the reshuffling using a dependence structure that is inherited from
#' days that have are climatologically similar.
#'
#' @param X is a matrix where the columns correspond to multivariate forecasts.
#' @param Y is a matrix where the columns give climatologically similar observations to
#' that of the forecast day.
#'
#' @return a matrix where the forecasts in X have been reshuffled according to the
#' dependence inherited from the climatological template in Y.
#'
#' @details
#' For the input matrix X, the number of rows correspond to the dimension of the
#' multivariate forecast and the number of columns corresponds the ensemble members.
#' The dimension of Y must correspond to X. To get climatologically similar days use
#' the function \code{sample_schaake_dates()}.
#'
#' No missing values should be present in Y. Data imputation or date resampling
#' should be used.
#'
#' This function is functionally the same as \code{apply_ecc_template()}, but to be
#' consistent with the formulation in the orginal papers has different inputs.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#'
#' Clark, Martyn, et al. "The Schaake shuffle: A method for reconstructing
#' space–time variability in forecasted precipitation and temperature fields."
#' Journal of Hydrometeorology 5.1 (2004): 243-262.
#'
#' @examples
#'
#' # code for this example was based on the function vs_sample()
#' # in the scoringRules package
#'
#' d <- 3  # number of dimensions
#' m <- 5  # number of samples from multivariate forecast distribution
#'
#' mu0 <- rep(0, d)
#' mu <- rep(1, d)
#' S0 <- S <- diag(d)
#' S[S==0] <- 0.1
#' S0[S0==0] <- 0.2
#'
#' # generate samples from multivariate normal distributions
#' obs <- drop(mu0 + rnorm(d) %*% chol(S0))
#' climate_example <- replicate(m, drop(mu + rnorm(d) %*% chol(S)))
#'
#' forecast_example <- matrix(mu0 + rnorm(d*m), nrow = d, ncol = m)
#'
#' schaake_shuffle(X = forecast_example, Y = climate_example)
#'
#'@export
schaake_shuffle <- function(X, Y){

  if(any(is.na(X))) stop("X should not contain missing values")
  if(any(is.na(Y))) stop("Y should not contain missing values")
  if(!all(dim(X) == dim(Y))) stop("the dimensions of X and Y should match")

  Chi = sort_members(X)
  # gamma = sort_members(Y)
  # B = order_members(Y_inter)
  B_map = rank_members(Y)
  X_ss = reorder_members(Chi, B_map)

  return(X_ss)

}
