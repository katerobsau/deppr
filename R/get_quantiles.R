#' Quantile sampling
#'
#' Creates a matrix of sampled quantiles for drawing new ensemble members
#'
#' @param n_members number of members in the ensemble. This must be an integer.
#' @param method character string corresponding to the desired quantile sampling method
#' and must be one of \code{'R'}, \code{'Q'}, \code{'QJ'} or \code{'QH'}.
#' @param n_reps number of replicates of the function corresponding to the
#' dimension of the forecast

#' @return a matrix of sample quantiles where the number of rows corresponds to
#' n_reps and the number of columns corresponds to n_members
#'
#' @details
#' If the \code{method} is \code{'R'} then the quantiles are randomly sampled.
#' If the \code{method} is \code{'Q'} then the sampled quantiles are equally spaced.
#' If the \code{method} is \code{'QH'} then the sampled quantiles are equally spaced using a different sampling scheme.
#' If the \code{method} is \code{'QJ'} then the sampled quantiles are equally spaced and jittered.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @seealso \code{\link{run_shuffle_template}}
#'
#' @examples
#'
#' # Examples of the different sampling methods
#' get_quantiles(n_members = 3, method = "R")
#' get_quantiles(n_members = 4, method = "Q")
#' get_quantiles(n_members = 4, method = "QJ")
#' get_quantiles(n_members = 5, method = "QH")
#'
#' # Examples showing difference in methods for repeat sampling
#' get_quantiles(n_members = 3, method = "R", n_reps = 2)
#' get_quantiles(n_members = 3, method = "Q", n_reps = 2)
#'
get_quantiles <- function(n_members, method, n_reps = 1){

  if(!is.integer(n_members))
    error("Number of members, n_members, must be an integer")

  if(!(method %in% c("R", "Q", "QJ", "QH")))
    error("Quantile sampling method must be one of R, ES, ESJ or ESH. See help.")

  if(!is.integer(n_reps))
    error("Number of replicates must be an integer")

  if(n_reps < 1)
    error("Number of replicates must be greater than or equal to 1")

  quantiles <- replicate(
                switch(method,
                      R = runif(n_members),
                      Q = (1:n_members)/(n_members + 1),
                      QJ = (1:n_members - 1)/n_members + runif(n_members)/n_members,
                      QH = (1:n_members - 0.5)/n_members),
                n = n_reps) %>%
    matrix(., byrow = TRUE, nrow = n_reps)

  return(quantiles)

}

