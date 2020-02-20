#' Reorders ensemble members
#'
#' Reorders the row entries of a matrix using a template based on order statistics
#'
#' @param X is a matrix where the columns correspond to multivariate forecasts.
#' @param B is a matrix with common dimension to X, and contains order statsitics for
#' reshuffling
#'
#' @return a reshufflied version of matrix according the order statistics given in B.
#'
#' @details
#' Each columns of X corresponds to an ensemble member. The order statistics in B are
#' generated from climatologically similar days to the forecast day.
#' This function is internal and used within \code{schaake_shuffle()}
#'
#' @author Kate Saunders
#'
#' @examples
#'
#' X = matrix(c(2,1,3, 5,6,7), nrow = 2, byrow = TRUE)
#' B = matrix(c(2,1,3, 3,2,1), nrow = 2, byrow = TRUE)
#' reorder_members(X, B)
#'
reorder_members <- function(X, B){

  if(!is.matrix(X) | !is.matrix(B))
    stop("M and B should both be matrices")

  X_new <- sapply(1:nrow(X), function(i,X,B){
    X[i,][B[i,]]
  }, X, B) %>% t()

  return(X_new)

}
