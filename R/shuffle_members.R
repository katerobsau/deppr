#' Function to rank, order or sort ensemble members
#'
#' Applies a function to rank, order or sort the ensemble members.
#'
#' @param M is a matrix where the columns correspond to multivariate forecasts.
#' @param type is a string of either 'rank', 'order' or 'sort'.
#' This operation will is applied to the rows
#'
#' @return a matrix where the ensemble members in X have had the operation type applied
#'
#' @details
#' No missing values should be present in X.
#'
#' This function is internal and used within \code{schaake_shuffle()}
#'
#' @author Kate Saunders
#'
#' @examples
#'
#' M = matrix(c(2,3,1, 5,6,7), nrow = 2, byrow = TRUE)
#' shuffle_members(M, 'rank')
#' shuffle_members(M, 'order')
#' shuffle_members(M, 'sort')
#'
shuffle_members <- function(M, type,  ...){

  M_new <- switch(type,
                rank  = rank_members(M,  ...),
                order = order_members(M,  ...),
                sort = sort_members(M, ...))

  return(M_new)

}

