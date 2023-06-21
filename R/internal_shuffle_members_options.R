#' Function to rank, order or sort ensemble members
#'
#' Applies one of the functions rank, order or sort to the ensemble members.
#'
#' @param M is a matrix where the columns correspond to multivariate forecasts.
#' @param type is a string of either 'rank', 'order' or 'sort'.
#' This operation is applied to the rows
#'
#' @return a matrix where the ensemble members in M have been altered
#' according to the function
#'
#' @details
#' No missing values should be present in M.
#'
#' This function is used within \code{schaake_shuffle()} and \code{ecc()}.
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @examples
#'
#' M = matrix(c(2,3,1, 5,6,7), nrow = 2, byrow = TRUE)
#' shuffle_members(M, 'rank')
#' shuffle_members(M, 'order')
#' shuffle_members(M, 'sort')
#'
#'@export
shuffle_members_options <- function(M, type,  ...){

  M_new <- switch(type,
                rank  = rank_members(M,  ...),
                order = order_members(M,  ...),
                sort = sort_members(M, ...))

  return(M_new)

}

