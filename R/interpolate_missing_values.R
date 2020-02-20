#' Interpolates missing observations
#'
#' Occasionally missing observations are present in observations sampled that are to be used in the
#' Schaake shuffle. If only a few of observations are missing if can be useful to interpolate
#' the missing values, particularly if only a few dates are available for sampling.
#'
#' @param M is a matrix where columns corresponds to different sample observations
#'
#' @return M with any missing values interpolated
#'
#' @details For each column with a missing observation, the next closest column is found
#' in terms of the smallest mean square error. If this close column has a valid observation in the same
#' row as the missing observation, then this value is used to interpolate the missing
#' observation. This is a simple method of data imputation. Care should be taken that
#' the reason the observation was missing was not for a systematic reason.
#'
#' @author Kate Saunders
#'
#' @examples
#' M = matrix(c(1,2,3,1,NA,3,4,5,6), nrow = 3)
#' interpolate_missing_values(M)
#'
#' M = matrix(c(-1,-2,-3,1,NA,3,4,5,6), nrow = 3)
#' interpolate_missing_values(M)
#'
interpolate_missing_values <- function(M){

  M <- as.matrix(M)
  missing_ij <- which(is.na(M), arr.ind = T)

  fill_ij <- apply(missing_ij, 1, function(row_ij, M){

    r = row_ij[1]; c = row_ij[2]

    member_rep <- matrix(rep(M[,c], ncol(M)), nrow = nrow(M), byrow = FALSE)
    ordered_mse_members = (M - member_rep)^2 %>% colMeans(na.rm = TRUE) %>% order()
    index = 1
    closest_member = ordered_mse_members[index]
    if(closest_member == c){
      index = 2;
      closest_member = ordered_mse_members[index]
    }

    fill_val= M[r, closest_member]
    while(is.na(fill_val) & select_member <= ncol(M)){
      index = index + 1
      closest_member = ordered_mse_members[index]
    }

    return(fill_val)

  }, M)

  M[missing_ij] = fill_ij

  # should probably exception handle all NA rows somewhere

  return(M)

}
