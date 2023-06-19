rank_members <- function(M, ties.method =  "first", ...){

  if(!is.matrix(M)) stop("M must be a matrix")
  if(any(is.na(M))) stop("Matrix must not have missing values")

  M_ranked <- apply(M, 1, rank, ties.method =  "random") %>%
    t()

  return(M_ranked)

}

order_members <- function(M, ...){

  if(!is.matrix(M)) stop("M must be a matrix")
  if(any(is.na(M))) stop("Matrix must not have missing values")

  B <- apply(M, 1, order) %>% t()

  return(B)
}

sort_members <- function(M, ...){

  if(!is.matrix(M)) stop("M must be a matrix")
  if(any(is.na(M))) stop("Matrix must not have missing values")

  M_sort <- apply(M, 1, sort) %>% t()

  return(M_sort)

}
