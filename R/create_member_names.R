#' Creates member names
#'
#' Takes a prefix string, ie "fc" and creates member reference names, ie, "fc001" "fc002"
#'
#' @param M prefix string
#' @param num_members integer giving the number of ensemble members
#' @param width (optional) this is the width for zero padding with the prefix string
#' (default  is 3)
#' @return a vector containing the names of the ensemble members
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @examples
#' create_member_names(prefix_string = "fc", num_members = 10)
#' create_member_names(prefix_string = "fc", num_members = 10, width = 2)
#'
#' @export
create_member_names <- function(prefix_string, num_members, width){
  if(missing(width)){width  = 3}
  col_names <- formatC(1:num_members, flag = "0", width = width) %>%
      paste0(prefix_string, .)
  return(col_names)
}
