#' Shuffles a post-processed forecast to restore dependence
#'
#' Need to update here
#'
#' @param X is a matrix where the columns correspond to multivariate forecasts.
#' @param Y is a matrix where the columns correspond to the template for
#' reshuffling
#'
#' @return a matrix where the post-processed forecast with no dependence
#' has been reshuffled according to the template to produce a forecast with
#' dependence
#'
#' @details
#' Need to update these
#'
#' @author Kate Saunders and Kirien Whan
#'
#' @references
#'
#' Clark, Martyn, et al. "The Schaake shuffle: A method for reconstructing
#' space–time variability in forecasted precipitation and temperature fields."
#' Journal of Hydrometeorology 5.1 (2004): 243-262.
#'
#'
#' Schefzik, Roman, Thordis L. Thorarinsdottir, and Tilmann Gneiting.
#' "Uncertainty quantification in complex simulation models using ensemble
#' copula coupling." Statistical science 28.4 (2013): 616-640.
#'
#' Hu, Yiming, et al. "A stratified sampling approach for improved sampling
#' from a calibrated ensemble forecast distribution." Journal of Hydrometeorology
#' 17.9 (2016): 2405-2417.
#'
#' @examples
#'
#'@export
run_shuffle_template <- function(forecast, template){

  if(any(is.na(forecast))) stop("forecast should not contain missing values")
  if(any(is.na(template))) stop("template should not contain missing values")
  if(!all(dim(forecast) == dim(template))) stop("the dimensions of forecast and template should match")


  sorted_forecast = sort_members(forecast)
  ranked_template = rank_members(template)

  forecast_with_dependence = reorder_members(sorted_forecast, ranked_template)

    return(forecast_with_dependence)

}
