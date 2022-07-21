#' Generic Preprocessing Function
#'
#' Generic function for preprocessing defined in other packages.
#'
#' @param x,... parameters.
#'
#' @export
sa.preprocessing<-function(x, ...){
  UseMethod("sa.preprocessing")
}


#' Generic Function for Seasonal Adjustment Decomposition
#'
#' Generic function to format the seasonal adjustment decomposition components.
#' \code{sa.decomposition()} is a generic function defined in other packages.
#'
#' @param y,sa,t,s,i,mul seasonal adjustment decomposition parameters.
#' @param x the object to print.
#' @param n_last_obs number of observations to print (by default equal to the frequency of the series).
#' @param ... further arguments (ignored).
#'
#' @return \code{"JD3_SADECOMPOSITION"} object.
#' @name sa.decomposition
NULL

#' @export
#' @rdname sa.decomposition
sa.decomposition<-function(x, ...){
  UseMethod("sa.decomposition")
}

