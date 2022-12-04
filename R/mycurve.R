#' Title a normal curve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a endpoint
#'
#' @importFrom graphics curve
#' @importFrom stats dnorm
#'
#' @return a normal graph
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){
  x <-NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, a))
}


