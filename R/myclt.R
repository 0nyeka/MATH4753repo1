#' Title uniform distribution
#'
#' @param n number of rows
#' @param iter number of columns
#'
#'
#' @importFrom graphics hist
#' @importFrom stats runif
#'
#' @return a histogram
#' @export
#'
#' @examples
#' myclt(10,1000)
myclt=function(n,iter){
  y=runif(n*iter,0,5)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  mn=apply(data,2,mean)
  hist(mn)
  mn
}
