#' Title Number of tickets to sell determiner
#'
#' @param N The number of available sits on the plane
#' @param gamma The probability that the plane will be overbooked
#' @param p The probability of a show
#'
#' @importFrom graphics abline curve plot
#' @importFrom stats pbinom pnorm qbinom qnorm uniroot
#'
#' @return 2 graphs and a list
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
ntickets=function(N,gamma,p){
  nd=N
  nc=N
  for(i in 0:50){
    if(qbinom(1-gamma,nd+i,p) == N & round(qnorm(1-gamma,(nc+i)*p,sqrt(((nc+i)*p) * (1-p))), digits=0) == N){
      nd=nd+i
      nc=nc+i
      break
    }
  }

  print(list(nd=nd,nc=nc,N=N,p=p,gamma=gamma))

  ## Discrete graph
  discrete <- function(n) {
    1 - gamma - pbinom(N, n, p)
  }

  n_discrete <- c()
  obj_discrete <- c()

  for(i in 0:20) {
    temp_n <- c(N + i)
    temp_obj <- discrete(N + i)
    n_discrete <- c(n_discrete, temp_n)
    obj_discrete <- c(obj_discrete, temp_obj)
  }


  main = paste0("Objective Vs n to find optimal tickets sold(", nc, ") gamma=", gamma, " N=", N, "discrete")


  plot(n_discrete, obj_discrete, type = "b", frame = FALSE, pch = 19, xlab = "n", ylab = "Objective",
       lty = 1, lwd = 1, main=main, cex.main=0.7)
  abline(h = 0, v = nc, col = "red")

  ## Continuous graph
  continuous <- function(n) {
    1 - gamma - pnorm(N, n*p, sqrt(n*p*(1-p)))
  }

  x <- uniroot(continuous, c(N, N+20))
  main = paste0("Objective Vs n to find optimal tickets sold(", x$root, ") gamma=", gamma, " N=", N, "continuous")

  curve(continuous(x),xlim=c(N, N+20), xlab="n", ylab="Objective", main=main, cex.main=0.7)
  abline(h = 0, v = x$root, col = "gray")

}
