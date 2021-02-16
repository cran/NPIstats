#' NPI for comparing two groups
#'
#' NPI lower and upper probabilities for the event that the next future observation from group  Y is larger than the next future observation from group  X.
#'
#' @param X numeric vector of data values
#' @param Y numeric vector of data values, to check if it is the best group
#'
#' @return NPI lower and upper probabilities for the event that the next future observation from group  Y is larger than the next future observation from group  X.
#' @export
#'
#' @references F.P.A. Coolen (1996). Comparing two populations based on low stochastic structure assumptions. Statistics & Probability Letters 29, 297-305.
#' @examples
#' data(BreakdownTimes)
#' data2<-split(BreakdownTimes$times, BreakdownTimes$group)
#' # No terminated tails, complete data
#' best.pair(data2$X, data2$Y)

best.pair<-function(X, Y){
X<-sort(X)
Y<-sort(Y)

nx <- length(X)
ny <- length(Y)

# Calculate M-functions
Mx <-rep(1, nx+1)/(nx +1)
My <-rep(1, ny+1)/(ny +1)

XL <- c(-Inf, X)
XU <- c(X, Inf)
YL <- c(-Inf, Y)
YU <- c(Y, Inf)


# To calculate  X<Y times M-function or Prob
funL <- function(X, Y) {
  return(outer(X, Y, "<"))
}


# To calculate  X<Y times M-function or Prob
funU <- function(X, Y) {
  return(outer(X, Y, "<="))
}


# Lower and Upper probabilities that Y is the best
Lprob<-sum(Mx %*% funL(XU, YL) %*% My)
Uprob<-sum(Mx %*% funU(XL, YU) %*% My)

return(round(c(Lprob=Lprob,Uprob=Uprob),4))
}

