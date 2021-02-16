#' NPI for comparing two groups with terminated tails
#'
#' NPI lower and upper probabilities for the event that the next future observation from group  Y is larger than the next future observation from group  X.
#' The information available consists of precise measurements of real-valued data only within a specific range, between the cut points, where the numbers of observations to the left and to the right of this range available.
#' @param X numeric vector of data values
#' @param Y numeric vector of data values, to check if it is the best group
#' @param Lx numeric value, lower cut point for group X, default set to -Inf
#' @param Ux numeric value, upper cut point for group X, default set to Inf
#' @param Ly numeric value, lower cut point for group Y, default set to -Inf
#' @param Uy numeric value, lower cut point for group Y, default set to Inf
#'
#' @return NPI lower and upper probabilities for the event that the next future observation from group  Y is larger than the next future observation from group  X.
#' @export
#'
#' @references T.A. Maturi, P. Coolen-Schrijner and F.P.A. Coolen (2009). Nonparametric predictive pairwise comparison with terminated tails. International Journal of Approximate Reasoning, 51(1), 141-150.
#' @examples
#' data(BreakdownTimes)
#' data2<-split(BreakdownTimes$times, BreakdownTimes$group)
#' # No terminated tails, complete data
#' best.pair.tt(data2$X, data2$Y)
#' # terminated tails with Ly = 0.5, Uy = 4 and Ux = 10, but as Lx is not given then Lx=-Inf
#' best.pair.tt(data2$X, data2$Y, Ux = 10, Ly = 0.5, Uy = 4)
#'
best.pair.tt<-function(X, Y, Lx = -Inf, Ux = Inf, Ly = -Inf, Uy = Inf){

# Data & choose the cut points
X<-sort(X)
Y<-sort(Y)

nx <- length(X)
ny <- length(Y)

nlx <- sum(X < Lx)
nux <- sum(X > Ux)
nrx <- sum(X >= Lx & X <=Ux)

nly <- sum(Y < Ly)
nuy <- sum(Y > Uy)
nry <- sum(Y >= Ly & Y <=Uy)

# Calculate M-functions but without dividing over n+1
Mlx <- nlx/(nx +1)
Mrx <-rep(1, nrx)/(nx +1)
Mux <- nux/(nx +1)

Mly <- nly/(ny +1)
Mry <-rep(1, nry)/(ny +1)
Muy <- nuy/(ny +1)

Xr <- X[X >= Lx & X <= Ux]
XL <- c(Lx, Xr)
XU <- c(Xr, Ux)

Yr <-Y[Y >= Ly & Y <= Uy]
YL <- c(Ly, Yr)
YU <- c(Yr, Uy)



# To calculate  X<Y times M-function or Prob
funL <- function(X, Y) {
  return(outer(X, Y, "<"))
}


# To calculate  X<Y times M-function or Prob
funU <- function(X, Y) {
  return(outer(X, Y, "<="))
}

MxL<-c(Mlx, Mrx)
MxU<-c(Mrx, Mux)

MyL<-c(Mry, Muy)
MyU<-c(Mly, Mry)



# Lower and Upper probabilities that Y is the best
Lprob<-sum(MxL %*% funL(XU, YL) %*% MyL)
Uprob<-sum(MxU %*% funU(XL, YU) %*% MyU)

return(round(c(Lprob=Lprob,Uprob=Uprob),4))
}


