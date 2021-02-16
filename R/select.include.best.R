#' NPI for selecting the subset including the best group
#'
#' NPI lower and upper probabilities for the event that at least one
#' of the next future observations from groups S is greater than all
#' future observations from the other groups.
#'
#' @param data a list of numeric data vectors
#' @param S a vector of indices of the groups in the data list to be considered as the subset of  groups that includes the best group.
#'
#' @return NPI lower and upper probabilities for the event that at least one of the next future observations from groups S is greater than all future observations from the other groups.
#' @export
#'
#' @examples
#' # NPI lower and upper probabilities for the event that  at least one
#' # of the next future observations from groups S is greater than all
#' # future observations from the other groups.
#'
#' data(FourSources)
#' # Convert the dataframe to a list of groups
#' data2<-split(FourSources$value,FourSources$source)
#' select.include.best(data2,S=c(1,3))


select.include.best <-function (data, S) {
  k <- length(data)
  jk <- 1:k
  NS <- jk[-S]
  ls <- length(S)
  lns <- length(NS)

  data <- lapply(data, function(x) sort(x)) # to sort the data
  dataL <- lapply(data, function(x) c(-Inf,x))  # add -Inf to each group
  dataU <- lapply(data, function(x) c(x, Inf)) # add Inf to each group

  ##########################################################################
  # To calculate  X<Y times M-function or Prob
  funL <- function(X, Y) {
    return(outer(X, Y, "<"))
  }


  # To calculate  X<Y times M-function or Prob
  funU <- function(X, Y) {
    return(outer(X, Y, "<="))
  }

  ########################################################
  # Create both sides of the inequality

  XLS <- expand.grid(dataL[S])
  XUS <- expand.grid(dataU[S])

  Max.XLS <- apply(XLS, 1, max)
  Max.XUS <- apply(XUS, 1, max)

  XLNS <- expand.grid(dataL[NS])
  XUNS <- expand.grid(dataU[NS])

  Max.XLNS <- apply(XLNS, 1, max)
  Max.XUNS <- apply(XUNS, 1, max)
  #############################################################
  # M-functions or Probabilities

  PP <- lapply(data, function(x) rep(1,length(x)+1)/(length(x)+1))

  PPS <- expand.grid(PP[S])
  prod.PPS <- apply(PPS, 1, prod)

  PPNS <- expand.grid(PP[NS])
  prod.PPNS <- apply(PPNS, 1, prod)

  #############################################################
  # Lower and Upper probabilities
  Lprob<-sum(prod.PPNS %*% funL(Max.XUNS, Max.XLS) %*% prod.PPS)
  Uprob<-sum(prod.PPNS %*% funU(Max.XLNS, Max.XUS) %*% prod.PPS)

  return(round(c(Lprob=Lprob,Uprob=Uprob),4))

}


