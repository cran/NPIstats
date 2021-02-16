#' Breakdown times of units from two  groups
#'
#' We consider a data set used by Nelson (1982, p.462), which gives the breakdown times
#' of units from 6 different groups. In this data set, only the first two groups are used
#' to illustrate the NPI method for pairwise comparison with tails termination.
#' Both groups consist of 10 observations. The first unit of group X has a reported
#' breakdown time of 0.00, we interpret this as a very small but positive breakdown time.
#'
#'
#' @docType data
#'
#' @usage data(BreakdownTimes)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{group}{group X or Y}
#'  \item{times}{Breakdown times}
#' }
#' @references Nelson W. (1982). Applied Life Data Analysis. New York, Wiley, p.462.
#' @keywords datasets
#' @examples
#' data(BreakdownTimes)
#' head(BreakdownTimes)
#'
"BreakdownTimes"
