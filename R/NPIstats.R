#' NPIstats: Nonparametric Predictive Inference
#'
#' An implementation of the Nonparametric Predictive Inference approach in R.
#' It provides tools for quantifying uncertainty via lower and upper probabilities.
#' It includes useful functions for pairwise and multiple comparisons:
#' comparing two groups with and without terminated tails, selecting the best
#' group, selecting the subset of best groups, selecting the subset including
#' the best group.
#' @section Details:
#' Nonparametric Predictive Inference (NPI) is a statistical method which uses
#' few modelling assumptions, enabled by the use of lower and upper probabilities
#' to quantify uncertainty. NPI has been presented for many problems in Statistics,
#' Risk and Reliability and Operations Research. NPI approach is  based on Hill’s assumption
#' A(n), which gives a direct conditional probability for a future observable random quantity,
#' conditional on observed values of related random quantities. Inferences based on A(n) are predictive
#' and nonparametric, and can be considered suitable if there is hardly any knowledge about the random
#' quantity of interest, other than the n observations, or if one does not want to use such information,
#' e.g. to study effects of additional assumptions underlying other statistical methods.
#' A(n) is not sufficient to derive precise probabilities for many events of interest,
#' but it provides optimal bounds for probabilities for all events of interest involving the next future observation.
#' These bounds are lower and upper probabilities in the theories of imprecise probability
#' and interval probability, and as such they have strong consistency properties.
#' NPI is a framework of statistical theory and methods that use these A(n)-based
#' lower and upper probabilities, and also considers several variations of A(n) which are suitable for different inferences.
#' For more info, visit  \href{https://npi-statistics.com}{NPI webpage.}
#'
#' @section References:
#'
#' Augustin, T. and Coolen, F.P.A. (2004). Nonparametric predictive inference and interval probability. Journal of Statistical Planning and Inference 124, 251-272.
#'
#' Coolen, F.P.A. (1998). Low structure imprecise predictive inference for Bayes’ problem. Statistics & Probability Letters 36, 349-357.
#'

#' Coolen, F.P.A. and van der Laan, P. (2001). Imprecise predictive selection based on low structure assumptions. Journal of Statistical Planning and Inference 98, 259-277.
#'
#' Coolen, F.P.A. (1996). Comparing two populations based on low stochastic structure assumptions. Statistics & Probability Letters 29, 297-305.
#'
#' Hill, B.M. (1968). Posterior distribution of percentiles: Bayes’ theorem for sampling from a population. Journal of the American Statistical Association 63, 677-691.
#'
#' Weichselberger K. (2000). The theory of interval-probability as a unifying concept for uncertainty. International Journal of Approximate Reasoning, 24(2-3), 149–170.
#'
#' @docType package
#' @name NPIstats
NULL
#> NULL
