#' Estymacja w modelu Coxa metoda stochastycznego spadku gradientu
#' 
#' Funkcja \code{coxphSGD} estymuje współczynniki w modelu proporcjonalnych
#' hazardów Coxa metodą stochastycznego spadku gradientu.
#' 
#' @param formula a formula object, with the response on the left of a ~ operator,
#' and the terms on the right. The response must be a survival object as returned by
#' the Surv function.
#' @param data a data.frame in which to interpret the variables named in the \code{formula}
#' @param order a numeric vector with suggested order of observations or \code{NULL}
#' when order of observations in estimation should be randomly generated
#' @param learningRates
#' @param epsilon a numeric value with the stop condition of the estimation algorithm. 
#' When \code{epoch} parameter is specified, \code{epsilon} is ignored.
#' @param epoch a numeric value declaring the number of epoches to run for the
#' estimation algorithm in the stochastic gradient descent. By defaul set to 
#' \code{NULL} - then the \code{epsilon} is used to access the convergence.
#'
#' @export
#' @importFrom survival Surv
#' @examples
#' library(survival)
#' coxphSGD(Surv(time, status) ~ ph.ecog + tt(age), data=lung)
#' 
coxphSGD = function(formula, data, order, learningRates, epsilon, epoch) {
  x + 1
}
