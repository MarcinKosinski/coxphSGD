#'
#' Cox Proportional Hazards Model Data Generation From Weibull Distribution
#'
#' Function \code{dataCox} generaters random survivaldata from Weibull
#' distribution (with parameters \code{lambda} and \code{rho} for given input
#' \code{x} data, model coefficients \code{beta} and censoring rate for censoring
#' that comes from exponential distribution with parameter \code{cens.rate}.
#'
#' @param n Number of observations to generate.
#' @param lambda lambda parameter for Weibull distribution.
#' @param rho rho parameter for Weibull distribution.
#' @param x A data.frame with an input data to generate the survival times for.
#' @param beta True model coefficients.
#' @param cens.rate Parameter for exponential distribution, which is
#' responsible for censoring.
#'
#' @details For each observation true survival time is generated and a censroing time. If censoring time is less then survival time, then the survival time
#' is returned and a status of observations is set to \code{0} which means the
#' observation had censored time. If the survival time is less than censoring
#' time, then for this observation the true survival time is returned and the
#' status of this observation is set to \code{1} which means that the event has
#' been noticed.
#'
#' @return A \code{data.frame} containing columns:
#' \itemize{
#' \item \code{id} an integer.
#' \item \code{time} survival times.
#' \item \code{status} observation status (event occured (1) or not (0)).
#' \item \code{x} a \code{data.frame} with an input data to generate the survival times for.
#' }
#'
#' @references
#' \url{http://onlinelibrary.wiley.com/doi/10.1002/sim.2059/abstract}
#'
#' \code{Generating survival times to simulate Cox proportional hazards models}, 2005 by Ralf Bender, Thomas Augustin, Maria Blettner.
#' @examples
#' \dontrun{
#' x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)
#' dataCox(10^4, lambda = 3, rho = 2, x,
#' beta = c(1,3), cens.rate = 5) -> dCox
#'}
#' @export
dataCox <- function(n, lambda, rho, x, beta, cens.rate){

  # real Weibull times
  u <- runif(n)
  Treal <- (- log(u) / (lambda * exp(x %*% beta)))^(1 / rho)

  # censoring times
  Censoring <- rexp(n, cens.rate)

  # follow-up times and event indicators
  time <- pmin(Treal, Censoring)
  status <- as.numeric(Treal <= Censoring)

  # data set
  data.frame(id = 1:n, time = time, status = status, x = x)
}
