% Please edit documentation in R/coxphSGD-GEN.R
\name{simulateCoxSGD}
\alias{simulateCoxSGD}
\title{Optimize Partial Log-Likelihood Function For Cox Propotional Hazards Model
Using Stochastic Gradient Descent}
\usage{
simulateCoxSGD(dCox = dCox, learningRates = function(x) {
    1/x
}, epsilon = 0.001, beta_0 = c(0, 0), max.iter = 100)
}
\arguments{
\item{dCox}{Input data.frame containing survival times (columnd should be named \code{time}) and status
(columnd should be named \code{status}). So far this function only supports 2 explanatory variable
(that should be named \code{x1} and \code{x2}.}

\item{learningRates}{Parameter passed to \link{coxphSGD}.}

\item{epsilon}{Parameter passed to \link{coxphSGD}.}

\item{beta_0}{Parameter passed to \link{coxphSGD}.}

\item{max.iter}{Parameter passed to \link{coxphSGD} to multiple the maximal iterations by the rows number.}
}
\description{
Function \code{simulateCoxSGD} splits input \code{dCox} data on 10, 30, 60, 90, 120 and 200
groups and for each split it uses \link{coxphSGD} function to generate estimates for Cox
Proportional Hazards Model with stochastic gradient descent optimization.
}
\examples{
\dontrun{
x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)
dCox <- dataCox(10^4, lambda = 3, rho = 2, x, beta = c(1, 3), censRate = 5)
d2ggplot <- simulateCoxSGD(dCox, learningRates = function(x) {
    1/(100 * sqrt(x))
}, max.iter = 10, epsilon = 1e-05)
}
}
