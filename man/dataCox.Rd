% Please edit documentation in R/coxphSGD-GEN.R
\name{dataCox}
\alias{dataCox}
\title{Cox Proportional Hazards Model Data Generation From Weibull Distribution}
\usage{
dataCox(N, lambda, rho, x, beta, censRate)
}
\arguments{
\item{N}{Number of observations to generate.}

\item{lambda}{lambda parameter for Weibull distribution.}

\item{rho}{rho parameter for Weibull distribution.}

\item{x}{A data.frame with input data to generate the survival times for.}

\item{beta}{True model coefficients.}

\item{censRate}{Parameter for exponential distribution, which is
responsible for censoring.}
}
\description{
Function \code{dataCox} generaters random survivaldata from Weibull
distribution (with parameters \code{lambda} and \code{rho} for given input
\code{x} data, model coefficients \code{beta} and censoring rate for censoring
that comes from exponential distribution with parameter \code{censRate}.
}
\details{
For each observation true survival time is generated and a censroing time. If censoring time is less then survival time, then the survival time
is returned and a status of observations is set to \code{0} which means the
observation had censored time. If the survival time is less than censoring
time, then for this observation the true survival time is returned and the
status of this observation is set to \code{1} which means that the event has
been noticed.
}
\examples{
\dontrun{
x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)
dCox <- dataCox(10^4, lambda = 3, rho = 2, x, beta = c(1, 3), censRate = 5)
}
}
\references{
\url{http://onlinelibrary.wiley.com/doi/10.1002/sim.2059/abstract}

\code{Generating survival times to simulate Cox proportional hazards models}, 2005 by Ralf Bender, Thomas Augustin, Maria Blettner.
}
