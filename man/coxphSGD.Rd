% Please edit documentation in R/coxphSGD-GEN.R
\name{coxphSGD}
\alias{coxphSGD}
\title{Stochastic Gradient Descent log-likelihood estimation in 
Cox proportional hazards model}
\usage{
coxphSGD(formula, data, learningRates = function(x) {
    1/x
}, beta_0 = 0, epsilon = 1e-05)
}
\arguments{
\item{formula}{a formula object, with the response on the left of a ~ operator,
and the terms on the right. The response must be a survival object as returned by
the Surv function.}

\item{data}{a list of batch data.frames in which to interpret the variables named in the \code{formula}.
See Details.}

\item{learningRates}{a function specifing how to define learning rates in 
steps of the algorithm. By default the \code{f(t)=1/t} is used, where \code{t} is
the number of algorithm's step.}

\item{beta_0}{a numeric vector (if of length 1 then will be replicated) of length 
equal to the number of variables after using \code{formula} in the \code{model.matrix}
function}

\item{epsilon}{a numeric value with the stop condition of the estimation algorithm.}
}
\description{
Function \code{coxphSGD} estimates coefficients using stochastic
gradient descent algorithm in Cox proportional hazards model.
}
\details{
A \code{data} argument should be a list of data.frames, where in every batch data.frame
there is the same structure and naming convention for explanatory and survival (times, censoring)
variables. See Examples.
}
\note{
If one of the conditions is fullfiled (j denotes the step number)
\itemize{
 \item \eqn{||\beta_{j+1}-\beta_{j}|| <}\code{epsilon} parameter for any \eqn{j}
 \item \eqn{j>} \code{\#batches} 
}
the estimation process is stopped.
}
\examples{
library(survival)
\dontrun{
coxphSGD(Surv(time, status) ~ ph.ecog + age, data = split(lung, sample(1:4, 
    size = 228, replace = TRUE)))
}

}
