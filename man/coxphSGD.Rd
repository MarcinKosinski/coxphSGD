% Please edit documentation in R/coxphSGD-GEN.R
\name{coxphSGD}
\alias{coxphSGD}
\title{Stochastic Gradient Descent log-likelihood estimation in
Cox proportional hazards model}
\usage{
coxphSGD(formula, data, reorderObs = TRUE, learningRates = function(x) 1/x, beta_0 = 0, 
    epsilon = 1e-05, batchSize = 10, epoch = 20)
}
\arguments{
\item{formula}{a formula object, with the response on the left of a ~ operator,
and the terms on the right. The response must be a survival object as returned by
the Surv function.}

\item{data}{a data.frame in which to interpret the variables named in the \code{formula}.}

\item{reorderObs}{a logical value telling whether reorder observations at each epoch.
when order of observations in estimation should be randomly generated.}

\item{learningRates}{a function specifing how to define learning rates in
steps of the algorithm. By default the \code{f(t)=1/t} is used, where \code{t} is
the number of algorithm's step.}

\item{beta_0}{a numeric vector (if of length 1 then will be replicated) of length
equal to the number of variables after using \code{formula} in the \code{model.matrix}
function}

\item{epsilon}{a numeric value with the stop condition of the estimation algorithm.}

\item{batchSize}{a numeric value specifing the size of a batch set to take from
the reordered dataset to update the coefficients in one step of an algorithm.}

\item{epoch}{a numeric value declaring the number of epoches to run for the
estimation algorithm in the stochastic gradient descent.}
}
\description{
Function \code{coxphSGD} estimates coefficients using stochastic
gradient descent algorithm in Cox proportional hazards model.
}
\note{
If one of the conditions is fullfiled
\itemize{
 \item \eqn{||\beta_{j+1}-\beta_{j}|| <}\code{epsilon} parameter for any \eqn{j}
 \item \eqn{#epochs >} \code{epochs} parameter
}
the estimation process is stopped.
}
\examples{
library(survival)
\dontrun{
coxphSGD(Surv(time, status) ~ ph.ecog + age, data = lung)
}
}
