#' Stochastic Gradient Descent log-likelihood Estimation in
#' Cox Proportional Hazards Model
#'
#' \code{coxphSGD} estimates coefficients using stochastic
#' gradient descent algorithm in Cox proportional hazards model.
#'
#' @param formula a formula object, with the response on the left of a ~ operator,
#' and the terms on the right. The response must be a survival object as returned by
#' the Surv function.
#' @param data a list of batch data.frames in which to interpret the variables named in the \code{formula}.
#' See Details.
#' @param learn.rates a function specifing how to define learning rates in
#' steps of the algorithm. By default the \code{f(t)=1/t} is used, where \code{t} is
#' the number of algorithm's step.
#' @param beta.zero a numeric vector (if of length 1 then will be replicated) of length
#' equal to the number of variables after using \code{formula} in the \code{model.matrix}
#' function
#' @param epsilon a numeric value with the stop condition of the estimation algorithm.
#' @param max.iter numeric specifing maximal number of iterations.
#'
#' @details A \code{data} argument should be a list of data.frames, where in every batch data.frame
#' there is the same structure and naming convention for explanatory and survival (times, censoring)
#' variables. See Examples.
#'
#' @note If one of the conditions is fullfiled (j denotes the step number)
#' \itemize{
#'  \item \eqn{||\beta_{j+1}-\beta_{j}|| <}\code{epsilon} parameter for any \eqn{j}
#'  \item \eqn{j>max.iter}
#' }
#' the estimation process is stopped.
#' @importFrom survival Surv
#' @importFrom stats model.extract
#' @importFrom stats rexp
#' @importFrom stats runif
#' @author
#' Marcin Kosinski, \email{m.p.kosinski@@gmail.com}
#' @examples
#' library(survival)
#' \dontrun{
#' coxphSGD(Surv(time, status) ~ ph.ecog + age,
#'          data = split(lung, sample(1:4, size = 228, replace = TRUE))
#' )
#' }
#' @export
#' @rdname coxphSGD
coxphSGD <- function(formula, data, learn.rates = function(x){1/x},
                    beta.zero = 0, epsilon = 1e-5, max.iter = 500 ) {
  # check arguments
  beta_start <-
    coxphSGDcheck(
      formula,
      data,
      learn.rates,
      beta.zero,
      epsilon
    )
  n <- length(data)
  diff <- epsilon + 1
  i <- 1
  beta_new <- list()     # steps are saved in a list so that they can
  beta_old <- beta_start # be traced in the future
  # estimate
  while(i <= max.iter & diff > epsilon) {
    beta_new[[i]] <-
      unlist(
        coxphSGDbatch(
          formula = formula,
          beta = beta_old,
          learning.rate = learn.rates(i),
          data = data[[ifelse(i%%n==0, n, i%%n)]]
        )
      )

    diff <- sqrt(sum((beta_new[[i]] - beta_old)^2))
    beta_old <- beta_new[[i]]
    i <- i + 1
    cat("\r iteration: ", i, "\r")
  }
  # return results
  list(
    Call = match.call(),
    epsilon = epsilon,
    learn.rates = learn.rates,
    steps = i,
    coefficients = c(list(beta_start), beta_new)
  )
}

coxphSGDbatch <- function(formula, data, learning.rate, beta){

  # collect times, status, variables and reorder samples
  # to make the algorithm more clear to read and track
  batchData <- coxphSGDprepare(formula = formula, data = data) # sorts times lol
  # calculate the log-likelihood for this batch sample
  batchData <- batchData[order(-batchData$times), ] # dplyr::arrange(-times) / sorts time again but with different order
  # scores occure in nominator and denominator
  scores <- apply(batchData[, -c(1, 2)], 1,
                  function(element) exp(element %*% beta) )
  nominator <- apply(batchData[, -c(1, 2)], 2,
    						 function(element) cumsum(scores*element) )
  denominator <- cumsum(scores)
  # sum over non-censored observations
  partial_sum <- (batchData[, -c(1, 2)] - nominator/denominator)*batchData[, "event"]
  # each column indicates one explanatory variable
  U_batch <- colSums(partial_sum)
  return(beta + learning.rate * U_batch)
}

coxphSGDcheck <- function(formula, data, learn.rates,
                          beta.zero, epsilon) {

    stopifnot(is.list(data) & length(data) > 0)
    stopifnot(length(unique(unlist(lapply(data, ncol)))) == 1)
    # + check names and types for every variables
    stopifnot(is.function(learn.rates))
    stopifnot(is.numeric(epsilon))
    stopifnot(is.numeric(beta.zero))

      # check length of the start parameter
    if (length(beta.zero) == 1) {
      beta.zero <-
        rep(beta.zero,
            length(
              unlist(
                strsplit(
                  as.character(
                    formula
                    )[3],
                  split = "\\+")
                )
              )
        )

    }

    return(beta.zero)
}


coxphSGDprepare <- function(formula, data) {
  # Parameter identification as in  `survival::coxph()`.
  Call <- match.call()
  indx <- match(c("formula", "data"),
                names(Call), nomatch = 0)
  if (indx[1] == 0)
      stop("A formula argument is required")
  temp <- Call[c(1, indx)]
  temp[[1]] <- as.name("model.frame")

  mf <- eval(temp, parent.frame())
  Y <- model.extract(mf, "response")

  if (!inherits(Y, "Surv"))
      stop("Response must be a survival object")
  type <- attr(Y, "type")

  if (type != "right" && type != "counting")
      stop(paste("Cox model doesn't support \"", type, "\" survival data",
          sep = ""))

  # collect times, status, variables and reorder samples
  # to make the algorithm more clear to read and track
  cbind(event = unclass(Y)[,2], # 1 indicates event, 0 indicates cens
        times = unclass(Y)[,1],
        mf[, -1]) -> data2return
  data2return[order(data2return$times), ] # dplyr::arrange(times)
}


