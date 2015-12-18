coxphGD_step <- function(formula, data, learningRate, beta){
  # collect times, status, variables and reorder samples
  # to make the algorithm more clear to read and track
  preparedData <- prepareData(formula = formula, data = data)
  # calculate the log-likelihood for this batch sample
  partial_sum <- list()
  foreach(k = 1:nrow(preparedData)) %do% {
    # risk set for current time/observation
    risk_set <- preparedData %>% filter(times >= preparedData$times[k])

    nominator <- apply(risk_set[, -c(1,2)], MARGIN = 1, function(element){
      element * exp(element * beta)
    }) %>% rowSums()

    denominator <- apply(risk_set[, -c(1,2)], MARGIN = 1, function(element){
      exp(element * beta)
    }) %>% rowSums()

    partial_sum[[k]] <-
      preparedData[k, "event"] * (preparedData[k, -c(1,2)] - nominator/denominator)
  }
  do.call(rbind, partial_sum) %>%
    colSums() -> U_batch

  return(beta + learningRate * U_batch)
}

prepareData <- function(formula, data) {
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
        mf[, -1]) %>%
    arrange(times)
}




coxphGD <- function(formula, data, learningRates = function(x){1/x},
                     beta_0 = 0, epsilon = 1e-5, max.iter = 500 ) {
  checkArguments(formula, data, learningRates,
                 beta_0, epsilon) -> beta_start # check arguments
  n <- length(data)
  diff <- epsilon + 1
  i <- 1
  beta_new <- list()     # steps are saved in a list so that they can
  beta_old <- beta_start # be tracked in the future
  # estimate
  while(i <= max.iter & diff > epsilon) {
    beta_new[[i]] <- coxphGD_step(formula = formula, beta = beta_old,
                                    learningRate = learningRates(i),
                                    data = data) %>%
      unlist # unlist as this might be the result of foreach
    diff <- sqrt(sum((beta_new[[i]] - beta_old)^2))
    beta_old <- beta_new[[i]]
    i <- i + 1  ; cat("\r iteration: ", i, "\r")
  }
  # return results
  list(Call = match.call(), epsilon = epsilon, learningRates = learningRates,
       steps = i, coefficients = c(list(beta_start), beta_new))
}


checkArguments <- function(formula, data, learningRates,
                           beta_0, epsilon) {

  # + check names and types for every variables
  assert_that(is.function(learningRates))
  assert_that(is.numeric(epsilon))
  assert_that(is.numeric(beta_0))

  # check length of the start parameter
  if (length(beta_0) == 1) {
    beta_0 <- rep(beta_0, as.character(formula)[3] %>%
                    strsplit("\\+") %>%
                    unlist %>%
                    length)
  }

  return(beta_0)
}




set.seed(456)
dataCox <- function(N, lambda, rho, x, beta, censRate){

  # real Weibull times
  u <- runif(N)
  Treal <- (- log(u) / (lambda * exp(x %*% beta)))^(1 / rho)

  # censoring times
  Censoring <- rexp(N, censRate)

  # follow-up times and event indicators
  time <- pmin(Treal, Censoring)
  status <- as.numeric(Treal <= Censoring)

  # data set
  data.frame(id=1:N, time=time, status=status, x=x)
}

x <- matrix(sample(0:1, size = 2000, replace = TRUE), ncol = 2)

dataCox(10^3, lambda = 5, rho = 1.5, x, beta = c(2,2), censRate = 0.2) -> dCox


library(assertthat)
library(magrittr)
library(survival)
library(dplyr)
library(foreach)

coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/x},
        beta_0 = 0, epsilon = 1e-5, max.iter = 50 ) -> results


coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(1000*x)},
        beta_0 = 0, epsilon = 1e-5, max.iter = 50 ) -> results2


coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(1000*sqrt(x))},
        beta_0 = 0, epsilon = 1e-5, max.iter = 50 ) -> results3


coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(10000*x)},
        beta_0 = 0, epsilon = 1e-5, max.iter = 50 ) -> results4

coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(100*x)},
        beta_0 = 0, epsilon = 1e-5, max.iter = 50 ) -> results5

coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(100*sqrt(x))},
        beta_0 = 0, epsilon = 1e-5, max.iter = 50 ) -> results6


coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(100*sqrt(x))},
        beta_0 = 0, epsilon = 1e-4, max.iter = 50 ) -> results7

coxphGD(formula = Surv(time, status)~x.1+x.2,
        data = dCox, learningRates = function(x){1/(50*sqrt(x))},
        beta_0 = 0, epsilon = 1e-5, max.iter = 150 ) -> results8



library(ggplot2)

png('results1.png')
as.data.frame(t(simplify2array(results$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()

png('results2.png')
as.data.frame(t(simplify2array(results2$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()

png('results3.png')
as.data.frame(t(simplify2array(results3$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()


png('results4.png')
as.data.frame(t(simplify2array(results4$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()


png('results5.png')
as.data.frame(t(simplify2array(results5$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()


png('results6.png')
as.data.frame(t(simplify2array(results6$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()


png('results7.png')
as.data.frame(t(simplify2array(results7$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()


png('results8.png')
as.data.frame(t(simplify2array(results8$coefficients))) %>%
  ggplot(aes(V1, V2)) + geom_path() +
  coord_cartesian(xlim=c(0,3), ylim=c(0,3))
dev.off()
