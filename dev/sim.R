
#'
#' Optimize Partial Log-Likelihood Function For Cox Propotional Hazards Model
#' Using Stochastic Gradient Descent
#'
#' Function \code{simulateCoxSGD} splits input \code{data} data on 10, 30, 60, 90, 120 and 200
#' groups and for each split it uses \link{coxphSGD} function to generate estimates for Cox
#' Proportional Hazards Model with stochastic gradient descent optimization.
#'
#' @param data Input data.frame containing survival times (columnd should be named \code{time}) and status
#' (columnd should be named \code{status}). So far this function only supports 2 explanatory variable
#' (that should be named \code{x1} and \code{x2}.
#' @param learn.rates Parameter passed to \link{coxphSGD}.
#' @param epsilon Parameter passed to \link{coxphSGD}.
#' @param beta.zero Parameter passed to \link{coxphSGD}.
#' @param max.iter Parameter passed to \link{coxphSGD} to multiple the maximal iterations by the rows number.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{
#' x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)
#' dataCox(10^4, lambda = 3, rho = 2, x,
#' beta = c(1,3), censRate = 5) -> data
#'  simulateCoxSGD(data, learn.rates = function(x){1/(100*sqrt(x))},
#'                 max.iter = 10, epsilon = 1e-5) -> d2ggplot
#'}
#' @export
simulateCoxSGD <- function(data = data, learn.rates = function(x){1/x},
                           epsilon = 1e-03, beta.zero = c(0,0), max.iter = 100){

  # podziel na pozdzbiory
  sample(1:90, size = 10^4, replace = TRUE) -> group

  split(data, group) -> data_splitted
  coxphSGD(Surv(time, status)~x.1+x.2, data = data_splitted, epsilon = epsilon,
           learn.rates = learn.rates, beta.zero = beta.zero, max.iter = max.iter*90) -> estimates



  sample(1:60, size = 10^4, replace = TRUE) -> group

  split(data, group) -> data_splitted


  coxphSGD(Surv(time, status)~x.1+x.2, data = data_splitted, epsilon = epsilon,
           learn.rates = learn.rates, beta.zero = beta.zero, max.iter = max.iter*60) -> estimates2



  sample(1:120, size = 10^4, replace = TRUE) -> group

  split(data, group) -> data_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = data_splitted, epsilon = epsilon,
           learn.rates = learn.rates, beta.zero = beta.zero, max.iter = max.iter*120) -> estimates3


  sample(1:200, size = 10^4, replace = TRUE) -> group

  split(data, group) -> data_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = data_splitted, epsilon = epsilon,
           learn.rates = learn.rates, beta.zero = beta.zero, max.iter = max.iter*200) -> estimates4


  sample(1:30, size = 10^4, replace = TRUE) -> group

  split(data, group) -> data_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = data_splitted, epsilon = epsilon,
           learn.rates = learn.rates, beta.zero = beta.zero, max.iter = max.iter*30) -> estimates5


  t(simplify2array(estimates$coefficients)) %>%
    as.data.frame() -> df1
  t(simplify2array(estimates2$coefficients)) %>%
    as.data.frame() -> df2
  t(simplify2array(estimates3$coefficients)) %>%
    as.data.frame() -> df3
  t(simplify2array(estimates4$coefficients)) %>%
    as.data.frame() -> df4
  t(simplify2array(estimates5$coefficients)) %>%
    as.data.frame() -> df5

  df1 %>%
    mutate(version = paste("90 batches,", nrow(df1), " steps")) %>%
    bind_rows(df2 %>%
                mutate(version = paste("60 batches,", nrow(df2), " steps"))) %>%
    bind_rows(df3 %>%
                mutate(version = paste("120 batches,", nrow(df3), " steps"))) %>%
    bind_rows(df4 %>%
                mutate(version = paste("200 batches,", nrow(df4), " steps"))) %>%
    bind_rows(df5 %>%
                mutate(version = paste("30 batches,", nrow(df5), " steps")))  -> d2ggplot

  return(list(d2ggplot = d2ggplot, est1 = estimates, est2 = estimates2,
              est3 = estimates3, est4 = estimates4, est5 = estimates5))

}
