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

x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)

dataCox(10^4, lambda = 3, rho = 2, x, beta = c(1,3), censRate = 5) -> dCox




library(coxphSGD)
library(ggplot2)
library(dplyr)

vizCoxSGD <- function(dCox = dCox){

  # podziel na pozdzbiory
  sample(1:90, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted
  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = 1e-03,
           learningRates = function(x){1/x}) -> estimates



  sample(1:60, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted


  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = 1e-03,
           learningRates = function(x){1/x}) -> estimates2



  sample(1:120, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = 1e-03,
           learningRates = function(x){1/x}) -> estimates3



  t(simplify2array(estimates$coefficients)) %>%
    as.data.frame() -> df1
  t(simplify2array(estimates2$coefficients)) %>%
    as.data.frame() -> df2
  t(simplify2array(estimates3$coefficients)) %>%
    as.data.frame() -> df3

  df1 %>%
    mutate(version = "90 batches") %>%
    bind_rows(df2 %>%
                mutate(version = "60 batches")) %>%
    bind_rows(df3 %>%
                mutate(version = "120 batches")) %>%
    ggplot(aes(x.1, x.2, group = version, colour = version)) +
    geom_path() +
    theme_bw(base_size = 20) +
    theme(panel.border = element_blank(),
          legend.key = element_blank()) +
    scale_colour_brewer(palette="Dark2", name = 'Algorithm') +
    coord_cartesian( ylim = c(-10,5)) -> plotX

  return(plotX)
}

vizCoxSGD(dCox) -> plot1
vizCoxSGD(dCox) -> plot2
vizCoxSGD(dCox) -> plot3
vizCoxSGD(dCox) -> plot4

pdf(file = "plot1_cox.pdf", width = 10, height =8)
plot(plot1)
dev.off()
pdf(file = "plot2_cox.pdf", width = 10, height =8)
plot(plot2)
dev.off()
pdf(file = "plot3_cox.pdf", width = 10, height =8)
plot(plot3)
dev.off()
pdf(file = "plot4_cox.pdf", width = 10, height =8)
plot(plot4)
dev.off()
