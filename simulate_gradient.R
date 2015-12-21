
library(survival)
library(coxphSGD)
library(ggplot2)
library(magrittr)
library(dplyr)
library(survival)
set.seed(123)
dataCox2 <- function(N, lambda, rho, x, beta, censRate){

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

dataCox(10^4, lambda = 3, rho = 2, x, beta = c(1,3), censRate = 0.2) -> dCox


library(survival)
coxph_loglik <- function(beta, formula, data) {
  coxph(formula, init=beta, control=list('iter.max'=0), data =data)$loglik[2]
}


library(reshape2)
coxph_loglik <- function(beta, formula, data) {
  coxph(formula, init=beta, control=list('iter.max'=0), data =data)$loglik[2]
}
calculate_outer_cox_3 <- function(dCox){
  ## contours
  outer_res <- outer(seq(-1,3, length = 100),
                     seq(0,4, length = 100),
                     Vectorize( function(beta1,beta2){
                       coxph_loglik(beta=c(beta1,beta2), Surv(time, status)~x.1+x.2-1, dCox)
                     } )
  )
  outer_res_melted <- melt(outer_res)
  outer_res_melted$Var1 <- as.factor(outer_res_melted$Var1)
  levels(outer_res_melted$Var1) <- as.character(seq(-1,3, length = 100))
  outer_res_melted$Var2 <- as.factor(outer_res_melted$Var2)
  levels(outer_res_melted$Var2) <- as.character(seq(0,4, length = 100))
  outer_res_melted$Var1 <- as.numeric(as.character(outer_res_melted$Var1))
  outer_res_melted$Var2 <- as.numeric(as.character(outer_res_melted$Var2))
  return(outer_res_melted)
}
calculate_outer_cox_3(dCox) -> outerCox


simulateCoxSGD <- function(dCox = dCox, learningRates = function(x){1/x},
                           epsilon = 1e-03, beta_0 = c(0,0), max.iter = 100){

  # podziel na pozdzbiory
  sample(1:90, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted
  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = epsilon,
           learningRates = learningRates, beta_0 = beta_0, max.iter = max.iter*90) -> estimates



  sample(1:60, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted


  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = epsilon,
           learningRates = learningRates, beta_0 = beta_0, max.iter = max.iter*60) -> estimates2



  sample(1:120, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = epsilon,
           learningRates = learningRates, beta_0 = beta_0, max.iter = max.iter*120) -> estimates3


  sample(1:200, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = epsilon,
           learningRates = learningRates, beta_0 = beta_0, max.iter = max.iter*200) -> estimates4


  sample(1:30, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = epsilon,
           learningRates = learningRates, beta_0 = beta_0, max.iter = max.iter*30) -> estimates5


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

#
#
# set.seed(4561)
# simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 1, epsilon = 1e-6) -> d2ggplot
# d2ggplot -> backpack
# d2ggplot <- d2ggplot$d2ggplot
# beta_0 = c(0,0)
# solution = c(1,3)
#
# pdf(file = "b_0_0_iter_1_e-6_50sqrt.pdf", width = 10, height = 10)
# ggplot() +
#   stat_contour(aes(x=outerCox$Var1,
#                    y=outerCox$Var2,
#                    z=outerCox$value),
#                bins = 40, alpha = 0.25) +
#   geom_path(aes(d2ggplot$V1, d2ggplot$V2, group = d2ggplot$version,
#                 colour = d2ggplot$version), size = 1) +
#   theme_bw(base_size = 20) +
#   theme(panel.border = element_blank(),
#         legend.key = element_blank(), legend.position = "top") +
#   scale_colour_brewer(palette="Dark2", name = 'Algorithm \n & Steps') +
#   geom_point(aes(x = beta_0[1], y = beta_0[2]), col = "black", size = 4, shape = 17) +
#   geom_point(aes(x = solution[1], y = solution[2]), col = "black", size = 4, shape = 15) +
#   geom_point(aes(x = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[1,1],
#                  y = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[2,1]),
#              col = "black", size = 4, shape = 13) +
#   xlab("X1") + ylab("X2") +
#   guides(col = guide_legend(ncol = 3))
# dev.off()



############################# (pmax, pmin)

set.seed(587)
simulateCoxSGD(dCox, learningRates = function(x){max(0.01,1/(50*sqrt(x)))}, max.iter = 1, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)
pdf(file = "b_0_0_iter_1_e-6_50sqrt_pmin.pdf", width = 10, height = 10)
ggplot() +
  stat_contour(aes(x=outerCox$Var1,
                   y=outerCox$Var2,
                   z=outerCox$value),
               bins = 40, alpha = 0.25) +
  geom_path(aes(d2ggplot$V1, d2ggplot$V2, group = d2ggplot$version,
                colour = d2ggplot$version), size = 1) +
  theme_bw(base_size = 20) +
  theme(panel.border = element_blank(),
        legend.key = element_blank(), legend.position = "top") +
  scale_colour_brewer(palette="Dark2", name = 'Algorithm \n & Steps') +
  geom_point(aes(x = beta_0[1], y = beta_0[2]), col = "black", size = 4, shape = 17) +
  geom_point(aes(x = solution[1], y = solution[2]), col = "black", size = 4, shape = 15) +
  geom_point(aes(x = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[1,1],
                 y = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[2,1]),
             col = "black", size = 4, shape = 13) +
  xlab("X1") + ylab("X2") +
  guides(col = guide_legend(ncol = 3))
dev.off()


## iter2


set.seed(99)
simulateCoxSGD(dCox, learningRates = function(x){min(0.01,1/(50*sqrt(x)))}, max.iter = 2, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)
pdf(file = "b_0_0_iter_2_e-6_50sqrt_pmin.pdf", width = 10, height = 10)
ggplot() +
  stat_contour(aes(x=outerCox$Var1,
                   y=outerCox$Var2,
                   z=outerCox$value),
               bins = 40, alpha = 0.25) +
  geom_path(aes(d2ggplot$V1, d2ggplot$V2, group = d2ggplot$version,
                colour = d2ggplot$version), size = 1) +
  theme_bw(base_size = 20) +
  theme(panel.border = element_blank(),
        legend.key = element_blank(), legend.position = "top") +
  scale_colour_brewer(palette="Dark2", name = 'Algorithm \n & Steps') +
  geom_point(aes(x = beta_0[1], y = beta_0[2]), col = "black", size = 4, shape = 17) +
  geom_point(aes(x = solution[1], y = solution[2]), col = "black", size = 4, shape = 15) +
  geom_point(aes(x = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[1,1],
                 y = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[2,1]),
             col = "black", size = 4, shape = 13) +
  xlab("X1") + ylab("X2") +
  guides(col = guide_legend(ncol = 3))
dev.off()


# iter 3

set.seed(14)
simulateCoxSGD(dCox, learningRates = function(x){min(0.01,1/(50*sqrt(x)))}, max.iter = 3, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)
pdf(file = "b_0_0_iter_3_e-6_50sqrt_pmin.pdf", width = 10, height = 10)
ggplot() +
  stat_contour(aes(x=outerCox$Var1,
                   y=outerCox$Var2,
                   z=outerCox$value),
               bins = 40, alpha = 0.25) +
  geom_path(aes(d2ggplot$V1, d2ggplot$V2, group = d2ggplot$version,
                colour = d2ggplot$version), size = 1) +
  theme_bw(base_size = 20) +
  theme(panel.border = element_blank(),
        legend.key = element_blank(), legend.position = "top") +
  scale_colour_brewer(palette="Dark2", name = 'Algorithm \n & Steps') +
  geom_point(aes(x = beta_0[1], y = beta_0[2]), col = "black", size = 4, shape = 17) +
  geom_point(aes(x = solution[1], y = solution[2]), col = "black", size = 4, shape = 15) +
  geom_point(aes(x = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[1,1],
                 y = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[2,1]),
             col = "black", size = 4, shape = 13) +
  xlab("X1") + ylab("X2") +
  guides(col = guide_legend(ncol = 3))
dev.off()


# iter 4 # different pmin
set.seed(666)
simulateCoxSGD(dCox, learningRates = function(x){min(0.01,1/(50*sqrt(x)))}, max.iter = 4, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)
pdf(file = "b_0_0_iter_4_e-6_50sqrt_001pmin.pdf", width = 10, height = 10)
ggplot() +
  stat_contour(aes(x=outerCox$Var1,
                   y=outerCox$Var2,
                   z=outerCox$value),
               bins = 40, alpha = 0.25) +
  geom_path(aes(d2ggplot$V1, d2ggplot$V2, group = d2ggplot$version,
                colour = d2ggplot$version), size = 1) +
  theme_bw(base_size = 20) +
  theme(panel.border = element_blank(),
        legend.key = element_blank(), legend.position = "top") +
  scale_colour_brewer(palette="Dark2", name = 'Algorithm \n & Steps') +
  geom_point(aes(x = beta_0[1], y = beta_0[2]), col = "black", size = 4, shape = 17) +
  geom_point(aes(x = solution[1], y = solution[2]), col = "black", size = 4, shape = 15) +
  geom_point(aes(x = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[1,1],
                 y = summary(coxph(Surv(time, status)~x.1+x.2, data = dCox))$coeff[2,1]),
             col = "black", size = 4, shape = 13) +
  xlab("X1") + ylab("X2") +
  guides(col = guide_legend(ncol = 3))
dev.off()
