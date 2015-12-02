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

x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)

dataCox(10^4, lambda = 3, rho = 2, x, beta = c(1,3), censRate = 5) -> dCox

library(dplyr)
dCox %>%
  dplyr::arrange(time) -> dCoxArr

# assuming observations are sorted by time
full_cox_loglik <- function(beta1, beta2, x1, x2, censored){
  sum(rev(censored)*(beta1*rev(x1) + beta2*rev(x2) -
                       log(cumsum(exp(beta1*rev(x1) + beta2*rev(x2))))))
}

library(reshape2)
calculate_outer_cox <- function(x1, x2, censored){
  ## contours
  outer_res <- outer(seq(-1,3, length = 100),
                     seq(0,4, length = 100),
                     Vectorize( function(beta1,beta2){
                       full_cox_loglik(beta1, beta2, x1 = x1, x2 = x2, censored = censored)
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


calculate_outer_cox(dCoxArr$x.1, dCoxArr$x.2, dCoxArr$status) -> outerCox


library(ggplot2)
ggplot()+
  #   stat_contour(aes(x=outerCox$Var1,
  #                    y=outerCox$Var2,
  #                    z=outerCox$value),
  #                binwidth = 2, size = 0.5, colour = "grey50") +
  stat_contour(aes(x=outerCox$Var1,
                   y=outerCox$Var2,
                   z=outerCox$value),
               bins = 40)
library(coxphSGD)
library(dplyr)

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


  sample(1:10, size = 10^4, replace = TRUE) -> group

  split(dCox, group) -> dCox_splitted

  coxphSGD(Surv(time, status)~x.1+x.2, data = dCox_splitted, epsilon = epsilon,
           learningRates = learningRates, beta_0 = beta_0, max.iter = max.iter*10) -> estimates6




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
  t(simplify2array(estimates6$coefficients)) %>%
    as.data.frame() -> df6

  df1 %>%
    mutate(version = paste("90 batches,", nrow(df1), " steps")) %>%
    bind_rows(df2 %>%
                mutate(version = paste("60 batches,", nrow(df2), " steps"))) %>%
    bind_rows(df3 %>%
                mutate(version = paste("120 batches,", nrow(df3), " steps"))) %>%
    bind_rows(df4 %>%
                mutate(version = paste("200 batches,", nrow(df4), " steps"))) %>%
    bind_rows(df5 %>%
                mutate(version = paste("30 batches,", nrow(df5), " steps"))) %>%
    bind_rows(df6 %>%
                mutate(version = paste("10 batches,", nrow(df6), " steps"))) -> d2ggplot

  return(list(d2ggplot = d2ggplot, est1 = estimates, est2 = estimates2,
              est3 = estimates3, est4 = estimates4, est5 = estimates5))

}



simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

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
  xlab("X1") + ylab("X2") +
  guides(col = guide_legend(ncol = 3))




vizCoxSGD(I2,  outerCox = outerCox) -> plot6

vizCoxSGD(dCox) -> plot1
vizCoxSGD(dCox, learningRates = function(x){1/sqrt(x)}) -> plot2
vizCoxSGD(dCox, beta_0 = c(2,2)) -> plot3
vizCoxSGD(dCox, learningRates = function(x){1/sqrt(x)}, beta_0 = c(2,2)) -> plot4
vizCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, beta_0 = c(2,2)) -> plot5
vizCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}) -> plot6
vizCoxSGD(dCox, learningRates = function(x){1/(40*sqrt(x))}, beta_0 = c(2,2)) -> plot7
vizCoxSGD(dCox, learningRates = function(x){1/(40*sqrt(x))}) -> plot8
vizCoxSGD(dCox, learningRates = function(x){1/(100*((x)^{2/3}))}, beta_0 = c(2,2)) -> plot9
vizCoxSGD(dCox, learningRates = function(x){1/(100*((x)^{2/3}))}) -> plot10
vizCoxSGD(dCox, learningRates = function(x){1/(40*((x)^{2/3}))}, beta_0 = c(2,2)) -> plot11
vizCoxSGD(dCox, learningRates = function(x){1/(40*((x)^{2/3}))}) -> plot12



vizCoxSGD(dCox, epsilon = 1e-4) -> plot13

vizCoxSGD(dCox, beta_0 = c(2,2), epsilon = 1e-4) -> plot15
vizCoxSGD(dCox, learningRates = function(x){1/sqrt(x)}, beta_0 = c(2,2), epsilon = 1e-4) -> plot16
vizCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, beta_0 = c(2,2), epsilon = 1e-4) -> plot17
vizCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, epsilon = 1e-4) -> plot18
vizCoxSGD(dCox, learningRates = function(x){1/(40*sqrt(x))}, beta_0 = c(2,2), epsilon = 1e-4) -> plot19
vizCoxSGD(dCox, learningRates = function(x){1/(40*sqrt(x))}, epsilon = 1e-4) -> plot20
vizCoxSGD(dCox, learningRates = function(x){1/(100*((x)^{2/3}))}, beta_0 = c(2,2), epsilon = 1e-4) -> plot21
vizCoxSGD(dCox, learningRates = function(x){1/(100*((x)^{2/3}))}, epsilon = 1e-4) -> plot22
vizCoxSGD(dCox, learningRates = function(x){1/(40*((x)^{2/3}))}, beta_0 = c(2,2), epsilon = 1e-4) -> plot23
vizCoxSGD(dCox, learningRates = function(x){1/(40*((x)^{2/3}))}, epsilon = 1e-4) -> plot24
vizCoxSGD(dCox, learningRates = function(x){1/sqrt(x)}, epsilon = 1e-4) -> plot24

pdf(file = "plot1_cox.pdf", width = 10, height =8)
plot(plot1$plot)
dev.off()
pdf(file = "plot2_cox.pdf", width = 10, height =8)
plot(plot2$plot)
dev.off()
pdf(file = "plot3_cox.pdf", width = 10, height =8)
plot(plot3$plot)
dev.off()
pdf(file = "plot4_cox.pdf", width = 10, height =8)
plot(plot4$plot)
dev.off()
pdf(file = "plot5_cox.pdf", width = 10, height =8)
plot(plot5$plot)
dev.off()
pdf(file = "plot6_cox.pdf", width = 10, height =8)
plot(plot6$plot)
dev.off()
pdf(file = "plot7_cox.pdf", width = 10, height =8)
plot(plot7$plot)
dev.off()
pdf(file = "plot8_cox.pdf", width = 10, height =8)
plot(plot8$plot)
dev.off()
pdf(file = "plot9_cox.pdf", width = 10, height =8)
plot(plot9$plot)
dev.off()
pdf(file = "plot10_cox.pdf", width = 10, height =8)
plot(plot10$plot)
dev.off()
pdf(file = "plot11_cox.pdf", width = 10, height =8)
plot(plot11$plot)
dev.off()
pdf(file = "plot12_cox.pdf", width = 10, height =8)
plot(plot12$plot)
dev.off()
pdf(file = "plot13_cox.pdf", width = 10, height =8)
plot(plot13$plot)
dev.off()
pdf(file = "plot14_cox.pdf", width = 10, height =8)
plot(plot14$plot)
dev.off()
pdf(file = "plot15_cox.pdf", width = 10, height =8)
plot(plot15$plot)
dev.off()
pdf(file = "plot16_cox.pdf", width = 10, height =8)
plot(plot16$plot)
dev.off()
pdf(file = "plot17_cox.pdf", width = 10, height =8)
plot(plot17$plot)
dev.off()
pdf(file = "plot18_cox.pdf", width = 10, height =8)
plot(plot18$plot)
dev.off()
pdf(file = "plot19_cox.pdf", width = 10, height =8)
plot(plot19$plot)
dev.off()
pdf(file = "plot20_cox.pdf", width = 10, height =8)
plot(plot20$plot)
dev.off()
pdf(file = "plot21_cox.pdf", width = 10, height =8)
plot(plot21$plot)
dev.off()
pdf(file = "plot22_cox.pdf", width = 10, height =8)
plot(plot22$plot)
dev.off()
pdf(file = "plot23_cox.pdf", width = 10, height =8)
plot(plot23$plot)
dev.off()
pdf(file = "plot24_cox.pdf", width = 10, height =8)
plot(plot24$plot)
dev.off()
