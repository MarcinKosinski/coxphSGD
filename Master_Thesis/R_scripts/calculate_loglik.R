library(survival)
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

dataCox(10^4, lambda = 3, rho = 2, x, beta = c(1,3), censRate = 0.2) -> dCox


library(survival)
coxph_loglik <- function(beta, formula, data) {
  coxph(formula, init=beta, control=list('iter.max'=0), data =data)$loglik[2]
}

# betas  <- data.frame(beta1 = seq(0, 4, by=0.01),
#                      beta2 = seq(0, 4, by=0.01))
#
# logliks <- apply(betas, MARGIN = 1,coxph_loglik, Surv(time, status)~x.1+x.2, dCox)
#
# plot(betas[,1], logliks)


# contours

coxph_loglik(beta=c(beta1,beta2), Surv(time, status)~x.1+x.2, dCox)


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

# calculate_outer_cox_4minus1 <- function(dCox){
#   ## contours
#   outer_res <- outer(seq(1,3, length = 100),
#                      seq(1,3, length = 100),
#                      Vectorize( function(beta1,beta2){
#                        coxph_loglik(beta=c(beta1,beta2), Surv(time, status)~x.1+x.2, dCox)
#                      } )
#   )
#   outer_res_melted <- melt(outer_res)
#   outer_res_melted$Var1 <- as.factor(outer_res_melted$Var1)
#   levels(outer_res_melted$Var1) <- as.character(seq(1,3, length = 100))
#   outer_res_melted$Var2 <- as.factor(outer_res_melted$Var2)
#   levels(outer_res_melted$Var2) <- as.character(seq(0,4, length = 100))
#   outer_res_melted$Var1 <- as.numeric(as.character(outer_res_melted$Var1))
#   outer_res_melted$Var2 <- as.numeric(as.character(outer_res_melted$Var2))
#   return(outer_res_melted)
# }



# calculate_outer_cox_4minus1(dCox) -> ouTT2
#
# calculate_outer_cox <- function(x1, x2, censored){
#   ## contours
#   outer_res <- outer(seq(1,3, length = 100),
#                      seq(0,4, length = 100),
#                      Vectorize( function(beta1,beta2){
#                        full_cox_loglik(beta1, beta2, x1 = x1, x2 = x2, censored = censored)
#                      } )
#   )
#   outer_res_melted <- melt(outer_res)
#   outer_res_melted$Var1 <- as.factor(outer_res_melted$Var1)
#   levels(outer_res_melted$Var1) <- as.character(seq(1,3, length = 100))
#   outer_res_melted$Var2 <- as.factor(outer_res_melted$Var2)
#   levels(outer_res_melted$Var2) <- as.character(seq(0,4, length = 100))
#   outer_res_melted$Var1 <- as.numeric(as.character(outer_res_melted$Var1))
#   outer_res_melted$Var2 <- as.numeric(as.character(outer_res_melted$Var2))
#   return(outer_res_melted)
# }
#
# library(dplyr)
# dCox %>%
#   dplyr::arrange(time) -> dCoxArr
#
# full_cox_loglik <- function(beta1, beta2, x1, x2, censored){
#   sum(rev(censored)*(beta1*rev(x1) + beta2*rev(x2) -
#                        log(cumsum(exp(beta1*rev(x1) + beta2*rev(x2))))))
#   # #   sum(rev(censored)*(beta1*rev(x1) + beta2*rev(x2))) -
#   # #     sum(rev(censored)*log(cumsum(exp(beta1*rev(x1)+beta2*rev(x2)))))
#   #   n <- length(censored)
#   #   part <- numeric(n)
#   #   for(i in 1:n){
#   #     part[i] <- beta1*x1[i:n]+beta2*x2[i:n] - log(sum(exp(beta1*x1[i:n]+beta2*x2[i:n])))
#   #   }
#   #   sum(part)
# }
#
# calculate_outer_cox(dCoxArr$x.1, dCoxArr$x.2, dCoxArr$status) -> outerCox
#
#
#
# library(ggplot2)
# ggplot()+
#   #   stat_contour(aes(x=outerCox$Var1,
#   #                    y=outerCox$Var2,
#   #                    z=outerCox$value),
#   #                binwidth = 2, size = 0.5, colour = "grey50") +
#   #   stat_contour(aes(x=outerCox$Var1,
#   #                    y=outerCox$Var2,
#   #                    z=outerCox$value),
#   #                bins = 30, col = "blue") +
#   stat_contour(aes(x=ouTT$Var1,
#                    y=ouTT$Var2,
#                    z=ouTT$value),
#                bins = 30, col = "red") +
#   stat_contour(aes(x=ouTT2$Var1,
#                    y=ouTT2$Var2,
#                    z=ouTT2$value),
#                bins = 30, col = "blue", lty = "dashed")
