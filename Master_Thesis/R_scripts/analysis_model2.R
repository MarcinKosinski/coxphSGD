library(magrittr)
library(foreach)



partial_coxph_loglik <- function (dCox, beta, status_number) {
  n <- nrow(dCox)
  foreach(i=1:n) %dopar% {
    sum(dCox[i,status_number]*(dCox[i,-status_number]*beta))
  } %>% unlist -> part1


  foreach(i=1:n) %dopar% {
    exp(sum(dCox[i,-status_number]*beta))
  } %>% unlist -> part2


  foreach(i=1:n) %dopar% {
    part1[i] - dCox[i, status_number]*(log(sum(part2[i:n])))
  } %>% unlist -> part3

  sum(part3)
}

load("model_1_over_100sqrt_t.rda")
load("model_1_over_50sqrt_t.rda")
load("model_1_over_t.rda")
load("testCox_binded.rda")

do.call(rbind,model_1_over_50sqrt_t$coefficients) %>% as.data.frame -> model2

#do.call(rbind,testCox) -> testCox_binded

library(pbapply)
pbapply(model2, MARGIN = 1, function(x){
  partial_coxph_loglik(testCox_binded[,-1093], beta = x, status_number = 1092)
}) -> cum_log_lik_model2

save(cum_log_lik_model2, file = "cum_log_lik_model2.rda")
