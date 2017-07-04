# # assuming observations are sorted by time
# loglikCox <- function(beta1, beta2, x1, x2, censored){
#   sum(rev(censored)*(beta1*rev(x1) + beta2*rev(x2) -
#                        log(cumsum(exp(beta1*rev(x1) + beta2*rev(x2))))))
# }


# calculate_outer_cox <- function(x1, x2, censored){
#   ## contours
#   outer_res <- outer(seq(-1,3, length = 100),
#            seq(0,4, length = 100),
#            Vectorize( function(beta1,beta2){
#              loglikCox(beta1, beta2, x1 = x1, x2 = x2, censored = censored)
#            } )
#   )
#   outer_res_melted <- melt(outer_res)
#   outer_res_melted$Var1 <- as.factor(outer_res_melted$Var1)
#   levels(outer_res_melted$Var1) <- as.character(seq(-1,3, length = 100))
#   outer_res_melted$Var2 <- as.factor(outer_res_melted$Var2)
#   levels(outer_res_melted$Var2) <- as.character(seq(0,4, length = 100))
#   outer_res_melted$Var1 <- as.numeric(as.character(outer_res_melted$Var1))
#   outer_res_melted$Var2 <- as.numeric(as.character(outer_res_melted$Var2))
#   return(outer_res_melted)
# }



