logitGD <- function(y, x, optim.method = "GDI", eps = 10e-4,
                    max.iter = 100, alpha = function(t){1/t}, beta_0 = c(0,0)){
  stopifnot(length(y) == length(x) & optim.method %in% c("GDI", "GDII", "SGDI")
            & is.numeric(c(max.iter, eps, x)) & all(c(eps, max.iter) > 0) &
              is.function(alpha))
  iter <- 0
  err <- list()
  err[[iter+1]] <- eps+1
  w_old <- beta_0

  res <-list()
  while(iter < max.iter && (abs(err[[ifelse(iter==0,1,iter)]]) > eps)){

    iter <- iter + 1
    if (optim.method == "GDI"){
      w_new <- w_old + alpha(iter)*updateWeightsGDI(y, x, w_old)
    }
    if (optim.method == "GDII"){
      w_new <- w_old + as.vector(inverseHessianGDII(x, w_old)%*%
                                   updateWeightsGDI(y, x, w_old))
    }
    if (optim.method == "SGDI"){
      w_new <- w_old + alpha(iter)*updateWeightsSGDI(y[iter], x[iter], w_old)
    }
    res[[iter]] <- w_new
    err[[iter]] <- sqrt(sum((w_new - w_old)^2))

    w_old <- w_new

  }
  return(list(steps = c(list(beta_0),res), errors = c(list(c(0,0)),err)))
}

updateWeightsGDI <- function(y, x, w_old){
  #(1/length(y))*c(sum(y-p(w_old, x)), sum(x*(y-p(w_old, x))))
  c(sum(y-p(w_old, x)), sum(x*(y-p(w_old, x))))
}

updateWeightsSGDI <- function(y_i, x_i, w_old){
  c(y_i-p(w_old, x_i), x_i*(y_i-p(w_old, x_i)))
}

p <- function(w_old, x_i){
  1/(1+exp(-w_old[1]-w_old[2]*x_i))
}

inverseHessianGDII <- function(x, w_old){
  solve(
    matrix(c(
      sum(p(w_old, x)*(1-p(w_old, x))),
      sum(x*p(w_old, x)*(1-p(w_old, x))),
      sum(x*p(w_old, x)*(1-p(w_old, x))),
      sum(x*x*p(w_old, x)*(1-p(w_old, x)))
    ),
    nrow =2 )
  )
}
set.seed(1283)
x <- runif(1000)
z <- 2 + 3*x
pr <- 1/(1+exp(-z))
y <- rbinom(1000,1,pr)


global_loglog <- function(beta1, beta2, xX, yY){
  sum(yY*(beta1+beta2*xX)-log(1+exp(beta1+beta2*xX)))
}



calculate_outer <- function(x, y){
  ## contours
  outer_res <- outer(seq(0,4, length = 100),
                     seq(0,5, length = 100),
                     Vectorize( function(beta1,beta2){
                       global_loglog(beta1, beta2, xX = x, yY = y)
                     } )
  )

  outer_res_melted <- melt(outer_res)


  outer_res_melted$Var1 <- as.factor(outer_res_melted$Var1)
  levels(outer_res_melted$Var1) <- as.character(seq(0,4, length = 100))
  outer_res_melted$Var2 <- as.factor(outer_res_melted$Var2)
  levels(outer_res_melted$Var2) <- as.character(seq(0,5, length = 100))
  outer_res_melted$Var1 <- as.numeric(as.character(outer_res_melted$Var1))
  outer_res_melted$Var2 <- as.numeric(as.character(outer_res_melted$Var2))
  return(outer_res_melted)
}



library(ggplot2); library(ggthemes); library(reshape2)
graphSGD <- function(beta, y, x, seed = 4561, outerBounds = calculate_outer(x,y)){
  set.seed(seed)

  beta <- rev(beta)
logitGD(y, x, optim.method = "GDI",beta_0 = beta,
        eps = 10e-6, max.iter = 5000, alpha = function(t){1/(100*t)})$steps -> GDI
logitGD(y, x, optim.method = "GDII", beta_0 = beta,
        eps = 10e-6, max.iter = 5000)$steps -> GDII


ind <- sample(length(y))
logitGD(y[ind], x[ind], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-6, alpha = function(t){1/t})$steps -> SGDI.1
ind2 <- sample(length(y))
logitGD(y[ind2], x[ind2], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-6, alpha = function(t){2/t})$steps -> SGDI.2
ind3 <- sample(length(y))
logitGD(y[ind3], x[ind3], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-6, alpha = function(t){3/t})$steps -> SGDI.3
ind4 <- sample(length(y))
logitGD(y[ind4], x[ind4], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-6, alpha = function(t){4/t})$steps -> SGDI.4
ind5 <- sample(length(y))
logitGD(y[ind5], x[ind5], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-6, alpha = function(t){5/t})$steps -> SGDI.5
ind6 <- sample(length(y))
logitGD(y[ind6], x[ind6], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-6, alpha = function(t){6/t})$steps -> SGDI.6

do.call(rbind, c(GDI, GDII, SGDI.1, SGDI.2, SGDI.3, SGDI.4, SGDI.5, SGDI.6)) -> coeffs
unlist(lapply(list(GDI, GDII, SGDI.1, SGDI.2, SGDI.3, SGDI.4, SGDI.5, SGDI.6), length)) -> algorithm
data2viz <- cbind(as.data.frame(coeffs),
      algorithm = unlist(mapply(rep,
                                c(paste("GDI", length(GDI), "steps"),
                                       paste("GDII", length(GDII), "steps"),
                                       paste("SGDI.1", length(SGDI.1), "steps"),
                                       paste("SGDI.2", length(SGDI.2), "steps"),
                                       paste("SGDI.3", length(SGDI.3), "steps"),
                                       paste("SGDI.4", length(SGDI.4), "steps"),
                                       paste("SGDI.5", length(SGDI.5), "steps"),
                                       paste("SGDI.6", length(SGDI.6), "steps")),
                                algorithm)))
names(data2viz)[1:2] <- c("Intercept", "X")
beta[2] -> XX
beta[1] -> YY

#outer_res_melted <- outerBounds

# ggplot(data2viz) +
#   geom_point(aes(x = X, y = Intercept, col = algorithm)) +
#   geom_path(aes(x = X, y = Intercept, col = algorithm,
#                 group = algorithm)) +
#   theme_bw(base_size = 20) +
#   scale_colour_brewer(palette="Dark2") +
#   #theme_tufte(base_size = 20) +
#   #geom_point(aes(x=3, y =2), col = "black", size =4) +
#   geom_point(aes(as.vector(round(coefficients(glm(y~x, family = 'binomial')), 2)[2]),
#                  as.vector(round(coefficients(glm(y~x, family = 'binomial')), 2)[1])),
#              col = "black", size = 4, shape = 15) +
#   geom_point(x=XX, y=YY,
#              col = "black", size = 4, shape = 17) +
#   theme(panel.border = element_blank(),
#         legend.key = element_blank()) +

  ggplot()+
  stat_contour(aes(x=outerBounds$Var1,
                   y=outerBounds$Var2,
                   z=outerBounds$value+900),
               alpha = 0.25) +
  geom_point(aes(as.vector(round(coefficients(glm(y~x, family = 'binomial')), 2)[2]),
                 as.vector(round(coefficients(glm(y~x, family = 'binomial')), 2)[1])),
             col = "black", size = 4, shape = 15) +
  geom_point(aes(x=XX, y=YY),
             col = "black", size = 4, shape = 17) +
  theme_bw(base_size = 20) +
  theme(panel.border = element_blank(),
        legend.key = element_blank()) +
  geom_path(aes(x = data2viz$X,
                y = data2viz$Intercept,
                col = data2viz$algorithm,
                group = data2viz$algorithm), size =1) +
#   geom_point(aes(x = data2viz$X,
#                 y = data2viz$Intercept,
#                 col = data2viz$algorithm,
#                 group = data2viz$algorithm)) +
  scale_colour_brewer(palette="Dark2", name = 'Algorithm') +
    xlab('X') +
    ylab('Intercept')  -> pl_g

return(pl_g)
}


graphSGD(beta=c(1,0), y, x, 456, outerBounds = calculate_outer(x,y))

pdf(file = "sgd_00_1.pdf", width = 10, height = 8)
graphSGD(c(0,0), y, x, 4561)
dev.off()
pdf(file = "sgd_00_2.pdf", width = 10, height = 8)
graphSGD(c(0,0), y, x, 456)
dev.off()
pdf(file = "sgd_1_0_1.pdf", width = 10, height = 8)
graphSGD(c(1,0), y, x, 4561)
dev.off()
pdf(file = "sgd_1_0_2.pdf", width = 10, height = 8)
graphSGD(c(1,0), y, x, 456)
dev.off()


pdf(file = "sgd_21_31_1.pdf", width = 10, height = 8)
graphSGD(c(2.1,3.1), y, x, 4561)
dev.off()
pdf(file = "sgd_21_31_2.pdf", width = 10, height = 8)
graphSGD(c(2.1,3.1), y, x, 456)
dev.off()



pdf(file = "sgd_3_4_1.pdf", width = 10, height = 8 )
graphSGD(c(3.2,4), y, x, 4561)
dev.off()
pdf(file = "sgd_3_4_1.pdf", width = 10, height = 8 )
graphSGD(c(3.2,4), y, x, 456)
dev.off()



pdf(file = "sgd_2_1_1.pdf", width = 10, height = 8)
graphSGD(c(2,1), y, x, 4561)
dev.off()

pdf(file = "sgd_2_1_2.pdf", width = 10, height = 8)
graphSGD(c(2,1), y, x, 456)
dev.off()

