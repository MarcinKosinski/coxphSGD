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
x <- runif(10000)
z <- 2 + 3*x
pr <- 1/(1+exp(-z))
y <- rbinom(10000,1,pr)


global_loglog <- function(beta1, beta2, xX, yY){
  sum(yY*(beta2+beta1*xX)-log(1+exp(beta2+beta1*xX)))
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
graphSGD <- function(beta, y, x, seed = 4561, outerBounds = calculate_outer(x,y),
                     eps = 10e-5){
  set.seed(seed)

  beta <- rev(beta)

  logitGD(y, x, optim.method = "GDI", beta_0 = beta,
          eps = eps, max.iter = 10000, alpha = function(t){1/(1000*sqrt(t))})$steps -> GDI.S

  logitGD(y, x, optim.method = "GDII", beta_0 = beta,
          eps = eps, max.iter = 5000)$steps -> GDII

  ind2 <- sample(length(y))
  logitGD(y[ind2], x[ind2], optim.method = "SGDI", beta_0 = beta,
          max.iter = 10000, eps = eps, alpha = function(t){1/sqrt(t)})$steps -> SGDI.1.S
  ind3 <- sample(length(y))
  logitGD(y[ind3], x[ind3], optim.method = "SGDI", beta_0 = beta,
          max.iter = 10000, eps = eps, alpha = function(t){5/sqrt(t)})$steps -> SGDI.5.S
  ind4 <- sample(length(y))
  logitGD(y[ind4], x[ind4], optim.method = "SGDI", beta_0 = beta,
          max.iter = 10000, eps = eps, alpha = function(t){6/sqrt(t)})$steps -> SGDI.6.S

  do.call(rbind, c(GDI.S, GDII, SGDI.1.S, SGDI.5.S, SGDI.6.S)) -> coeffs
  unlist(lapply(list(GDI.S, GDII, SGDI.1.S, SGDI.5.S, SGDI.6.S), length)) -> algorithm
  data2viz <- cbind(as.data.frame(coeffs),
                    algorithm = unlist(mapply(rep,
                                                c(paste("GDI", length(GDI.S), "steps"),
                                                paste("GDII", length(GDII), "steps"),
                                                paste("SGDI.1", length(SGDI.1.S), "steps"),
                                                paste("SGDI.5", length(SGDI.5.S), "steps"),
                                                paste("SGDI.6", length(SGDI.6.S), "steps")),
                                              algorithm)))
  names(data2viz)[1:2] <- c("Intercept", "X")
  data2viz$algorithm <- factor(data2viz$algorithm, levels = rev(levels(data2viz$algorithm)))
  beta[2] -> XX
  beta[1] -> YY


  ggplot()+
    stat_contour(aes(x=outerBounds$Var1,
                     y=outerBounds$Var2,
                     z=outerBounds$value+900),
                 alpha = 0.25, bins = 150) +
    geom_path(aes(x = data2viz$X,
                  y = data2viz$Intercept,
                  col = data2viz$algorithm,
                  group = data2viz$algorithm), size = 1) +
    geom_point(aes(as.vector(round(coefficients(glm(y~x, family = 'binomial')), 2)[2]),
                   as.vector(round(coefficients(glm(y~x, family = 'binomial')), 2)[1])),
               col = "black", size = 4, shape = 15) +
    geom_point(aes(x=XX, y=YY),
               col = "black", size = 4, shape = 17) +
    theme_bw(base_size = 20) +
    theme(panel.border = element_blank(),
          legend.key = element_blank()) +
    scale_colour_brewer(palette="Set1", name = 'Algorithm') +
    xlab('X') +
    ylab('Intercept')  -> pl_g

  return(pl_g)
}


pdf("contour_00.pdf", width = 10, height = 8)
pl_g
dev.off()

pdf("contour_2_1.pdf", width = 10, height = 8)
pl_g
dev.off()

pdf("contour_35_1.pdf", width = 10, height = 8)
pl_g
dev.off()


pdf("contour_4_32.pdf", width = 10, height = 8)
pl_g
dev.off()


pdf("sgd_00_1.pdf", width = 10, height = 8)
pl_g
dev.off()


pdf("sgd_00_2.pdf", width = 10, height = 8)
pl_g + coord_cartesian(ylim=c(0,5), xlim=c(0,4))
dev.off()


pdf("sgd_1_2_1.pdf", width = 10, height = 8)
pl_g + coord_cartesian(ylim=c(1,4), xlim=c(1,4))
dev.off()


pdf("sgd_1_2_2.pdf", width = 10, height = 8)
pl_g + coord_cartesian(ylim=c(1,4), xlim=c(1,4))
dev.off()


pdf("sgd_35_1_1.pdf", width = 10, height = 8)
pl_g + coord_cartesian(ylim=c(1,3), xlim=c(2.5,4.5))
dev.off()


pdf("sgd_35_1_2.pdf", width = 10, height = 8)
pl_g + coord_cartesian(ylim=c(1,3), xlim=c(2.5,4.5))
dev.off()


pdf("sgd_32_4_1.pdf", width = 10, height = 8)
pl_g  + coord_cartesian(ylim=c(0,5), xlim=c(0,4))
dev.off()


pdf("sgd_32_4_2.pdf", width = 10, height = 8)
pl_g + coord_cartesian(ylim=c(0,5), xlim=c(0,4))
dev.off()

