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

x <- rnorm(1000)
z <- 2 + 3*x
pr <- 1/(1+exp(-z))
y <- rbinom(1000,1,pr)


library(ggplot2); library(ggthemes)
graphSGD <- function(beta, y, x){


  beta <- rev(beta)
logitGD(y, x, optim.method = "GDI",beta_0 = beta,
        eps = 10e-5, max.iter = 5000, alpha = function(t){1/(100*t)})$steps -> GDI
logitGD(y, x, optim.method = "GDII", beta_0 = beta,
        eps = 10e-5, max.iter = 5000)$steps -> GDII

ind <- sample(length(y))
logitGD(y[ind], x[ind], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-5, alpha = function(t){1/t})$steps -> SGDI.1
ind2 <- sample(length(y))
logitGD(y[ind2], x[ind2], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-5, alpha = function(t){2/t})$steps -> SGDI.2
ind3 <- sample(length(y))
logitGD(y[ind3], x[ind3], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-5, alpha = function(t){3/t})$steps -> SGDI.3
ind4 <- sample(length(y))
logitGD(y[ind4], x[ind4], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-5, alpha = function(t){4/t})$steps -> SGDI.4
ind5 <- sample(length(y))
logitGD(y[ind5], x[ind5], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-5, alpha = function(t){5/t})$steps -> SGDI.5
ind6 <- sample(length(y))
logitGD(y[ind6], x[ind6], optim.method = "SGDI", beta_0 = beta,
        max.iter = 1000, eps = 10e-5, alpha = function(t){6/t})$steps -> SGDI.6

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
ggplot(data2viz) +
  geom_point(aes(x = X, y = Intercept, col = algorithm)) +
  geom_path(aes(x = X, y = Intercept, col = algorithm,
                group = algorithm)) +
  theme_tufte(base_size = 20) +
  geom_point(aes(x=3, y =2), col = "black", size =4) -> p
return(p)
}


graphSGD(c(0,0), y, x)

pdf(file = "sgd_00_1.pdf", width = 10, height = 8)
graphSGD(c(0,0), y, x)
dev.off()


pdf(file = "sgd_2.1_3.1_1.pdf", width = 10, height = 8)
graphSGD(c(2.1,3.1), y, x)
dev.off()


pdf(file = "sgd_3_4_2.pdf", width = 10, height = 8 )
graphSGD(c(3,4), y, x)
dev.off()


pdf(file = "sgd_2_1_1.pdf", width = 10, height = 8)
graphSGD(c(2,1), y, x)
dev.off()
