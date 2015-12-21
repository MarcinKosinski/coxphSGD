library(survival)
library(coxphSGD)
library(ggplot2)
library(magrittr)
library(dplyr)
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 10, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_10_e-5_100sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 10, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_10_e-6_100sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(20*sqrt(x))}, max.iter = 10, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_10_e-5_20sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(20*sqrt(x))}, max.iter = 10, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_10_e-6_20sqrt.pdf", width = 10, height = 10)
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


set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 10, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_10_e-5_50sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 10, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_10_e-6_50sqrt.pdf", width = 10, height = 10)
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




set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 5, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_5_e-5_100sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 5, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_5_e-6_100sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(20*sqrt(x))}, max.iter = 5, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_5_e-5_20sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(20*sqrt(x))}, max.iter = 5, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_5_e-6_20sqrt.pdf", width = 10, height = 10)
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


set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 5, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_5_e-5_50sqrt.pdf", width = 10, height = 10)
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
set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 5, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_5_e-6_50sqrt.pdf", width = 10, height = 10)
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


set.seed(4562)
simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 2, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_1_e-5_100sqrt.pdf", width = 10, height = 10)
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

set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(100*sqrt(x))}, max.iter = 1, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_1_e-6_100sqrt.pdf", width = 10, height = 10)
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

set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(20*sqrt(x))}, max.iter = 1, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_1_e-5_20sqrt.pdf", width = 10, height = 10)
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

set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(20*sqrt(x))}, max.iter = 1, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_1_e-6_20sqrt.pdf", width = 10, height = 10)
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


set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 1, epsilon = 1e-5) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_1_e-5_50sqrt.pdf", width = 10, height = 10)
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

set.seed(4561)
simulateCoxSGD(dCox, learningRates = function(x){1/(50*sqrt(x))}, max.iter = 1, epsilon = 1e-6) -> d2ggplot
d2ggplot -> backpack
d2ggplot <- d2ggplot$d2ggplot
beta_0 = c(0,0)
solution = c(1,3)

pdf(file = "b_0_0_iter_1_e-6_50sqrt.pdf", width = 10, height = 10)
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
