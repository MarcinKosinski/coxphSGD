coxphSGD
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/coxphSGD)](http://cran.r-project.org/web/packages/coxphSGD) [![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/coxphSGD?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/coxphSGD) [![Build Status](https://api.travis-ci.org/MarcinKosinski/coxphSGD.png)](https://travis-ci.org/MarcinKosinski/coxphSGD)

-   [Overview](#overview)
    -   [Applications](#applications)
    -   [Installation](#installation)
    -   [Assumptions](#assumptions)
-   [Simple example](#simple-example)
    -   [Simulated data](#simulated-data)
    -   [Fit the model](#fit-the-model)
    -   [Track estimates during each iteration on the contour lines plot](#track-estimates-during-each-iteration-on-the-contour-lines-plot)
    -   [Used code](#used-code)
-   [Complex example](#complex-example)
    -   [The Cancer Genome Atlas Data and RTCGA](#the-cancer-genome-atlas-and-rtcga)
    -   [Fit the complex model](#fit-the-complex-model)
    -   [Estimated survival curves](#estimated-survival-curves)
-   [Mathematical formulas](#mathematical-formulas)
-   [useR 2017](#user-2017)
    -   [Long abstract](#long-abstract)
    -   [Short abstract](#short-abstract)
    -   [Presentation](#presentation)

Overview
========

Applications
------------

Know the `survival::coxph()` function that calculates the estimates of the Cox Proportional Hazards model? <br> It uses the **gradient descent order II** method (known also as Newton-Raphson method) to optimize the **partial** log-likelihood function of the Cox PH model.

The `coxphSGD::coxphSGD()` is an equivalent that uses the **stochastic gradient descent order I** method in the optimization process, which can be beneficial in situations like

-   the data is in a **streaming structure** and appear in batches
-   the data is of the great volume (computations and RAM issues) and one can't simply use all data for computations and needs to go **block by block** of the data

> The stochastic gradient descent order I method can be used with already calculated estimates to generate updated ones in the situation in which new data appear and one can't recalculate the whole model with all historical data.

Installation
------------

``` r
install.packages('coxphSGD') # once it is on CRAN
devtools::install_github('MarcinKosinski/coxphSGD') # development version
```

Usage.

``` r
library(coxphSGD)
## help(package = 'coxphSGD') # manual reference
```

Assumptions
-----------

The Cox Proportional Hazards model assumes (mainly) that the explanatory variables are constant over time and that the **hazard** (one of the survival measures) estimates between considered groups are proportional over the time.

This model calculates the estimates that can be interpreted as proportion of the hazard change while moving from one level of the considered explanatory variable to another (while all other variables are fixed) - explained with details in the [Mathematical formulas](#mathematical-formulas) section.

Simple example
==============

Simulated data
--------------

Let's simulate artificial/fake data that follows the Cox Proportional Hazards model assumptions. Such data can be simulated from the Weibull distribution of the survival times (times during which the patient/item is under observation till **the event or censoring**) with some exponentially distributed conditions on the occurance of the censoring.

The approach was taken from the StackOverflow question [How to create a toy survival (time to event) data with right censoring](https://stats.stackexchange.com/questions/135124/how-to-create-a-toy-survival-time-to-event-data-with-right-censoring) and is based on [Generating survival times to simulate Cox proportional hazards models](http://onlinelibrary.wiley.com/doi/10.1002/sim.2059/abstract)

``` r
library(survival)
set.seed(456)
x <- matrix(sample(0:1, size = 20000, replace = TRUE), ncol = 2)
head(x)
```

         [,1] [,2]
    [1,]    0    0
    [2,]    0    1
    [3,]    1    0
    [4,]    1    0
    [5,]    1    1
    [6,]    0    0

``` r
dCox <- dataCox(10^4, lambda = 3, rho = 2, x,
                beta = c(2,2), cens.rate = 5)
head(dCox)
```

      id       time status x.1 x.2
    1  1 0.04408843      0   0   0
    2  2 0.04919427      0   0   1
    3  3 0.17103077      0   1   0
    4  4 0.07568921      1   1   0
    5  5 0.05604885      1   1   1
    6  6 0.07529861      1   0   0

Fit the model
-------------

One can fit the model with the `coxphSGD()` function. Below is the explanation of parameters:

-   `formula` - the `formula` object passed in the same way you would with the `coxph` function
-   `data` - the list of data.frame's (containing the same columns) corresponding to batches of data
-   `epsilon` - the convergence parameter - optimization will stop when the difference of the estimates in subsequent steps will be smaller than epsilon (euclidean distance)
-   `learn.rates` - the function to be used to determine the step length in subsequent steps of the optimization process
-   `beta.zero` - vector containing start points for the optimization process
-   `max.iter` - the maximal number of algorithm iterations, when the iterations number exceeds the batches number then the data are used again (one full data usage / all batches usage is called an epoch)

``` r
batch_id <- sample(1:90, size = 10^4, replace = TRUE)
dCox_split <- split(dCox, batch_id)
results <-
  coxphSGD(formula     = Surv(time, status) ~ x.1+x.2,
           data        = dCox_split,
           epsilon     = 1e-5,
           learn.rates = function(x){1/(100*sqrt(x))},
           beta.zero   = c(0,0),
           max.iter    = 10*90)
```

Track estimates during each iteration on the contour lines plot
---------------------------------------------------------------

One can extract the estimaes of the Cox PH model for each of the iteration with the following code.

``` r
coeff_by_iteration <-
  as.data.frame(
    do.call(
      rbind,
      results$coefficients
      )
    )
head(coeff_by_iteration)
```

            x.1       x.2
    1 0.0000000 0.0000000
    2 0.1287670 0.1505911
    3 0.2077041 0.2279745
    4 0.2815383 0.2849962
    5 0.3466851 0.3416297
    6 0.3864788 0.4102790

Then having the estimates in each iteration of the optimization process, one can create a plot of the contour lines of the partial log-likelihood calculated on the full data to compare the paths of optimization process with the usage of **stochastic gradient descent order I** method to get the final Cox PH model estimates.

Here with a black triangle the original `beta.zero` (set to iteration 0) are marked and with a red square the point that was estimated by SGD I algorithm and with blue crossed dot the estimates calculated by the GD II (Newton-Raphson algorithm).

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Used code

The code to reproduce the graph can be found in [this gist](https://gist.github.com/MarcinKosinski/909826b62f8849675f0980384fd6e28e).

Complex example
===============

The Cancer Genome Atlas and RTCGA
---------------------------------

[The Cancer Genome Atlas](https://cancergenome.nih.gov/) data included in the [RTCGA](https://github.com/RTCGA/) family of R packages

rtcga overview

Fit the complex model
---------------------

data presentation

fit model

Estimated surival curves
------------------------

model results

link to the code

Mathematical formulas
=====================

useR 2017
=========

Long abstract
-------------

[Link]()

Short abstract
--------------

[Link]()

Presentation
------------

[Link]()
