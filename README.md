
<!-- README.md is generated from README.Rmd. Please edit that file -->
# cts

<!-- badges: start -->
<!-- badges: end -->
The goal of cts is to fit continuous time autoregressive models with the Kalman filter. See Wang (2013) <https://www.jstatsoft.org/article/view/v053i05>.

## Installation

You can install the development version of cts from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zhuwang46/cts")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cts)
#> 
#> Attaching package: 'cts'
#> The following objects are masked from 'package:stats':
#> 
#>     spectrum, tsdiag
## basic example code
data(V22174)
fit <- car(V22174,scale=0.2,order=7, ctrl=car_control(trace=FALSE))
summary(fit)
#> 
#> Call:
#> car(x = V22174, scale = 0.2, order = 7, ctrl = car_control(trace = FALSE))
#> 
#> Order of model = 7, sigma^2 = 1.37e-09 
#> 
#> Estimated coefficients (standard errors):
#>       phi_1 phi_2 phi_3  phi_4 phi_5  phi_6 phi_7
#> coef -0.501 0.355 0.085 -0.022 0.605 -0.371 0.483
#> S.E.  0.108 0.111 0.060  0.071 0.084  0.124 0.112
#> 
#> Estimated mean (standard error):
#> [1] 0.173
#> [1] 0.022
AIC(fit)
#> 
#> Call:
#> car(x = V22174, scale = 0.2, order = 7, ctrl = car_control(trace = FALSE))
#> 
#> Model selection statistics 
#> 
#>  order t.statistic     AIC
#>      1       -4.77  -20.78
#>      2       -4.45  -38.57
#>      3        3.25  -47.15
#>      4        2.37  -50.76
#>      5        6.11  -86.05
#>      6       -0.76  -84.63
#>      7        4.32 -101.27
factab(fit)
#> 
#> Call:
#> factab(object = fit)
#> 
#> Characteristic root of original parameterization in alpha 
#> 
#>              1               2               3               4               5  
#>  -0.006+0.058i   -0.006-0.058i   -0.029+0.300i   -0.029-0.300i   -0.030+0.135i
#>              6               7  
#>  -0.006+0.058i   -0.006-0.058i
#> 
#> Frequency 
#> 
#>     1      2      3      4      5      6      7  
#> 0.009  0.009  0.048  0.048  0.022  0.022  0.000
```
