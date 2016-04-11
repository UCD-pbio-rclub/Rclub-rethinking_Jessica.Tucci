# Statistical Rethinking Chapter 4 problems

__Name:__ Jessica


# For 04/11/2016

## 5E1

2 and 4

## 5E2
D=diversity 
L=lattitude  
P=Plant Diversity

Di~Normal(μ_i,σ)

μi=α+βL Li+βP Pi

α~Normal(10,10)

βL~Normal(0,1)

βP~Normal(0,1)

σ~ Uniform(0,10)

## 5M1
Drinking whole fat milk is correlated with lower levels of obsesity. However, choice of low-fat milk over whole-fat milk is also correlated with those that are choosing to diet.

## 5M3
Those that get divorced proably also will remarry more often. Take into account how many times the person has been married.

## 5M4

```r
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Warning: package 'rstan' was built under R version 3.2.3
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```
## rstan (Version 2.9.0, packaged: 2016-01-05 16:17:47 UTC, GitRev: 05c3d0058b6a)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.58)
```

```r
data(WaffleDivorce)
setwd("C:/Users/Jessica/Documents/Rclub-rethinking_Jessica.Tucci/Assignment_Chapter_05")
lds=read.csv("lds_table.csv",header=F)
names(lds)=c("State","percent_lds")

lds_marriage=merge(WaffleDivorce,lds,by.x="Location",by.y="State")

#standardize variables
lds_marriage$Marriage.s <- (lds_marriage$Marriage - mean(lds_marriage$Marriage))/sd(lds_marriage$Marriage)

lds_marriage$MedianAgeMarriage.s <- (lds_marriage$MedianAgeMarriage - mean(lds_marriage$MedianAgeMarriage))/sd(lds_marriage$MedianAgeMarriage)

lds_marriage$percent_lds.s <- (lds_marriage$percent_lds - mean(lds_marriage$percent_lds))/sd(lds_marriage$percent_lds)

#create a model

model_lds <- map(
    alist(
      Divorce ~ dnorm(mu, sigma),
      mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s + bL*percent_lds.s,
      a ~ dnorm(10, 10),
      bR ~ dnorm(0, 1),
      bA ~ dnorm(0,1),
      bL ~ dnorm(0,1),
      sigma ~ dunif(0,10)
    ) ,
    data = lds_marriage)
precis(model_lds)
```

```
##        Mean StdDev  5.5% 94.5%
## a      9.69   0.19  9.38  9.99
## bR     0.06   0.27 -0.38  0.49
## bA    -1.27   0.27 -1.70 -0.85
## bL    -0.58   0.22 -0.93 -0.23
## sigma  1.34   0.13  1.13  1.56
```



