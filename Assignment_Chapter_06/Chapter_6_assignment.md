# Chapter 6 Problems
# Statistical Rethinking Chapter 6 problems

__Name:__ Jessica


##6E1
**State the three motivating criteria that define information entropy. Try to express each in your
own words.**

1. The measure of uncertainty should be continuous

2. The measure of uncertainty should increase as the number of possible events increases

3. The measure of uncertainty should be additive

##6E2
**Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads
70% of the time. What is the entropy of this coin?**


```r
p <- c( 0.3 , 0.7 )
-sum( p*log(p) )
```

```
## [1] 0.6108643
```

##6E3
**Suppose a four-sided die is loaded such that, when tossed onto a table, it shows "1" 20%, "2"
25%, "3" 25%, and "4" 30% of the time. What is the entropy of this die?**


```r
p <- c( 0.2 , 0.25, 0.25, .30 )
-sum( p*log(p) )
```

```
## [1] 1.376227
```



##6E4 
**Suppose another four-sided die is loaded such that it never shows "4". The other three sides
show equally often. What is the entropy of this die?**


```r
p <- c( 1/3, 1/3, 1/3 )
-sum( p*log(p) )
```

```
## [1] 1.098612
```


##6M1

**Write down and compare the definitions of AIC, DIC, and WAIC. Which of these criteria
is most general? Which assumptions are required to transform a more general criterion into a less
general one?**

AIC= Akaike information criterion AIC provides an estimate of the average out-of-sample deviance.

AIC = Dtrain + 2p

where p is the number of free parameters to be estimated in the model.

AIC provides an approximation of predictive accuracy, as measured by out-of-sample
deviance. All information criteria aim at this same target, but are derived under more and
less general assumptions.

Criterion is least general for AIC, and are as follows

(1) The priors are flat or overwhelmed by the likelihood.

(2) The posterior distribution is approximately multivariate Gaussian.

(3) The sample size N is much greater95 than the number of parameters k.

DIC=Deviance Information Criterion

slightly less general and allows for informative priors, but still assumes that the posterior is multivariate
Gaussian and that N ≫ k.

WAIC= Widely Applicable Information Criterion

WAIC is the most general of the three and makes no assumption about the shape of the posterior.

##6M5
**Provide an informal explanation of why informative priors reduce overfitting.**

Overfitting can occur when a model is overly influenced by the training sample. This is worse when the sample size is small and the priors are flat or close to flat. When this happens the posterior encodes as much of the training sample as possible and is not relatable for future predictions. If you have informative priors then the model will put more weight on values that are more plausable based on your informative priors, and be less likely to put too much weight onto values that might appear by chance in a small sample size (and probably are not very predictive of future values).


##6M6
**Provide an information explanation of why overly informative priors result in underfitting.**

Underfitting can occur when a prior is too skeptical, you can miss features of the data that may have good predictive value because your model is too skeptical to properly learn from the data it is being given. When you have informative priors that are too restrictive, the model will not allow for the natural variation that may exist in the data set and therefore again not give valuable predictions.

##6J1
**explore how the code in Code Block 6.16 works.  Explain what is happening in each line.**


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
#code is for Overthinking: WAIC calculations
data(cars)
m <- map(
alist(
dist ~ dnorm(mu,sigma),
mu <- a + b*speed,
a ~ dnorm(0,100),
b ~ dnorm(0,10),
sigma ~ dunif(0,30)
) , data=cars )
post <- extract.samples(m,n=1000)

#6.16
#log-likelihood of each observation i at each sample s from the posterior
n_samples <- 1000 #number of samples you extracted above
ll <- sapply( 1:n_samples , #runs the function below for s equal to each value 1 through 1000 and returns a matrix
function(s) {
mu <- post$a[s] + post$b[s]*cars$speed #this function takes collumn a + collumn b multiplied by the length 50 vector cars$speed for each row s
dnorm( cars$dist , mu , post$sigma[s] , log=TRUE ) #this part returns a value from the standard distribution based on each of the 50 different mean calculated above and the extracted sigma value from post and row s
} )
#You end up with a 50-by-1000 matrix of log-likelihoods, with observations in rows and samples in columns.
```


