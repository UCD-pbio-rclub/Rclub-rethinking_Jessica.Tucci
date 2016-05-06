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
