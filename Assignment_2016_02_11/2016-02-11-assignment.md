# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name: Jessica Tucci

## 2E1
2

## 2E2
3

## 2E3
1 and 4

## 2E4
In the globe tossing example from the chapter saying "the probability of water is 0.7" tells us that
given all the information provided to our model this is the best prediction of reality, this prediction can also be updated when more information is provided.
The probability of water is based on a model and even if the model created is usefull there is still no guarantee that the model will match reality.
The globe has a certain amount of water on it and this is the objective reality.
The idea of a probability is that we are making a prediction on that state of reality.

## 2M3
Bayes Theorem states that Pr(Earth|Land)=Pr(Land|Earth)Pr(Earth)/Pr(Land)

Earth is 70% water so Pr(Land|Earth)=0.30

Mars is 100% Land so Pr(Land|Mars)=1

Earth and Mars are equally likely to be tossed so Pr(Earth)=0.5

Pr(Land)=Pr(Land|Earth)Pr(Earth)+ Pr(Land|Mars)Pr(Mars)=0.3(0.5)+1(0.5)=0.65

Therefore Pr(Earth|Land)=0.30(0.5)/0.65= 0.23

## 2M4
The number of ways to produce the data of a face up black card for each of the three cards:

    card 1(two black sides): 2 ways
    card 2(one black and one white side): 1 ways
    card 3(two white sides): 0 ways
    
Dividing the number of ways for card 1 by the total number of ways we get:

pr(card 1)=pr(other side is also black)=2 ways/3 total ways= 2/3

**JNM: Nice Job!**


## 2M1

1.

```
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior
#plot grid
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```
2.

```
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 3 , size=4 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior
#plot grid
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```
3.

```
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 5 , size=7 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior
#plot grid
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```

## 2M2

1.

```
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
# compute likelihood at each value in grid
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior
#plot grid
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```
2.

```
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
# compute likelihood at each value in grid
likelihood <- dbinom( 3 , size=4 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior
#plot grid
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```

3.

```
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
# compute likelihood at each value in grid
likelihood <- dbinom( 5 , size=7 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior
#plot grid
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```

## 2M5

The number of ways to produce the data of a face up black card for each of the three cards:

    card 1(B/B): 2 ways
    card 2(B/W): 1 ways
    card 3(W/W): 0 ways
    card 4(B/B): 2 ways
    
Dividing the number of ways to get card 1 or card 4 by the total number of ways we get:

pr(card 1 or card 4)=pr(other side is also black)=(2 ways + 2 ways)/5 total ways= 4/5

## 2M6

The number of ways to produce the data of a face up black card for each of the three cards multiplied by prior:

    card 1(B/B): 2 ways X 1 = 2 ways
    card 2(B/W): 1 ways X 2 = 2 ways
    card 3(W/W): 0 ways X 3 = 0 ways
    
Dividing the number of ways for card 1 by the total number of ways we get:

pr(card 1)=pr(other side is also black)=2 ways/4 total ways= 1/2 = 0.5

## 2M7

The number of ways to produce the data of a face up black card, then a face up white card for each of the three cards:

    card 1(B/B): 2 ways X 3 ways = 6 ways
    card 2(B/W): 1 ways X 2 ways = 2 ways
    card 3(W/W): 0 ways X 1 ways = 0 ways
    
Dividing the number of ways for card 1 by the total number of ways we get:

pr(card 1)=pr(other side is also black)=6 ways/8 total ways= 3/4 = 0.75


## 2H1

Since we have one set of twins, it is twice as likely that we have species B, therefore instead of pr(A)=Pr(B)=0.5 we can update the probabilities to pr(A)=(1/2)(1)/(3/2)=1/3 and pr(B)=(1/2)(2)/(3/2)=2/3

From this we can calculate the probability of a second set of twins given the first set of twins as:

Pr(twins|A)pr(A)+Pr(twins|B)Pr(B)= (0.1)(1/3)+(0.2)(2/3)=0.16

## 2H2

Bayes theroem states that pr(A|twins)=pr(twins|A)pr(A)/Pr(twins)

Pr(twins|A)=0.1

Pr(A)=0.5

Pr(twins)=Pr(twins|A)Pr(A)+Pr(twins|B)*Pr(B)=(0.1)(0.5)+(0.2)(0.5)=0.15

Therefore probability that we have species A given that we know the first birth was twins is:

Pr(A|twins)=(0.1)(0.5)/0.15=0.3333=1/3

## 2H3
Using the priors calculated from the data of one twin birth we have: Pr(A)=1/3 Pr(B)=2/3 therefore to get the posterior probability that we have species A we can calulate as follows:

Pr(A)=(likelihood A X Prior A)/average likelihood=

Pr(A|single birth)=Pr(single|A)Pr(A)/Pr(single)

Pr(Single|A)=1-0.1=0.9
Pr(A)=1/3
Pr(single)=Pr(single|A)Pr(A)+Pr(Single|B)Pr(B)=(0.9)(1/3)+(0.8)(2/3)=0.833

Pr(A|single birth)=(0.9)(1/3)/0.833=0.36

## 2H4
Using the original prior: Pr(A)=0.5:
```
Pr(A|positive A)=Pr(Positive A|A)Pr(A)/Pr(Positive A)

Probability that the test correctly identifies species A is Pr(Positive A|A)=0.8

Probability that the test either correctly identifies species A or gives a false positive for species A
when we acctually have species B:

Pr(Positive A|A)Pr(A)+Pr(Positive A|B)Pr(B)=(0.8)(0.5)+(1-0.65)(0.5)=0.575

Pr(A|positive A)=(0.8)(0.5)/(0.575)=0.69
```
Using the prior calculated given the data of twins and then the single birth: Pr(A)=0.36
```
Pr(A|positive A)=Pr(Positive A|A)Pr(A)/Pr(Positive A)

Probability that the test correctly identifies species A is Pr(Positive A|A)=0.8

Probability that the test either correctly identifies species A or gives a false positive for species A
when we acctually have species B:

Pr(Positive A|A)Pr(A)+Pr(Positive A|B)Pr(B)=(0.8)(0.36)+(1-0.65)(0.64)=0.512

Pr(A|positive A)=(0.8)(0.36)/(0.512)=0.5625
```


