# Chapter 8
##Easy.

#8E1. Which of the following is a requirement of the simple Metropolis algorithm?

1. The parameters must be discrete.

2. The likelihood function must be Gaussian.

3. The proposal distribution must be symmetric.

```
3
```

#8E2. Gibbs sampling is more efficient than the Metropolis algorithm. How does it achieve this extra efficiency? Are there any limitations to the Gibbs sampling strategy?

Gibbs sampling is more efficient because it uses adaptive proposals in which the distributions of a proposed paramter can adjust itself depending on the values that parameter may have in the moment. Gibbs does this using conjugate pairs, which are combinations of prior distributions and likelihoods, and have analytical solutions of the posterior distribution for a single paramter.

The limiations to Gibbs are that it requires the use of conjugate priors and as model complexity increases Gibbs becomes inefficient.



#8E3. Which sort of parameters can Hamiltonian Monte Carlo not handle? Can you explain why?


HMC requires continuous parameters and can not handle discrete parameters, this is because it works by running over the data continuously and spending more time on areas where it is more likely.


