# mekala.github.io
My goal is to showcase preliminary statistical pipelines I am working on. 
Here are some example codes I am developing:
1. Bayesian Hierarhical Pipeline

Start by installing and loading required packages. We will set seed to replicate our results in this example.
```yml
library(R2jags)
library(jags)
set.seed(42)
```
Next, lets cook up some probability data.

```yml
samplesize<-100
p_host<-sort(runif(samplesize,0,1))
p_report<-sort(runif(samplesize,0,1))

popd<-rnorm(samplesize,mean=1000,sd=400)
lu_forest<-rnorm(samplesize,mean=50,sd=20)
z<-1+0.0005*popd+0.003*lu_forest
p_hostexp<-1/(1+exp(-z))

outp<-p_host*p_report*p_hostexp
outbreak<-rbinom(100,size=1,prob=outp)
```

Now lets specify this cooked up model in JAGS to estimate a latent probability.
```yml
mod_jags <- function(){
 	# Likelihood:
 	for (i in 1:N){
 		outbreak[i] ~ dbern(psi[i]) 
 		psi[i] <- p_host[i]* p_report[i]*p_hostexp[i]
 		p_hostexp[i]<-1/(1.000001+exp(-(alpha+beta1*popd[i]+beta2*lu_forest[i])))
 	}
 	# Priors:
 	alpha ~ dnorm(0, 0.01) # intercept
 	beta1 ~ dnorm(0, 0.01) # slope
 	beta2 ~ dnorm(0, 0.01) # slope
 
 }

init_values <- function(){
 	list(alpha = 1, beta1 = 0.0005, beta2 = 0.003)
}

params <- c("alpha", "beta1", "beta2") 
```

Finally, we can fit our JAGS model to estimate coefficients with the latent probability.
```yml
fit_lm1 <- jags(data = jagsdata_s1, inits = init_values, parameters.to.save = params, model.file = mod_jags,
			   n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = F)
			   
traceplot(fit_lm1, mfrow = c(2, 2), ask = F)
```


