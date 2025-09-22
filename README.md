# mekala.github.io
My goal is to showcase preliminary statistical pipelines I am working on. 
Here are some example codes I am developing:
1. Bayesian Beta-Binomial pipeline (corrected for overdispersion) to explore outbreak amplification risk

Start by installing and loading required packages. Read outbreak dataset. 
```yml
library(brms)
data<-read.csv("Disease Outbreaks_FINAL.csv",header=TRUE)
```

Standardized covariates for variables with measurements outside 0 and 1 and so that Bayesian priors are not heard to select.
```yml
data$nightlightsc<-scale(data$nightlight,center=TRUE,scale=TRUE)
data$GDPsc<-scale(data$GDP,center=TRUE,scale=TRUE)
data$popsc<-scale(data$population,center=TRUE,scale=TRUE)
data$BIO01sc<-scale(data$BIO01,center=TRUE,scale=TRUE)
data$BIO12sc<-scale(data$BIO12,center=TRUE,scale=TRUE)
data$cellsc<-scale(data$cell,center=TRUE,scale=TRUE)
data$landlinesc<-scale(data$landline,center=TRUE,scale=TRUE)
```
Now setup the Bayesian Beta-binomial model (overdispersion parameter is phi just like a negative binomial distribution).
```yml
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"),
  lb = c(0, 0), ub = c(1, NA),
  type = "int", vars = "vint1[n]"
)


stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"

stanvars <- stanvar(scode = stan_funs, block = "functions")
```
Fit the brm model for real outbreak case data setting number of trials in the model to be total population size at location that can be infected.
```yml
fit2 <- brm(
  cases | vint(round(popREV)) ~ nightlightsc+GDPsc+popsc+BIO01sc+BIO12sc+cellsc+urban_percent+forest_percent+crop_percent+landlinesc+shdi, data = data,
  family = beta_binomial2, stanvars = stanvars
)
```

Now make predictions of expected case numbers with new population sizes and covariate data.
```yml
newdata_pred <- data.frame(popREV = c(90000,129000,123812))
newdata_pred<-as.data.frame(cbind(newdata_pred,data[1:3,c("crop_percent","forest_percent","urban_percent","GDPsc","landlinesc","nightlightsc","popsc","cellsc","BIO01sc","BIO12sc","shdi")]))
pp<-posterior_predict(fit2,newdata_pred)
colMeans(pp)
```

2. Bayesian Hierarchical Pipeline to model infectious disease exposure risk

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

Finally, we can fit our JAGS model to estimate coefficients for the latent probability.
```yml
fit_lm1 <- jags(data = jagsdata_s1, inits = init_values, parameters.to.save = params, model.file = mod_jags,
n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = F)			   
traceplot(fit_lm1, mfrow = c(2, 2), ask = F)
```


