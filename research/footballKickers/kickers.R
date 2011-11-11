## Load the appropriate libraries
library(R2OpenBUGS)

## Create the data

n.per.kicker <- 25
K <- 4
p <- 2                       # Number of kicker specific parameters
b <- matrix(rnorm(K*p),K,p)
b[,p] <- 0
N <- n.per.kicker*K

dist <- runif(N,18,60)
kicker.by.name <- as.factor(rep(LETTERS[1:4], each=25))
kicker <- as.numeric(kicker.by.name)
y <- rbinom(N,1,1/(1+exp(-b[kicker,1]-b[kicker,2]*dist)))

data <- list(N=length(y),              # number of kicks
             K=length(unique(kicker)), # number of kickers
             y=y,                      # 1=kick good, 0=kick missed
             kicker=kicker,            # kicker ID (numeric), must not have gaps
             dist=dist,                # kick distance
             p=p,                      # number of covariates including intercept
             mu.prec=1e-6,             # overall mean precision
             R=diag(p))                # Wishart location

### Create Initial Values

inits <- list(list(b=b*0, mu=rep(0,2), Tau=diag(2)))
parameters <- c("b","mu","Tau")

## Run the simulation 

nburn=1e2
kicker.sim <- bugs(data,inits,parameters,"kickermodel.txt",
                   n.chains=1,n.burnin=nburn,n.thin=1,n.iter=nburn*2)
kicker.coda <- as.mcmc.list(kicker.sim)
#attach.all(kicker.sim$sims.list) 

## Gather results and statistics

summary(kicker.coda)
#raftery.diag(kicker.coda)
#effectiveSize(kicker.coda)
#autocorr.diag(kicker.coda) 

