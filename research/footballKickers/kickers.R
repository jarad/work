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

N <- length(y)               # number of kicks
K <- length(unique(kicker))  # number of kickers

data <- list(N=N,K=K,y=y,kicker=kicker,dist=dist,p=p,
             mu.prec=1e-6,R=diag(p))

### Create Initial Values

inits <- list(list(b.raw=matrix(0,K,p), mu.raw=rep(0,p), 
                   Tau.raw=diag(p), xi=rep(1,p)))
parameters <- c("b","mu","sigma")

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

