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

data <- list(N=length(y),K=length(unique(kicker)),y=y,
             kicker=kicker,dist=dist,p=p,R=diag(p))

### Create Initial Values

inits <- list(list(b=b*0, mu=rep(0,2), Tau=diag(2)))
parameters <- c("b","mu","Tau","Sigma")

## Run the simulation 

nburn=1e4
kicker.sim <- bugs(data,inits,parameters,"kickermodel.txt",
                   n.chains=1,n.burnin=nburn,n.thin=1,n.iter=nburn*2)
kicker.coda <- as.mcmc.list(kicker.sim)
#attach.all(kicker.sim$sims.list) 

## Gather results and statistics

summary(kicker.coda)
#raftery.diag(kicker.coda)
#effectiveSize(kicker.coda)
#autocorr.diag(kicker.coda) 

