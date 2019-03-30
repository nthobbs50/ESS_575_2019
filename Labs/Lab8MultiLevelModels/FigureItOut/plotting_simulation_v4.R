rm(list=ls())
library(rjags)
library(MCMCvis)
library(HDInterval)
set.seed(4)
#set some values to simulate data
mux1 = 2.3
sdx1 = 2
mux2 = 1.5
sdx2 =1
B1 = 1.2
B2 = 3.7
muB0 = .4
sdB0 = .1

n.x=10
n.groups = 15

###need different x's for different sites
x = seq(1,n.x)
x2 = sample(rnorm(10000, mux2,sdx2), n.x)
B0 = sample(rnorm(10000, muB0, sdB0), n.groups)
y = mu = x1 = x2 = matrix(nrow=n.x,ncol=n.groups)
sigma = 10
for(i in 1:n.x){
  for(j in 1:n.groups){
   x1[i,j] = x[i]
   x2[i,j] = rnorm(1,mux2,sdx2)
   mu[i,j] = B0[j] + B1*x1[i,j] + B2*x2[i,j]
   y[i,j] = rnorm(1,mu[i,j],sigma)

  }
}
par(mfrow=c(1,1))
matplot(x1,y)


data.list = list(
  x1=x1,
  x2=x2,
  y.n.groups = n.groups,
  y.n.x = n.x,
  x1.hat = x,  #time sequence
  y=y
)

inits = list(
 list(b1 = 1.1, b2 = 4, b0=rep(.75,n.groups),sigma=.5, mu.b0 = .5 ),
 list(b1 = 1.8, b2 = 3, b0=rep(.65,n.groups),sigma=.7, mu.b0 = .1 )
)
jm = jags.model("/Users/Tom/Documents/Conservation Science Partners/NPS_IM_project/nps-ima-fork/bin/plotting_simulationJAGSv4.R", data=data.list, n.chains = length(inits), inits=inits)
update(jm, n.iter=5000)
zm = coda.samples(jm, variable.names = c("mu.b0", "b0", "b1", "b2", "mu.hat.fixed",  "y.new.fixed", "mu.hat.all", "y.new.all", "sigma"), n.iter=5000)
MCMCsummary(zm)


matplot(x1,y, pch=19, cex=.5)

med = MCMCpstr(zm, params=c("mu.hat.fixed",  "y.new.fixed", "mu.hat.all", "y.new.all"), func = median)
HPDI <-  MCMCpstr(zm, params=c("mu.hat.fixed",  "y.new.fixed", "mu.hat.all", "y.new.all"), func = function(x) hdi(x,.95))

par(mfrow=c(2,2))
matplot(x1,y, pch=19, cex=.5, main = "Prediction of mean across specific sites")
lines(data.list$x1.hat,med$mu.hat.fixed, type="l", col = "red")
lines(data.list$x1.hat, HPDI$mu.hat.fixed[,1], lty="dashed", col = "blue") 
lines(data.list$x1.hat, HPDI$mu.hat.fixed[,2], lty="dashed", col = "blue")  


matplot(x1,y, pch=19, cex=.5, main = "Prediction of new observation across specific sites")
lines(data.list$x1.hat,med$y.new.fixed, type="l", col = "red")
lines(data.list$x1.hat, HPDI$y.new.fixed[,1], lty="dashed", col = "blue") 
lines(data.list$x1.hat, HPDI$y.new.fixed[,2], lty="dashed", col = "blue") 

matplot(x1,y, pch=19, cex=.5, main = "Prediction of mean across all sites")
lines(data.list$x1.hat,med$mu.hat.all, type="l", col = "red")
lines(data.list$x1.hat, HPDI$mu.hat.all[,1], lty="dashed", col = "blue") 
lines(data.list$x1.hat, HPDI$mu.hat.all[,2], lty="dashed", col = "blue") 

matplot(x1,y, pch=19, cex=.5, main = "Prediction of new observation across all sites")
lines(data.list$x1.hat,med$y.new.all, type="l", col = "red")
lines(data.list$x1.hat, HPDI$y.new.all[,1], lty="dashed", col = "blue") 
lines(data.list$x1.hat, HPDI$y.new.all[,2], lty="dashed", col = "blue") 
