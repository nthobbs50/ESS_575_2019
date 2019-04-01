model{
  mu.b0 ~ dnorm(0,.000001)
  sigma.b0 ~ dunif(0,10)
  tau.b0 <- 1/sigma.b0^2
  b1 ~ dnorm(0,.000001)
  b2 ~dnorm(0,.000001)
  sigma ~ dunif(0,5)
  tau <- 1/sigma^2
  
  for(j in 1:y.n.groups){
    b0[j] ~ dnorm(mu.b0,tau.b0)
  }
  #Note that i indexes time.
  for(i in 1:y.n.x){
    for(j in 1:y.n.groups){
      mu[i,j] <- b0[j] + b1*x1[i,j] + b2*x2[i,j]
      y[i,j] ~ dnorm(mu[i,j],tau)
    } #ne of j
    
  } #end of i
 
###predictions for in-sample sites
  for(j in 1:y.n.groups){
    group[j] <- 1
  }
  p <- group[]/sum(group)
for(i in 1:length(x1.hat)){
   j[i] ~ dcat(p[]) 
   #prediction of mean at site j[i]
   mu.hat.fixed[i] <- b0[j[i]] + b1*x1.hat[i] + b2*x2[i,j[i]]
   #prediction of new observation at site j[i]
   y.new.fixed[i] ~ dnorm(mu.hat.fixed[i], tau)
 } #end of i
  
   #prediction of new observation from all sites studied
  for(i in 1:length(x1.hat)){
    b0.new[i] ~ dnorm(mu.b0,tau.b0)
    x2.new[i] ~ dnorm(mean(x2[i,]), 1/sd(x2[i,])^2)
    #prediction of mean at site j
    mu.hat.all[i] <- b0.new[i] + b1*x1.hat[i] + b2*x2.new[i]
    #prediction of new observation at site j
    y.new.all[i] ~ dnorm(mu.hat.all[i], tau)
  } #end of i
  
} #end of model
