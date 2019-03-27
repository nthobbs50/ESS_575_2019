library(rjags)
rm(list=ls());n.group=5
beta=1.3;sigma=3;n=15
alphha=numeric(n.group)
alpha=rnorm(n.group,20,5)
x=sort(runif(n,-2,20))
y=matrix(0,nrow=500,ncol=6)
x=numeric(50)
count=1

#simulate some data
for(j in 1:n.group){
	n = round(runif(1,3,100),0)
	x=runif(n,-2,10)
	for(i in 1:n){
		y[count,1]=j
		y[count,2]=count
		y[count,3]=x[i]
		y[count,4]=rnorm(1,alpha[j]+beta*x[i],sigma)
		y[count,5]=alpha[j]
		y[count,6]=n
	count=count+1
	}
}
y=y[y[,2]>0,]


plot(y[,3],y[,4],pch=y[,1],col=y[,1], xlab="x", ylab="y")


colnames(y)=c("group", "i","x[i]","y[i]","alpha[j]", "n")




sink("var intercept 1")
cat(" 
model{
beta ~ dnorm(0,.000001)
tau ~ dgamma(.0001,.0001)
sigma <- 1/sqrt(tau)
mu.alpha ~ dnorm(0,.00001)
tau.alpha ~ dgamma(.001,.001)
 for (i in 1:length(y)){
	mu[i] <- alpha[group[i]]+ beta*x[i]
	y[i] ~ dnorm(mu[i],tau)
	}
	for(j in 1:n.group){
	alpha[j]~dnorm(mu.alpha,tau.alpha)
		}
	#individual regression predictions
		for(j in 1:n.group){
		for(i in 1:length(x.hat)){
		y.hat[i,j] <- alpha[j] + beta*x.hat[i]
		}
	}
	
}# end of model

",fill=TRUE)
sink()

x.hat=seq(-2,15,.5)
data=list(y=y[,4],x=y[,3],group=y[,1],n.group=n.group,x.hat=x.hat)

jm=jags.model("var intercept 1",data=data,n.chain=1, n.adapt=1000)
update(jm, n.iter=10000)
jz=jags.samples(jm,variable.names=c("alpha","beta","mu.alpha","sigma","mu","y.hat", "y.hat.all"),n.iter=25000)
j.coda = coda.samples(jm,variable.names=c("alpha","beta","sigma", "mu.alpha"),n.iter=25000)
q=summary(jz$y.hat, quantile, c(.025,.5,.975))$stat

x=y[,3]
plot(x,y[,4],pch=y[,1],col=y[,1], xlab="x", ylab="y",xlim=c(-5,12))
x=x.hat
lines(x,q[2,,1])
lines(x,q[3,,1],lty="dashed",col="black")
lines(x,q[1,,1],lty="dashed", col="black")

q=summary(jz$y.hat.all, quantile, c(.025,.5,.975))$stat

x=y[,3]
plot(x,y[,4],pch=y[,1],col=y[,1], xlab="x", ylab="y",xlim=c(-2,10))
x=x.hat
lines(x,q[2,])
lines(x,q[3,],lty="dashed",col="black")
lines(x,q[1,],lty="dashed", col="black")




