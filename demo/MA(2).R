source("R/ABC.R")
########################################################
##data generation MA(2)
n = 100
q = 2
theta = c(0.6,0.2)

u = rnorm(n+q, 0, 1)  #shifts the indices by q
y = c()
for (k in 1:n) {
  y[k] = u[k+q] + t(theta) %*% u[(k-1+q):k]
}

##define prior and likelihood sampler

prior = function() {
  a = runif(2)
  x = c(-2*sqrt(a[1]) + 4*a[2]*sqrt(a[1]),2*sqrt(a[1])-1)
  return(x)
}

f = function(x) {
  z = c()
  u = rnorm(n+q, 0, 1)
  for (k in 1:n) {
    z[k] = u[k+q] + t(x) %*% u[(k-1+q):k]
  }
  return(z)
}

###evaluate in parallel
require(doParallel)
require(plyr)
require(doSNOW)
#registerDoSNOW(makeCluster(2, type="SOCK"))
registerDoParallel()

getDoParWorkers()
getDoParName()
getDoParVersion()
time<-system.time({
  theta_raw <- foreach(icount(100),.combine=rbind) %dopar% 
  ABC(1000,0.01,function(y,z) sqrt(sum((y-z)^2)) ,function(x) x,prior,f)   
  theta_auto <- foreach(icount(100),.combine=rbind) %dopar% 
  ABC(1000,0.01,function(y,z) sqrt(sum((y-z)^2)) ,function(x) c(x[q:n]%*%x[1:(n-q+1)],x[(q+1):n]%*%x[1:(n-q)]),prior,f)  
})


par(mfrow=c(1,2))
hist(theta_raw[,1])
hist(theta_raw[,2])

par(mfrow=c(1,2))
hist(theta_auto[,1])
hist(theta_auto[,2])

par(mfrow=c(1,2))
plot.new()
plot.window(xlim=c(-2,2),ylim=c(-1,1))
polygon(c(-2,0,2),c(1,-1,1),col="yellow")
axis(1)
axis(2)
points(theta_raw[,1],theta_auto[,2],col=2)
points(0.6,0.2,lwd=2)
title("sampled particles based on raw distance")

plot.new()
plot.window(xlim=c(-2,2),ylim=c(-1,1))
polygon(c(-2,0,2),c(1,-1,1),col="yellow")
axis(1)
axis(2)
points(theta_auto[,1],theta_auto[,2],col=2)
points(0.6,0.2,lwd=2)
title("sampled particles based on auto distance")

time






