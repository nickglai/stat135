# compare three methods of sampling
wh=rnorm(100000,63,2.5)
mh=rnorm(100000,69,3.5)

# find E(X^2))
ex1=2.5^2+63^2
ex2=3.5^2+69^2
exsq=(ex1+ex2)/2
sig2=exsq-66^2
sqrt(sig2)
sig2/20

threesamples=function(n=20){
srs=sample(c(wh,mh),n)
prop=c(sample(wh,n/2),sample(mh,n/2))
# get sample sizes for optimal allocation
nj=c(n/2*2.5/(2.5/2+3.5/2),n/2*3.5/(2.5/2+3.5/2))
opt=c(sample(wh,nj[1]),sample(mh,nj[2]))
return(list(srs,prop,opt))
}
X=replicate(1000,threesamples())

srs=X[1:1000*3-2]
prop=X[1:1000*3-1]
opt=X[1:1000*3]
ms=sapply(srs,mean)
mp=sapply(prop,mean)
optmean=function(x,n=20){
  nj=c(n/2*2.5/(2.5/2+3.5/2),n/2*3.5/(2.5/2+3.5/2))
  return(0.5*(mean(x[1:nj[1]])+mean(x[-(1:nj[1])])))
}
mo=sapply(opt,optmean)

boxplot(list(ms,mp,mo))
abline(h=mean(c(wh,mh)),col="blue") 

# empirical variances (theoretical done in lecture)
var(ms)
var(mp)
var(mo)




