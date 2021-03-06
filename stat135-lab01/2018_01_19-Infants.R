# Need to change next line depending on where you put the file
load("/Users/hankibser/Dropbox/0Teaching/0S135/Code/KaiserBabies.rda")
plot(density(infants$bwt), xlab = "Birth Weight (oz)", main = "Male Babies, Oakland Kaiser 1960s")
plot(density(infants$bwt,bw=1), xlab = "Birth Weight (oz)", main = "Male Babies, Oakland Kaiser 1960s")
plot(density(infants$bwt,adjust=0.5), xlab = "Birth Weight (oz)", main = "Male Babies, Oakland Kaiser 1960s")

hist(infants$bwt)
hist(infants$bwt[!infants$smoke=="Now"],breaks=50,col=rgb(0,0,1,.3),
     xlab="Birthweight (ounces)",main="Birthweight")
hist(infants$bwt[infants$smoke=="Now"],breaks=50,col=rgb(1,0,0,.3),add=T)
legend(50,40,legend=c("Non-smokers","Smokers"),
       fill=c(rgb(0,0,1,.3),rgb(1,0,0,.3)))

mean(infants$bwt)

sd(infants$bwt)

summary(infants$bwt)

boxplot(infants$bwt)

qqnorm(infants$bwt)
qqline(infants$bwt)
qqnorm(infants$wt)
qqline(infants$wt)
X=runif(1000)
qqnorm(X)
qqline(X)
X=rexp(1000)
qqnorm(X)
qqline(X)
X=rnorm(1000)
qqnorm(X)
qqline(X)

