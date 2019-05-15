rm(list=ls()) # clears workspace

load("ccte_data.Rdata")

# load packages
library(runjags)
library(coda)

## Operationalising enjoyment
# Reverse boring
library(car)
Revesed_Boring <- recode(ccte$Boring,"1=9; 2=8; 3=7; 4=6; 5=5; 6=4; 7=3; 8=2; 9=1") 
# Enjoyment 
ccte$enjoyment=rowMeans(cbind(ccte$Enjoy,ccte$Entertain,Revesed_Boring))
ccte$enjoyment

# get descriptives
## inspect the data: condition means and sds
tapply(ccte[,"enjoyment"], ccte$Condition, mean)  
tapply(ccte[,"enjoyment"], ccte$Condition, sd)
par(mfrow=c(1,2))

## visualization
## histogram Doing
Doing_data <- ccte[which(ccte$Condition =="Doing"),]
hist(x=Doing_data $enjoyment, 
     main = "Doing",
     col = "lightblue",
     border = "white",
     xlab = "Enjoyment",
     ylab = "n",
     breaks=10)

## histogram Thinking
Thinking_data <- ccte[which(ccte$Condition =="Thinking"),]
hist(x=Thinking_data $enjoyment, 
     main = "Thinking",
     col = "lightblue",
     border = "white",
     xlab = "Enjoyment",
     ylab = "n",
     breaks=10)


## classical analysis
t.test(formula = ccte$enjoyment ~ ccte$Condition, # formula: Enjoyment is a function of Condition
      var.equal=T) # let's assume the variances are equal in the two Conditions

## JAGS approach
### modelString still need to be changed!!!!!!!
modelString = "

model{
#data come from a Gaussian
for (i in 1:2565){
x[i] ~ dnorm(mu,lambda)
}
#Priors
mu ~ dnorm(Mmu, 1/Smu^2)
sigma ~ dunif(L,H)
lambda <- 1/pow(sigma,2)
delta <- (mu - mu0)/sigma
}

"
writeLines(modelString, con = "Mean.txt")

### ?????????
modelString = "
model{
#Data Come From Gaussians With Different Means But Common Precision
for (i in 1:ndata){
      x[i] ~ dnorm(mu[Condition[i]],lambda)
    }
#Priors
sigma ~ dunif(0,100)
lambda <- 1/pow(sigma,2)
for (i in 1:n){
mu[i] ~ dunif(0,300)
        }
}
"
writeLines(modelString, con = "Mean_Repeated_Nested.txt")


# Get the data in the correct format
enjoyment = as.numeric(ccte[,"enjoyment"])
Condition = as.numeric(as.factor(ccte[,"Condition"])) #group 2 is smart drug; group 1 is placebo  

x = enjoyment
Condition = y

# Specify the data in a list, for later shipment to JAGS:
data = list(
  x=x, # all scores
  Condition = y, # index of group membership (1 or 2)
  n = length(unique(y)), # number of groups
  ndata = length(x)  # total number of measurements
)

myinits=NULL

# parameters to be monitored:   
parameters <- c("mu", "sigma")

out <- run.jags( model="Mean_Repeated_Nested.txt" , monitor=parameters , data=data ,  
                 inits=myinits , n.chains=3 ,  adapt=100  , burnin=10 ,  sample=100) 
##1000,1000,10000

summary(out)




# compute effect size 
delta<-( combine.mcmc(out$mcmc)[,"mu[2]"]-combine.mcmc(out$mcmc)[,"mu[1]"] )/ combine.mcmc(out$mcmc)[,"sigma"]
mean(delta)

##calculate the credibility interval on the effect size
quantile(delta, c(.025,.975))
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
Nbreaks <- 80
y       <- hist(delta, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=1,
     xlab="Effect size", ylab="Posterior Density") 
segments(c(y$breaks, max(y$breaks)), rep(0,89),
         c(y$breaks, max(y$breaks)),
         c(0,y$density,0), col=rgb(0,0,0, alpha=.2))
title(main =expression(paste("density plot of ", delta)))





