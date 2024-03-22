library(rethinking)
library(dagitty)


data("Howell1")
d <- Howell1[Howell1$age < 13,]
#prob-1: Drawing the DAG
dag2.1 <- dagitty("dag{A->W; A->H; H->W}")
coordinates(dag2.1) <- list(x=c(A=0, H=1, W=2), y=c(A=0, H=1, W=0))
drawdag(dag2.1)
#generative simulation that takes age as input and simulates height and weight
#before that lets simulate individually and see what it shows
H <- standardize(d$height)
W <- standardize(d$weight)
A <- standardize(d$age)

#std dev age
sd(d$age) #--> 3.83
sd(d$height) #--> 21.49
sd(d$weight) #--> 5.72
#if slope is'1', a change of 3.83 years changes 1std dev of height and weight
# here it seems very reasonable since in younger age, growth makes it plausible

#height vs age
m1.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bA *A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(1, 0.25),
    sigma ~ dexp(1)
  ), data=d
)
precis(m1.1)

#weight vs height
m1.2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bH *H,
    a ~ dnorm(0, 0.2),
    bH ~ dnorm(1, 0.25),
    sigma ~ dexp(1)
  ), data=d
)
precis(m1.2)

#age, height to model weight independently
m1.3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bH *H + bA * A,
    a ~ dnorm(0, 0.2),
    bH ~ dnorm(1, 0.25),
    bA ~ dnorm(1, 0.25),
    sigma ~ dexp(1)
  ), data=d)
precis(m1.3)
# this seems to tell that Age is entirely contributing through height!!
plot(coeftab(m1.1, m1.2, m1.3), par=c("bA", "bH"))
#coeftab_plot(coeftab(m1.1, m1.2, m1.3), par=c("bA", "bH")) #--> same

#Now for the final quest --> simulating including the dependencies
#note --> without creating a list as below; it is erroring out
new_d <- list()
new_d$H <- standardize(d$height)
new_d$W <- standardize(d$weight)
new_d$A <- standardize(d$age)
m1.4 <- quap(
  alist(
    #A-->W<--H
    W ~ dnorm(mu, sigma),
    mu <- a + bHW *H + bAW * A,
    a ~ dnorm(0, 0.2),
    bHW ~ dnorm(1, 0.25),
    bAW ~ dnorm(1, 0.25),
    sigma ~ dexp(1),
    #A-->H
    H ~ dnorm(mu_H, sigma_H),
    mu_H <- aH + bAH * A,
    aH ~ dnorm(0, 0.2),
    bAH ~ dnorm(1, 0.25),
    sigma_H ~ dexp(1)
  ),data=new_d
)
 precis(m1.4) 
 # results tell us that Age acts entirely through height!!
 #generative simulation that takes age as input and simulates height and weight
 A_seq <- seq(from=-2, to=2, length.out=30)
 #prep_data
 sim_dat <- data.frame(A=A_seq)
 # vars object tells which variables to simulate and in what order
 s <- sim(m1.4, data=sim_dat, vars=c("H", "W")) 
 #Question one answered above finally
 
 #question:2 estimate causal effect of each year of growth on weight
 #bA      0.10 0.05  0.02  0.18 --> age on weight
 #bAH     0.92 0.03  0.87  0.97 --> age on height
 #total causal effect of age on weight is 90% from m1.1
 
 #trying without standarizing to see if we meet the solutions
 m1.5 <- quap(
   alist(
     W ~ dnorm(mu, sigma),
     mu <- a + bA *A,
     a ~ dnorm(5,1),  #birth weight --> 5kgs (age=0)
     bA ~ dunif(0, 10),
     sigma ~ dexp(1)
   ), data=list(W=d$weight, A=d$age)
 )
precis(m1.5) 
#need to check
#above result tells that total casual effect for age (89% interval) is 1.29 to 1.46 kgs per yearr

#However m1.1 tells bA interval at 0.84 to 0.96
#the interpretation is different here; this tells that 1 std dev change in weight is associated
#with 0.84 to 0.96 times the std deviation of age

#Alter sol from PDF
# sim from priors
n <- 10
a <- rnorm(n,5,1)
b <- runif(n,0,10)
plot( NULL , xlim=range(d$age) , ylim=range(d$weight) ,
      xlab="age" , ylab="weight" )
for ( i in 1:n ) abline( a[i] , b[i] , lwd=3 , col=2 )
#assumption is birth weight is around 5kgs


#optional Challenge
data(Oxboys)
d <- Oxboys
str(d)
#subjects are in order so we can directly loop through
#ignore Occasion 1 as difference cant be computed to occ 0
d$delta <- NA
for ( i in 1:nrow(d)) {
  if (d$Occasion[i] > 1) {
    d$delta[i] <- d$height[i] - d$height[i-1]
  }
}
d <- d[!is.na(d$delta), ]
#processing is done.. now time to model
m2.6 <- quap(
  alist(
    delta ~ dlnorm(alpha, sigma),
    alpha ~ dnorm(0, 0.1),
    sigma ~ dexp(3)
  ), data=list(delta=d$delta)
)
precis(m2.6)

# simulation from priors
n <- 1e3
alpha <- rnorm(n,0,0.1)
sigma <- rexp(n,1)
delta_sim <- rlnorm(n,alpha,sigma)
dens(delta_sim)
#If you play around with prior predictive simulation, 
#you’ll see that variance in thepriors leads to really explosive means.
#if sigma is wider, it will make prior mean way too high
#if sigma exp(3), it is around 10, for exp(1), around 5
###

post <- extract.samples(m2.6)
dens(post)
dsim <- rlnorm(1e3, post$alpha, post$sigma)
dens(dsim)
#sum over 8 occasions of growth
inc_sum <- sapply(1:1000, function(s) sum(rlnorm(8, post$alpha[s], post$sigma[s])))
dens(inc_sum)
  