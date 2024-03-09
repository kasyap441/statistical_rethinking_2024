library(ggplot2)
library(rethinking)
#1. Constructing the posterior distribution
W <- 3
L <- 11

p_grid <- seq(from=0, to=1, length.out=1000)
unstd_prior <- rep(1, 1000) # flat prior
likeli <- dbinom(3, 14, p_grid)
plot(p_grid, likeli, type='b')
unstd_pos <- likeli * unstd_prior
std_pos <- unstd_pos/sum(unstd_pos)
#plotting
plot(p_grid, std_pos, type='b')

#alter:
curve( dbeta(x,3+1,11+1) , from=0 , to=1 , xlab="p" )

#2. Using posterior from #1, construct posterior predictive distribution
#for next 5 tosses

#sample from the posterior
samp <- sample(p_grid, size=1e4, replace=TRUE, prob=std_pos)
ppd <- rbinom(10000, 5, prob=samp)
simplehist(ppd)
table(ppd)
plot(table(ppd))

#alter
#p_samples <- rbeta(1e4,3+1,11+1)
#W_sim <- rbinom(1e4,size=5,p=p_samples)

#7 water points, no idea about total tosses, p=0.7, construct posterior for 
#total number of tosses

w <- 7
p <- 0.7
N_grid <- seq(from=1, to=100, length.out=100)
prior <- rep(1, 100)
likeli_2 <- dbinom(x=7,N_grid, prob=p)
unstd_pos_2 <- likeli_2*prior
std_pos_2 <- unstd_pos_2/sum(unstd_pos_2)
plot(N_grid, std_pos_2, type='b')

#alter
#compute_posterior_N <- function( W , p , N_max ) {
#  ways <- sapply( W:N_max ,
#                  function(n) choose(n,W) * p^W * (1-p)^(n-W) )
#  post <- ways/sum(ways)
#  data.frame( N=W:N_max , ways , post=round(post,3) )
#}
#t <- compute_posterior_N( W=7 , p=0.7 , N_max=20 )
#plot(t$N, t$post)
