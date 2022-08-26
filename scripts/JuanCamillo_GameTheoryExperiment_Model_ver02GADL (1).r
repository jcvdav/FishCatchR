#########################################################################################
# 
# NSF-CNH 2022, Project lead: Fiorenza Micheli
# 
# Model by: Juan Camillo, John Lynham, Brock Woodson, Besty Field and Giulio De Leo
#
# 
#########################################################################################
rm(list=ls(all=TRUE)) 
#########################################################################################
# random number generator for unbiased log-normal distribution whose mean is really equal 
# to "m" given SD. We used to accound for background environmental variability.

rlnormcustom <- function(n, m, s){
  rlnorm(n, 
         meanlog=log(m^2 / sqrt(s^2 + m^2)), 
         sdlog=sqrt(log(1 + (s^2 / m^2))))
}

#########################################################################################
# Juan Camillo's stock recruitment relationship
# 
# It assumes a fixed rate of increase of the stock, until the stock reaches carrying 
# capacity K. 
# The model here coded  multiplies recruitment by a random factor "gvariation" 
# lognormally distributed, with mean equal to 1 and a desired standard deviation.
# It also multiplies the stock by the random factor "CatEvent", which is equal to 1 in 90%
# of the ases and 0.65 in 10% of the cases, thus representing a catastrophic mortality 
# with a fairly small probability to occur. This is superimposed to background 
#environmental variability

SRfun <-  function(Ni, K, growthrate, gvariation, CatEvent){
  
  # growthrate <- gr; gvariation <- ge[i]; CatEvent <- ce[i]; Ni <- N[i]
  # growthrate; gvariation; CatEvent; Ni
  
  Nx <- K/(1 + growthrate) # If Ni(t) < Nx then recruitment Np(t+1) < K 
  
  # if Ni(t) >= Nx, then recruitment Np(t+1) = K ( * env.variation * catastrophic factor)  
  Np <- (1 + growthrate) * gvariation * ifelse(Ni < Nx, Ni, Nx) * CatEvent
  
  return(Np)

}



#########################################################################################
# model parameters
 
gr <- 0.1          # 10 percent annual rate of increase
K  <-  100         # population carrying capacity
InitCond   <-  K   # Initial Conditions (at carrying capacity)
Iterations <-  20  # number of iterations pf the games (time horizon)
catprob    <-  0.1 # probability of a catastrophic event
catmort    <-  0.5 # catastrophic mortality

# for this model, it is simple to derive the Maximum Sustainable Yield (MSY) 

N.MSY <- K/(1+gr)   # stock at MSY
MSY   <- K * (1 - 1/(1+gr)); MSY; K/(1+gr);  # MSY 
N.MSY + MSY  == K  # double check that this is correct 


#########################################################################################
# we now have to set aside memory for the state variables

N  <-  numeric(length = Iterations)  # population at time t

# this is the vector to simulate year-o-year background environmental variability

ge <- rlnormcustom(Iterations, m = 1, s = 0.1) 
mean(ge); sd(ge); sd(ge)/mean(ge); min(ge); max(ge) 

#  this is the vector to simulate catastrophic collapse
ce <- ifelse(runif(Iterations) > catprob, 1, (1 - catmort))

# number of catastrophic events in
Iterations                    # this is the simulation time
length(which(ce == catmort))  # this is the number of catastrophic events
which(ce == catmort)          # this is when catastrophic events occurs

#########################################################################################
# Now let's simulate the dynamics of the unfished population

N[1] <- InitCond  # set the initial condition

for (i in (1:(Iterations - 1))){  # run the simulation

   N[i+1] <- SRfun(N[i], K, gr, ge[i], ce[i])

}

#########################################################################################
# let's plot the results

plot(1:Iterations, N, type = 'l', lwd = 3, ylim = c(0, max(N)))
abline(h = K, lty = 3, lwd = 3, col = 'lightgrey')
abline(v = c(1+which(ce == catmort)), lty = 3, col = "grey")

mean(N); sd(N); min(N); max(N); sd(N)/mean(N)


#########################################################################################
# this chunk is to plot the stock-recruitment relationship (the phase diagram) 

# create a sequence of values of stock abundances 
Nd   <- seq(from=0, to = K*1.5, by = K/(1+gr)/111)  

# for each of them, determine the corresponding recruitment (no noise, # no catastrophes)
SRd <- SRfun(Nd, K = K, growthrate = gr, gvariation = 1, CatEvent = 1)
  
plot(Nd, SRd,lwd=4,ylab="N(t+1)",xlab="N(t)", axes = FALSE, type = 'l', ylim = c(0, K * 1.25), main = "stock-recruitment relationship") 
axis(1, pos=0); axis(2, pos=0)

# draw some straight lines with specific meaning
abline(a=0,b=1) # draw the bisector (a straight line with slope equal to 1) 
abline(v=K, lty=3, col="darkgrey") #this is the vertical line at N(t)=K
abline(h=K, lty=3, col="darkgrey") #this is the vertical line at N(t)=K
points(K,K,col=3, bg=2, pch=21, cex=2)


# now we simulate the population dynamics in the deterministic framework (no noise, 
# no catastrophes)
tbuf  <- 100
nb    <- numeric(tbuf) 
nb[1] <- 8 # K/50 

for(i in 1:(tbuf-1)) {
  
  nb[i+1] <- SRfun(nb[i], K = K, growthrate = gr, gvariation = 1, CatEvent = 1)
  
}  

# and plot the dynamics in the phase diagram (in red)
lines(nb[-length(nb)],nb[-1],type="s",lwd=2,col="red",ylab="N[t+1]",xlab="N[t]") 

# now plot the data with environmental variability and, if occurring, catastrophic events
points(N[-length(N)],N[-1],type="s",lty = 2, lwd=1,col="darkgray",ylab="N[t+1]",xlab="N[t]") 
points(N[-length(N)],N[-1],type="p",lwd=1,col="blue",ylab="N[t+1]",xlab="N[t]") 

# again, let's remind the number of catastrophic events
length(which(ce == catmort))

# and plot the stock abundance following catastrophic mortality in red
points(N[which(ce == catmort)], N[1+which(ce == catmort)], type="p",lwd=1,col="red",
       ylab="N[t+1]",xlab="N[t]", pch = 16) 

#########################################################################################
# Here below we simulate harvesting with any number of fishermen, each following one of 
# the three fishing strategies, namely: aggressive (A), moderate (M) and 
# conservative (C)


#########################################################################################
# catch preference for fishermen type, scaled to maximum harvestable quota

preference <-  function(Ftype, maxCatch){ # Ftype: type of fishermen, maxCatch: max quota
  
  probv <- switch(Ftype,
                  "A" = {dnorm(0:maxCatch/maxCatch, mean = 4.0/5, sd = 0.25)}, # Aggressive
                  "M" = {dnorm(0:maxCatch/maxCatch, mean = 2.5/5, sd = 0.25)}, # Moderate
                  "C" = {dnorm(0:maxCatch/maxCatch, mean = 1.0/5, sd = 0.25)}  # Conservative
  )
  
  return(probv/sum(probv))  # normalize the probabilities so that they sum to 1
  
}

def <- 
par(mfrow=c(3,1))
par(mar = c(3, 3.5, 2, 0.5))
par(mgp = c(1.5, 0.5, 0))
par(oma = c(4, 0, 3, 0))

barplot(preference(Ftype ="A", maxCatch = 5), names.arg = 0:5, main = 'Aggressive')
barplot(preference(Ftype ="M", maxCatch = 5), names.arg = 0:5, main = 'Moderate')
barplot(preference(Ftype ="C", maxCatch = 5), names.arg = 0:5, xlab = "harvested units", 
        main = 'Conservative')
par(def)

#########################################################################################
# Harvesting model

HarvModel <-  function(InitCond, K, ge, ce, FishermenType, Iterations){
  
  # growthrate <- gr; gvariation <- ge[i]; CatEvent <- ce[i]; Ni <- N[i]
  # growthrate; gvariation; CatEvent; Ni
  
  
  numFishermen <-  length(FishermenType) # this tells us how many player we have
  catch <- matrix(0,nrow = Iterations, ncol = numFishermen) # here we save the catch
  colnames(catch) <- FishermenType
  obsN <- Np <- Nc <- TotCatch <- numeric(length = Iterations) # other variables
  Nc[1] <- InitCond # initial value
  
  i <- 1 # this can be erased, it is here just for convenience, for debugging now
  
  for (i in (1:Iterations)){
    
    Np[i]      <- SRfun(Nc[i], K, gr, ge[i], ce[i])  # recruitment with background noise
    
    obsN[i]    <- rpois(1, Np[i])                    # stock assessment (obs. error)
    
    MaxCatchPF <- round(min(5, obsN[i]/numFishermen))# max catch per fishermen
    
    # print(i)
    j <-  1  # this is just for debugging
    
    for (j in (1:numFishermen)){
      catch[i, j] <- ifelse(MaxCatchPF == 0, 0, 
                            which(rmultinom(1, 
                                            size = 1, 
                                            prob = preference(FishermenType[j], 
                                                              MaxCatchPF))>0)-1)   
    }
    
    TotCatch[i] <- sum(catch[i,]) # compute the cumulatyed catch
    
    # double check we are not trying to remove more than it is actually available
    # this is because the decision on how much to harevst is based on observations that 
    # cab be flawed with uncertainty.
    #  
    
    TotCatch[i] <- ifelse(TotCatch[i]<=Np[i], TotCatch[i], Np[i])
    
    
    if (i< Iterations){ # we do not run it when i == Iteractions 
      Nc[i+1] <- ifelse(TotCatch[i]< Np[i], Np[i] - TotCatch[i], 0)  
    }
  }
  
  
  return(cbind(Nc, Np, obsN, TotCatch, catch, ge, ce))  # return the values
}



FishermenType <- c("A", "M", "C",  "C", "C") # add more fishermen by adding letters (with a comma) 

numFishermen  <- length(FishermenType) 

HarvSim <- HarvModel(InitCond, K, ge, ce, FishermenType, Iterations)
options(digits = 3)
HarvSim

# head(HarvSim)

Nc <- HarvSim[, 1]
Np <- HarvSim[, 2]
No <- HarvSim[, 3]
TC <- HarvSim[, 4]
Yi <- HarvSim[, 5:(4+length(FishermenType))]

Yi
colSums(Yi)
rowSums(Yi)
colMeans((Yi))
mean(Yi)
MSY/numFishermen
MSY; 

output <- rbind( cbind(1:Iterations, Nc), cbind((1:Iterations)+0.5, Np))
output <- output[order(output[,1], decreasing = FALSE),]

plot(output[,1], output[,2], type = 'l', lwd = 3, ylim = c(0, max(output[,2], N)),
    xlab = 'years', ylab = 'abundances')
points(1:Iterations,  N, type = 'l', lwd = 3, col = 'gray', lty = 3)
points((1:Iterations)+0.5, Np, type = 'p', lwd = 3, col = 'green', pch = 16)
points((1:Iterations)+0.5, No, type = 'p', lwd = 1, col = 'red', pch = 17)
points((1:Iterations)+0.5, TC, type = 'h', lwd = 10, col = 'gray')

abline(h = K, lty = 3, lwd = 1, col = 'lightgrey')
abline(v = 0.5 + which(ce == catmort), lty = 3, lwd = 2, col = "darkred")

legend(x = 'left', 
       legend = c('recruitment', 'stock assessment with obs. error','unfished dyn', 'catch'),
       col = c('green', 'red', 'gray', 'gray'), 
       pch = c(16, 17, -1, -1), 
       lty = c(0, 0, 3, 1),
       lwd = c(1, 1, 3, 10),
       bty = 'n')

# tail(HarvSim[, 1:4])

matplot(1:Iterations, Yi, type = 'l', lwd = 3, 
        lty = 1)

par(mfrow=c(1,1))
#########################################################################################
# 