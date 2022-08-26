######################################################
# supporting functions
######################################################
# 
# 
#
######################################################

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

# DEFINE FUNCTIONS
rlnormcustom <- function(n, m, s){
  rlnorm(n, 
         meanlog=log(m^2 / sqrt(s^2 + m^2)), 
         sdlog=sqrt(log(1 + (s^2 / m^2))))
}

SRfun <-  function(Ni, K, growthrate, gvariation, CatEvent){
  
  # growthrate <- gr; gvariation <- ge[i]; CatEvent <- ce[i]; Ni <- N[i]
  # growthrate; gvariation; CatEvent; Ni
  
  Nx <- K/(1 + growthrate) # If Ni(t) < Nx then recruitment Np(t+1) < K 
  
  # if Ni(t) >= Nx, then recruitment Np(t+1) = K ( * env.variation * catastrophic factor)  
  Np <- (1 + growthrate) * gvariation * ifelse(Ni < Nx, Ni, Nx) * CatEvent
  
  return(Np)
  
}

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

get_N <- function(harvest, N, K, gr, ge, ce, FishermenType, i) {
  numFishermen <- length(FishermenType) + 1 # this tells us how many player we have
  catch <- numeric(length = numFishermen) # here we save the catch
  Nc <- N
  
  Np <- SRfun(Nc, K, gr, ge, ce)  # recruitment with background noise
  obsN <- rpois(1, Np)                    # stock assessment (obs. error)
  MaxCatchPF <- round(min(5, obsN/numFishermen))# max catch per fishermen
  
  # print(i)
  # j <-  1  # this is just for debugging
  # This is the harvest the user chooses
  
  for (j in (1:(numFishermen - 1))){
    catch[j] <- ifelse(MaxCatchPF == 0, 0, 
                       which(rmultinom(1, 
                                       size = 1, 
                                       prob = preference(FishermenType[j], 
                                                         MaxCatchPF))>0)-1)
  }
  
  catch[5] <- harvest
  
  TotCatch <- sum(catch, na.rm = T) # compute the cumulatyed catch
  
  # double check we are not trying to remove more than it is actually available
  # this is because the decision on how much to harevst is based on observations that 
  # cab be flawed with uncertainty.
  if(TotCatch > Np) {
    TotCatch <- Np
  }
  
  Nc <- ifelse(TotCatch < Np, Np - TotCatch, 0)  
  
  return(tibble(Nc, Np, obsN, h = harvest, TotCatch, ge, ce, t = i))  # return the values
}