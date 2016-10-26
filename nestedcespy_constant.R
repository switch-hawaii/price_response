###### R Function
rm(list=ls(all=TRUE))
#library(rootSolve, quietly=TRUE) #package for solving the implicit function

#parameters reused by all functions
# parameters for one customer
# fixed parameters
# 1. shares electricity expenditure = energy expenditure share of GDP
# source: http://199.36.140.204/todayinenergy/detail.cfm?id=18471
# alpha <- c(0.95)
# update: calibrate as 2015 Hawaii GDP = $75,728,404,494.00
# AEI 821 Total annual electricity usage = $ 2,134,093,685.00 
# alpha = 2.82%
 	
alpha <- c(0.9718)

# flexible parameters for different scenarios
# MF note: judging from the cited paper, it seems like elasticity between hours
# should be about 0.1 and own-price elasticity should be about -0.3 
# (opposite of what's shown here)
# 1. elasticity of subtitution for electricity between hours
# source: http://robjhyndman.com/papers/Elasticity2010.pdf page 8 for own-price elasticity
sigma <- c(0.1)  #  allow for any vector
# 2. elasticity of subtitution between electricity and other goods
theta <- c(0.1)
# 3. customer shares of in aggregate demand
#shares <- 1  # allow for any vector, sum=1


#demand for every hour for each agent
# X = electricity demand

####################################################
#simple demand
demandf1 <- function(beta, pd, pb, Mb) {
  p <- pd/pb
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- (sigma-theta)/(1-sigma)
  exp3 <- sum(beta*(p^(1-sigma)))
  e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
  x <- Mb * e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma))
  print(e)
  print((exp3^(exp1))^(-1))
  return(x)
}

demandf <- function(beta, pd, pb, Mb) {
  x1 <- 0
  p <- pd/pb
  for (i in (1:nrow(param))) {
    Mbs <- Mb*param[i,c("type")]*param[i,c("share")]
    sigma <- param[i,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- (sigma-theta)/(1-sigma)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
    x <- Mbs * e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma))
    x1 <- x +x1
  }
  return(x1)
}

####################################################
#demand with different scenario
sigmap <<- c(10, 1.000001, 0.1)
customershare <<- c(0.25, 0.75)
param <- matrix(seq(1,24),6,4, dimnames=list(seq(1,6),c("scen","type","share","sigma")))
set.scenario <- function(scen) {
   ifelse(scen==1, {#1st scenario
                    shareres <<- c(0.25, 0.31, 0.44)
                    sharebus <<- c(0.3, 0.2, 0.5)
                    param <<- matrix(c(rep(scen,6),rep(customershare[[1]],3),rep(customershare[[2]],3),shareres, sharebus, sigmap, sigmap),6,4, dimnames=list(seq(1,6),c("scen","type","share","sigma")))},
          ifelse(scen==2, 
                    {#2nd scenario
                    shareres <<- c(0.175, 0.341, 0.484)
                    sharebus <<- c(0.225, 0.22, 0.555)
                    param <<- matrix(c(rep(scen,6),rep(customershare[[1]],3),rep(customershare[[2]],3),shareres, sharebus, sigmap, sigmap),6,4, dimnames=list(seq(1,6),c("scen","type","share","sigma")))},
          ifelse(scen==4, 
                    {#4nd scenario
                      shareres <<- c(0.025, 0.403, 0.572)
                      sharebus <<- c(0.075, 0.263, 0.662)
                      param <<- matrix(c(rep(scen,6),rep(customershare[[1]],3),rep(customershare[[2]],3),shareres, sharebus, sigmap, sigmap),6,4, dimnames=list(seq(1,6),c("scen","type","share","sigma")))},
          ifelse(scen==3,
                    {#3rd scenario
                    shareres <<- c(0.10, 0.372, 0.528)
                    sharebus <<- c(0.15, 0.24, 0.61)
                    param <<- matrix(c(rep(scen,6),rep(customershare[[1]],3),rep(customershare[[2]],3),shareres, sharebus, sigmap, sigmap),6,4, dimnames=list(seq(1,6),c("scen","type","share","sigma")))
                    }))))
}

#select which scenario
#set.scenario <- function(scen) {
# demand with some plausible shiftable shares derived from data available
# share of residential and business from EIA826 2015
#1) residential #2) business (combined industrial and commercial)
#  customershare <<- c(0.25, 0.75)

#share (highly shiftable, slighly shiftable, no shiftable)
#https://www.hawaiianelectric.com/Documents/save_energy_and_money/Energy-Tips-and-Choices.pdf 
#page 76
#lets use CA as proxy for now https://www.eia.gov/consumption/residential/reports/2009/state_briefs/pdf/ca.pdf
#  shareres <<- c(0.25, 0.31, 0.44)
#https://hawaiienergy.com/for-businesses/business-solutions/
#rough measure
#  sharebus <<- c(0.3, 0.2, 0.5)

#elasticity params 
#  sigmap <<- c(10, 1, 0.1)
#}
set.scenario(4)

demandf2 <- function(beta, pd, pb, Mb) {
  p <- pd/pb
  exp1a <- (1-theta)/(1-sigma1)
  exp1b <- (sigma1-theta)/(1-sigma1) 
  exp1c <- sum(beta*(p^(1-sigma1)))
  exp1d <- share1*((exp1c)^exp1b)*beta*(p^(-1*sigma1))
  exp2a <- (1-theta)/(1-sigma2)
  exp2b <- (sigma2-theta)/(1-sigma2)
  exp2c <- sum(beta*(p^(1-sigma2)))
  exp2d <- share2*((exp2c)^exp2b)*beta*(p^(-1*sigma2))
  exp3a <- (1-theta)/(1-sigma3)
  exp3b <- (sigma3-theta)/(1-sigma3)
  exp3c <- sum(beta*(p^(1-sigma3)))
  exp3d <- share3*((exp3c)^exp3b)*beta*(p^(-1*sigma3))
  e <- alpha+((1-alpha)*((share1*(exp1c^exp1a))+(share2*(exp2c^exp2a))+(share3*(exp3c^exp3a))))
  x <- Mb*((1-alpha)*(exp1d + exp2d + exp3d))
  return(x/e)
}

####################################################
#residential and business customers
demandf3 <- function(beta, pd, pb, Mb) {
  result <- NULL
  p <- pd/pb
  #residential
  for (i in 1:length(sigmap)) {
    sigma <- sigmap[i]
    share <- shareres[i]
    expa <- (1-theta)/(1-sigma)
    expb <- (sigma-theta)/(1-sigma)
    expc <- sum(beta*(p^(1-sigma)))
    expd <- share*((expc)^expb)*beta*(p^(-1*sigma))  
    #expd[is.nan(expd) | expd >3000/share]<- share*3000
    expe <- share*(expc^expa)
    if( sigma==sigmap[1] & share==shareres[1]) result <- matrix(c(share, sigma, expe, expd), nrow=1)
    else result <- rbind( result, matrix(c(share, sigma, expe, expd), nrow=1) )
  }
  eres <- alpha+(1-alpha)*sum(result[,3])
  xres <- (Mb*((1-alpha)*(colSums(result[,4:ncol(result)]))))/eres
  #business
  result <- NULL
  for (i in 1:length(sigmap)) {
    sigma <- sigmap[i]
    share <- sharebus[i]
    expa <- (1-theta)/(1-sigma)
    expb <- (sigma-theta)/(1-sigma)
    expc <- sum(beta*(p^(1-sigma)))
    expd <- share*((expc)^expb)*beta*(p^(-1*sigma))
    #expd[is.nan(expd) | expd >3000/share]<- share*3000
    expe <- share*(expc^expa)
    if( sigma==sigmap[1] & share==sharebus[1]) result <- matrix(c(share, sigma, expe, expd), nrow=1)
    else result <- rbind( result, matrix(c(share, sigma, expe, expd), nrow=1) )
  }
  ebus <- alpha+((1-alpha)*sum(result[,3]))
  xbus <- (Mb*((1-alpha)*(colSums(result[,4:ncol(result)]))))/ebus
  totalx <- (customershare[1]*xres) + (customershare[2]*xbus)
  return(totalx)
}

####################################################
betaf <- function(base.load) {
  betas <- base.load/sum(base.load)
  return (betas)
}

# unit expenditure function
expf1 <- function(beta, pd, pb) {
  p <- pd/pb
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- 1/(1-theta)
  exp3 <- sum(beta*(p^(1-sigma)))
  e <- (alpha+(1-alpha)*(exp3^(exp1)))^exp2
  return(e)
}

expf <- function(beta, pd, pb) {
  e1 <- 0
  p <- pd/pb
  for (i in (1:nrow(param))) {
    sigma <- param[i,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- 1/(1-theta)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- param[i,c("type")]*param[i,c("share")]*((alpha+(1-alpha)*(exp3^(exp1)))^exp2)
    e1 <- e + e1
  }
  return(e1)
}

expf2 <- function(beta, pd, pb) {
  p <- pd/pb
  exp1a <- (1-theta)/(1-sigma1)
  exp1b <- (sigma1-theta)/(1-sigma1)
  exp1c <- sum(beta*(p^(1-sigma1)))
  exp1d <- ((exp1c)^exp1b)*share1*beta*(p^(-1*sigma1))
  
  exp2a <- (1-theta)/(1-sigma2)
  exp2b <- (sigma2-theta)/(1-sigma2)
  exp2c <- sum(beta*(p^(1-sigma2)))
  exp2d <- ((exp2c)^exp2b)*share2*beta*(p^(-1*sigma2))
  
  exp3a <- (1-theta)/(1-sigma3)
  exp3b <- (sigma3-theta)/(1-sigma3)
  exp3c <- sum(beta*(p^(1-sigma3)))
  exp3d <- ((exp3c)^exp3b)*share3*beta*(p^(-1*sigma3))
  
  e <- alpha+(1-alpha)*((share1*(exp1c^exp1a))+(share2*(exp2c^exp2a))+(share3*(exp3c^exp3a)))
  return(e^(1/(1-theta)))
}

expf3 <- function(beta, pd, pb) {
  result <- NULL
  p <- pd/pb
  #residential
  for (i in 1:length(sigmap)) {
    sigma <- sigmap[i]
    share <- shareres[i]
    expa <- (1-theta)/(1-sigma)
    expb <- (sigma-theta)/(1-sigma)
    expc <- sum(beta*(p^(1-sigma)))
    expd <- share*((expc)^expb)*beta*(p^(-1*sigma))  
    expe <- share*(expc^expa)
    if( sigma==sigmap[1] & share==shareres[1]) result <- matrix(c(share, sigma, expe, expd), nrow=1)
    else result <- rbind( result, matrix(c(share, sigma, expe, expd), nrow=1) )
  }
  eres <- (alpha+(1-alpha)*sum(result[,3])^(1/(1-theta)))
  #business
  result <- NULL
  for (i in 1:length(sigmap)) {
    sigma <- sigmap[i]
    share <- sharebus[i]
    expa <- (1-theta)/(1-sigma)
    expb <- (sigma-theta)/(1-sigma)
    expc <- sum(beta*(p^(1-sigma)))
    expd <- share*((expc)^expb)*beta*(p^(-1*sigma))  
    expe <- share*(expc^expa)
    if( sigma==sigmap[1] & share==sharebus[1]) result <- matrix(c(share, sigma, expe, expd), nrow=1)
    else result <- rbind( result, matrix(c(share, sigma, expe, expd), nrow=1) )
  }
  ebus <- (alpha+((1-alpha)*sum(result[,3])))^(1/(1-theta))
  totale <- (customershare[1]*eres) + (customershare[2]*ebus)
  #return(totale^(1/(1-theta)))
  return(totale)
}

indirectuf1 <- function(beta, pd, pb, xd, xb, Mb) { #b=baseline, d=newdata 
  #Md <- sum(xd*pd)/(1-alpha)
  #Md <- (Mb / expf(beta, pd, pb))-sum(xd*pd) #consumer surplus
  #Md <- Mb * (1-expf(beta, pd, pb))
  #Md <- expf(beta, pb, pd) - Mb
  Md <- (Mb / expf(beta, pd, pb))+sum(xd*pd) #willingness to pay
  return(Md)
}

indirectuf <- function(beta, pd, pb, xd, xb, Mb) { #b=baseline, d=newdata 
  m1 <- 0
  p <- pd/pb
  for (i in (1:nrow(param))) {
    sigma <- param[i,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- 1/(1-theta)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- ((alpha+(1-alpha)*(exp3^(exp1)))^exp2)
    m <- (param[i,c("type")]*param[i,c("share")]*Mb)/e
    m1 <- m + m1
    #print(paste(exp1, exp2, exp3, e, m, m1))
  }
return(m1+sum(xd*pd))
}
#indirectuf1(beta, df1$base_price, df1$base_price, df1w$FlexibleDemand, df1$base_price, Mb)-Mb

calibrate <- function(base.loads, base.prices, scenario) {
  # force strictly positive prices (never really an issue with base.prices)
  #base.prices[base.prices < 1] <- 1
  #base.prices <- base.prices + 10
  #base.prices[base.prices < 1] <- 5
  # store base values for future reference
  base.loads <<- base.loads
  base.prices <<- base.prices
  set.scenario(scenario)
  # make an empty array for the beta values
  b <<- array(numeric(length(base.loads)), dim(base.loads), dimnames=dimnames(base.loads))
  # b <<- 0.0 * base.loads
  # fill in the beta values, column by column (there should be some way to do this with
  # apply, mapply or similar, but it's not obvious how)
  dims = dim(base.loads)
  for (ts in 1:dims[2]) 
    for (lo in 1:dims[3])
      b[,ts,lo] <<- betaf(base.loads[,ts,lo])
}

bid <- function(location, timeseries, prices, scenario) {
  # force strictly positive prices
  #prices[prices < 15] <- 15
  #prices <- prices + 10
  prices[prices < 5] <- 5
  b.loads = base.loads[,timeseries,location]
  b.prices = base.prices[,timeseries,location] 
  Mb <- sum(b.loads*b.prices)/(1-alpha)
  demand = demandf(b[,timeseries,location], prices, b.prices, Mb)/b.prices
  #demand[demand >3000] <- 3000
  #demand[is.nan(demand)] <- 3000
  #prices1 <- prices
  #prices1[prices1 < 5] <- 5
  wtp = indirectuf(b[,timeseries,location], prices, b.prices, demand, b.loads, Mb)
  return (list(demand, wtp))
}

makearray <- function() {
	array(0:31, dim=c(3,2,2,2), dimnames=list(c(12, 13, 14), c("prices", "demand"), c("oahu","maui"), c(1, 2)))
}

make2darray <- function() {
	array(rep(2, 16), dim=c(2,2), dimnames=list(c(12, 13), c("prices", "demand")))
}


showobject <- function(obj) {
	print(obj)
	print(attributes(obj))
	return (obj)
}


test_calib <- function () {
	base.loads <- 1000 * array(rep(1, 5), dim=c(5,1,1), dimnames=list(c(12, 13, 14, 15, 16), c(100), c("oahu")))
	base.prices <- 0.0*base.loads - 180
	calibrate(base.loads, base.prices, 3)
	print_calib()
}

print_calib <- function () {
	print ("beta:")
	print (b)
	print ("base prices:")
	print (base.prices)
	print ("base demand:")
	print (base.loads)
	#print ("demand (day 1, site 1):")
	#print (demandf(b[,1,1], base.prices[,1,1], M[1,1]))
  print ("bid(1, 1, base.prices[,1,1]):")
  print (bid(1, 1, base.prices[,1,1]))
}

test <- function() {
	# parameters that vary day by day
	#1. base load
	base.l <-c(718.0014, 680.5867, 662.7549, 665.3259, 706.5587, 796.7969, 890.2706, 962.2579, 1032.4267, 
	           1074.0698, 1093.5465, 1099.8633, 1103.0553, 1100.2026, 1097.8984,
	           1095.9867, 1088.0893, 1091.6672, 1111.0894, 1094.1057, 1042.7211, 969.1186, 871.5261, 778.1248)
	#2. base price
	base.p <- rep(180, 24)

	#3. total money each agent has
	M <- sum(base.l*base.p)/(1-alpha)

	#4. the calibrated beta is the share of expenditure
	betas <- base.l/sum(base.l)
	
  #reset the parameters
  theta <- 0.1
	sigma <- 0.1
	alpha <- 0.95
  
	#test
	print(demandf(betas, base.p, base.p, M)/base.p)
	print(demandf(betas, base.p*-2, base.p, M)/base.p)
}
