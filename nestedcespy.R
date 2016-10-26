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
# sigma <- c(0.1)  #  allow for any vector
# 2. elasticity of subtitution between electricity and other goods
theta <- c(0.1)
# 3. customer shares of in aggregate demand
#shares <- 1  # allow for any vector, sum=1


#demand for every hour for each agent
# X = electricity demand

####################################################
#demand with different scenario

#select which scenario
#share (highly shiftable, slighly shiftable, no shiftable)
sigmap <<- c(10, 1.000001, 0.1)
# the next line is not needed and doesn't work on computers with different directory structures
#setwd("~/Dropbox/demand_system")
#setwd("D:/Dropbox/demand_system")

flexshares <- read.csv(file="flexshares.csv", header=TRUE, sep=",")
#param <- matrix(seq(1,72),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))
set.scenario <- function(scen, month) {
  ifelse(scen==1, {#optimistic
    shareflex <<- c(0.67, 0.05, 0.28)
    shareother <<- c(0.15, 0.05, 0.8)
    paramf <<- matrix(c(rep(scen,72),rep(flexshares[,month],3),rep(shareflex[[1]],24), rep(shareflex[[2]],24), rep(shareflex[[3]],24), rep(sigmap[[1]],24),rep(sigmap[[2]],24),rep(sigmap[[3]],24)),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))
    paramot <<- matrix(c(rep(scen,72),rep(1-flexshares[,month],3),rep(shareother[[1]],24), rep(shareother[[2]],24), rep(shareother[[3]],24), rep(sigmap[[1]],24),rep(sigmap[[2]],24),rep(sigmap[[3]],24)),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))},
    ifelse(scen==2, 
{#2nd scenario=baseline
  shareflex <<- c(0.33, 0.05, 0.62)
  shareother <<- c(0.075, 0.05, 0.875)
  paramf <<- matrix(c(rep(scen,72),rep(flexshares[,month],3),rep(shareflex[[1]],24), rep(shareflex[[2]],24), rep(shareflex[[3]],24), rep(sigmap[[1]],24),rep(sigmap[[2]],24),rep(sigmap[[3]],24)),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))
  paramot <<- matrix(c(rep(scen,72),rep(1-flexshares[,month],3),rep(shareother[[1]],24), rep(shareother[[2]],24), rep(shareother[[3]],24), rep(sigmap[[1]],24),rep(sigmap[[2]],24),rep(sigmap[[3]],24)),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))},
ifelse(scen==3,
{#3rd scenario=pessimistic
  shareflex <<- c(0.15, 0.05, 0.8)
  shareother <<- c(0, 0.05, 0.95)
  paramf <<- matrix(c(rep(scen,72),rep(flexshares[,month],3),rep(shareflex[[1]],24), rep(shareflex[[2]],24), rep(shareflex[[3]],24), rep(sigmap[[1]],24),rep(sigmap[[2]],24),rep(sigmap[[3]],24)),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))
  paramot <<- matrix(c(rep(scen,72),rep(1-flexshares[,month],3),rep(shareother[[1]],24), rep(shareother[[2]],24), rep(shareother[[3]],24), rep(sigmap[[1]],24),rep(sigmap[[2]],24),rep(sigmap[[3]],24)),72,4, dimnames=list(seq(1,72),c("scen","type","share","sigma")))
})))
}
#set.scenario(2,2)

####################################################

demandf <- function(beta, pd, pb, Mb, month, scen) {
  x1 <- 0
  p <- pd/pb
  set.scenario(scen,month)
  for (i in (1:3)) {
    ifelse (i==1, startrow <- 1, startrow<- ((i-1)*24)+1)
    sigmarow <- i*24
    Mbsf <- paramf[startrow:sigmarow,c("type")]*paramf[startrow:sigmarow,c("share")]*Mb
    Mbsot <- paramot[startrow:sigmarow,c("type")]*paramot[startrow:sigmarow,c("share")]*Mb
    sigma <- paramf[sigmarow,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- (sigma-theta)/(1-sigma)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
    x <- (Mbsf * e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma)))+(Mbsot * e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma)))
    x1 <- x +x1
  }
  return(x1)
}
#test
#sum((demandf(betas, base.p, base.p, M,1,5)/base.p)-base.l) #shoould be 0

####################################################
betaf <- function(base.load) {
  betas <- base.load/sum(base.load)
  return (betas)
}

# unit expenditure function
expf <- function(beta, pd, pb, month, scen) {
  e1 <- 0
  p <- pd/pb
  set.scenario(scen,month)
  for (i in (1:3)) {
    ifelse (i==1, startrow <- 1, startrow<- ((i-1)*24)+1)
    sigmarow <- i*24
    sigma <- paramf[sigmarow,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- 1/(1-theta)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- (paramf[startrow:sigmarow,c("type")]*paramf[startrow:sigmarow,c("share")]*((alpha+(1-alpha)*(exp3^(exp1)))^exp2))+(paramot[startrow:sigmarow,c("type")]*paramot[startrow:sigmarow,c("share")]*((alpha+(1-alpha)*(exp3^(exp1)))^exp2))
    e1 <- e + e1
  }
  return(e1)
}

wtpf <- function(beta, pd, pb, xd, xb, Mb, month, scen) { #b=baseline, d=newdata 
  m1 <- 0
  p <- pd/pb
  set.scenario(scen,month)
  for (i in (1:3)) {
    ifelse (i==1, startrow <- 1, startrow<- ((i-1)*24)+1)
    sigmarow <- i*24
    sigma <- paramf[sigmarow,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- 1/(1-theta)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- ((alpha+(1-alpha)*(exp3^(exp1)))^exp2)
    m <- ((paramf[startrow:sigmarow,c("type")]*paramf[startrow:sigmarow,c("share")]*Mb)/e)+((paramot[startrow:sigmarow,c("type")]*paramot[startrow:sigmarow,c("share")]*Mb/e))
    m1 <- m + m1
    #print(paste(exp1, exp2, exp3, sigma, e, m, m1))
  }
return(mean(m1))
}
#test
#wtp(betas, base.p, base.p, base.l, base.l,M,1,5)-M #should be 0

calibrate <- function(base.loads, base.prices) {
  # force strictly positive prices (never really an issue with base.prices)
  #base.prices[base.prices < 1] <- 1
  #base.prices <- base.prices + 10
  #base.prices[base.prices < 1] <- 5
  # store base values for future reference
  base.loads <<- base.loads
  base.prices <<- base.prices
  #set.scenario(scenario, month)
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
  # we don't have a good way to know the month, but this works for inputs_tiny and inputs_2045_15
  if (nchar(timeseries) == 4) {   # tiny dataset
    month <- as.numeric(substr(timeseries, 3, 4))
  } else {                      # normal dataset
    month <- as.numeric(substr(timeseries, 5, 6))    
  }
  # force strictly positive prices
  #prices[prices < 15] <- 15
  #prices <- prices + 10
  prices[prices < 5] <- 5
  b.loads = base.loads[,timeseries,location]
  b.prices = base.prices[,timeseries,location] 
  Mb <- sum(b.loads*b.prices)/(1-alpha)
  demand = demandf(b[,timeseries,location], prices, b.prices, Mb, month, scenario)/b.prices
  #demand[demand >3000] <- 3000
  #demand[is.nan(demand)] <- 3000
  #prices1 <- prices
  #prices1[prices1 < 5] <- 5
  wtp = wtpf(b[,timeseries,location], prices, b.prices, demand, b.loads, Mb, month, scenario)
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
	base.prices <- 0.0*base.loads-180
	print(base.prices)
	calibrate(base.loads, base.prices)
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
  print (bid(1, 20070515, base.prices[,1,1], 2))
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
	print(demandf(betas, base.p, base.p, M, 1, 1)/base.p)
	print(demandf(betas, base.p-100, base.p, M, 1, 1)/base.p)
}
