###### R Function
rm(list=ls(all=TRUE))
#library(rootSolve, quietly=TRUE) #package for solving the implicit function

#parameters reused by all functions
# parameters for one customer
# fixed parameters
# 1. shares electricity expenditure = energy expenditure share of GDP
# source: http://199.36.140.204/todayinenergy/detail.cfm?id=18471
# alpha <- c(0.95)
# update: calibrate as Hawaii GDP 2014=76,588 million USD souce:https://www.bea.gov/
# AEI 826 Total annual electricity usage 2014 = $ 2,134,093,685.00 from https://www.dropbox.com/s/x4vr57hcgq9j55c/f8262014.xls?dl=0
# Hawaii Electric Co Inc. monthly alpha is calculated = 1-monthly utility revenue/(Hawaii GDP  )
# monthly alpha = 2.82%
 	
alpha <- c(0.9718)
# alpha <- 0.0282

#alpha <- c(0.9727, 0.9744, 0.9720, 0.9735, 0.9711, 0.9716, 0.9698, 0.9693, 0.9693, 0.9696, 0.9741, 0.9744)
# 2. elasticity of substitution between electricity and other goods
# following Gowrisankaran et al 2016
theta <- c(0.1)

print(sprintf('using alpha=%f, theta=%f', alpha, theta))

#assumption on interhourly substitution (highly shiftable, slighly shiftable, no shiftable)
sigmap <<- c(10, 1.000001, 0.1) 
# slightly shiftbale is not exactly 1 because if it is 1 the results turn to be NA 
# there is exponential term that is divided by 0

# the next line is not needed and doesn't work on computers with different directory structures
# but needed if we want to test the R code without running SWITCH
# (or you can just set the working directory before running R or in RStudio)
#setwd("~/Dropbox/demand_system") # for mac
#setwd("C:/users/new/Dropbox/demand_system") # only for Imelda's laptop

#load the monthly hourly flexible shares
flexshares <- read.csv(file="flexshares.csv", header=TRUE, sep=",")

#scenario latest update
#                           sigma	Optimistic	Moderate	Pessimistic
# Share of flexible load
# Highly Flexible	          10	    67%	        33%	      15%
# Somewhat Flexible	        1	       5%	        5%	      5%
# Highly Inflexible	        0.1   	28%	        62%	      80%
# read.csv(file = "flexshares.csv", header = TRUE, stringsAsFactors = FALSE)

# Share of other load
# Highly Flexible	          10	      15%	      8%	      0%
# Somewhat Flexible	        1	        5%	      5%	      5%
# Highly Inflexible	        0.1	      80%	      88%	      95%

set.scenario <- function(scen) {
  if(scen==1) {#optimistic
    shareflex <<- c(0.67, 0.05, 0.28) #sigma level - highly flex, midflex, inflex
    shareother <<- c(0.15, 0.05, 0.8)
  } else if(scen==2) {#2nd scenario=baseline/moderate
    shareflex <<- c(0.33, 0.05, 0.62)
    shareother <<- c(0.075, 0.05, 0.875)
  } else if(scen==3) {#3rd scenario=pessimistic
    shareflex <<- c(0.15, 0.05, 0.8)
    shareother <<- c(0, 0.05, 0.95)
  } else {
    stop("Invalid elasticity scenario selected (must be 1-3)")
  }
}

print("loaded share parameters for each scenario")

#####################################################################
betaflexf <- function(xb, sigmath, month, scen) { #sigmath=1:highlyflex, 2:midflex, 3:inflex
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare <- xb*(flexload+otherload)
  betas <- loadshare/sum(loadshare)
  return(betas)
}
print("loaded beta function")
#beta across groups are the same 
# (betaflexf(b.loads, 3,1,2))
# (betaflexf(b.loads, 1,1,2))
# (betaflexf(b.loads, 2,1,2))

demandNestCES <- function(pd, xb, pb, parms, alpha) { 
  # pd         = price vector (main argument) 
  # xb and pb =  baseline loads and price used in calibration
  # parms[1]  = theta, overal demand elasticity
  # parms[2]  = sigma, interhour demand elasticity
  # alpha     = baseline share of income spent on electricity 

  #set parameters
  p      <- pd/pb
  Mb     <- sum(pb*xb)/(1-alpha)
  beta   <- xb/sum(xb)
  theta  <- parms[1]
  sigma  <- parms[2]   
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- (sigma-theta)/(1-sigma)
  exp3 <- sum(beta*(p^(1-sigma)))
  e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
  x <- (Mb* e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma)))
  return(x/pb)
}

demandbygroupf <- function( xb, pd, pb, sigmath, month, scen ) { 
  set.scenario(scen)
  flexload      <- flexshares[,month]*shareflex[sigmath]
  otherload     <- (1-flexshares[,month])*shareother[sigmath]
  loadshare     <- xb*(flexload+otherload)

  theta         <- theta # set globally
  sigma         <- sigmap[sigmath]
  alpha         <- alpha # set globally
  
  return (demandNestCES(pd, loadshare, pb, parms=c(theta, sigma), alpha=alpha))
}

# demand
demandtotf <- function(xb, pd, pb, month, scen){
  tot <- 0
  for (sigmath in (1:3)) {
    tot <- tot + demandbygroupf(xb, pd, pb, sigmath, month, scen)
  }
  return(tot)
}
print("loaded demand functions")

x <- function(p) {
  # 10, 1.00001 or 0.1
  sigma <- 10
  demandNestCES(p, rep(1000, length(p)), rep(180, length(p)), parms=c(theta, sigma), alpha=alpha )
}

integrand <- function(t, p0, p1) {
  p <- t * p1 + (1-t) * p0
  x <- x(p)
  return (sum(x * (p1-p0)))
}

wtp <- function(p) {
  p0 = 180+0*p # arbitrary baseline prices
  p1 = p
  wtp = -integrate(Vectorize(integrand, vectorize.args=c('t')), 0, 1, p0, p1)$value
  return (wtp)
}


#####################################################################

#willingness to pay function total
wtpbygroupf.old <- function(xd, xb, pd, pb, sigmath, month, scen) { 
  #pb = price at baseline; pd=new price; Mb=budget at baseline; scen=which scenario
  #sigmath:for each group highlyflex=1; midflex=2; inflex=3
  #calculate load shares that are flexible
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  #set parameters
  p             <- pd/pb
  Mb            <- sum(pb*loadshare)/(1-alpha)
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- 1/(1-theta)
  exp3 <- sum(beta*(p^(1-sigma)))
  e <- ((alpha+(1-alpha)*(exp3^(exp1)))^exp2)
  cs  <- Mb/e
  cost <- sum(pd*xd)
  return(cs+cost)
}

wtpbygroupf <- function(xb,samplep, pd,pb,sigmath,month,scen) {
  wtp = 0
  n = nrow(samplep)
  for (i in (1:n)) wtp = wtp + sum( samplep[i,]*demandbygroupf(xb,samplep[i,],pb,sigmath,month,scen) )/n
  wtp = wtp + sum( pd*demandbygroupf(xb,pd,pb,sigmath,month,scen))
  return( wtp )
}
csbygroupf <- function(xd, xb, pd, pb, sigmath, month, scen) { 
  #pb = price at baseline; pd=new price; Mb=budget at baseline; scen=which scenario
  #sigmath:for each group highlyflex=1; midflex=2; inflex=3
  #calculate load shares that are flexible
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  #set parameters
  p             <- pd/pb
  Mb            <- sum(pb*loadshare)/(1-alpha)
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- 1/(1-theta)
  exp3 <- sum(beta*(p^(1-sigma)))
  e <- ((alpha+(1-alpha)*(exp3^(exp1)))^exp2)
  cs  <- Mb/e
  return(cs)
}

utilityf <- function(xd, xb, pd, pb, sigmath, month, scen) {
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  #set parameters
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  
  yb <- sum(xb*pb)*(alpha/(1-alpha))
  Mb <- yb + sum(xb*pb)
  yd <- Mb-sum(xd*pd)
  
  exp4          <- beta/(xd*pb)
  p             <- (Mb-yd)/(((exp4)^(-1/sigma))*sum(((exp4)^(1/sigma))*(pb*xd)))
  
  
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- 1/(1-theta)
  exp3 <- sum(beta*(p^(1-sigma)))
  u <- ((alpha+(1-alpha)*(exp3^(exp1)))^exp2)
  return(Mb/u)
}

calibrate <- function(base.loads, base.prices, scenario) {
  # force strictly positive prices (never really an issue with base.prices)
  #base.prices[base.prices < 1] <- 1
  #base.prices <- base.prices + 10
  #base.prices[base.prices < 1] <- 5
  # store base values for future reference
  base.loads <<- base.loads
  base.prices <<- base.prices
  # we don't currently receive baselines for reserves, but they are useful
  # when integrating from base level to final level
  base.qup <<- 0 * base.loads
  base.qdown <<- base.qup
  base.pup <<- 0 * base.prices
  base.pdown <<- base.pup
  
  #set.scenario(scenario, month)
  # make an empty array for the beta values
  # b <<- array(numeric(length(base.loads)), dim(base.loads), dimnames=dimnames(base.loads))
  # b <<- 0.0 * base.loads
  # fill in the beta values, column by column (there should be some way to do this with
  # apply, mapply or similar, but it's not obvious how)
  # dims = dim(base.loads)
  #for (ts in 1:dims[2]) 
    #for (lo in 1:dims[3])
      #b[,ts,lo] <<- betaf(base.loads[,ts,lo])
}

bid <- function(location, timeseries, prices, pup, pdown, scenario) {
  # we don't have a good way to know the month, but this works for 
  # inputs_tiny and inputs_2045_15
  if (nchar(timeseries) == 4) {   # tiny dataset
    month <- as.numeric(substr(timeseries, 3, 4))
  } else {                      # normal dataset
    month <- as.numeric(substr(timeseries, 5, 6))    
  }

  p0 = list(
    base.prices[, timeseries, location], 
    base.pup[, timeseries, location], 
    base.pdown[, timeseries, location]
  )
  p1 = list(prices, pup, pdown)
  
  q0 = list(
    base.loads[, timeseries, location], 
    base.qup[, timeseries, location], 
    base.qdown[, timeseries, location]
  )
  q1 = totaldemand(location, timeseries, p1[[1]], p1[[2]], p1[[3]], month, scenario)

  # if (sum(p1[[1]]) == 0) browser()

  iqp = integrate(
    Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1, 
    p0, p1, location, timeseries, month, scenario)$value  
    
  # q0.p0 + iqp + ipq = q1.p1, so
  # ipq = q1.p1 - q0.p0 - iqp (at least in 2d)
  wtp = q.dot.p(q1, p1) - q.dot.p(q0, p0) - iqp
  # wtp = -iqp  # q.dot.p terms drop out when including "all other goods"

  return (c(q1, wtp))
}

totaldemand <- function(location, timeseries, prices, pup, pdown, month, scenario) {
  b.loads = base.loads[, timeseries, location]
  b.prices = base.prices[, timeseries, location]

  # calculate the highest and lowest loads that could occur in each hour, 
  # holding prices constant in other hours
  demandflexmin <- vector('numeric', length(prices))
  demandflexmax <- vector('numeric', length(prices))
  testprices <- prices
  testprices[testprices < 1] <- 1
  for (i in (1:length(prices))) {
    # could find the "realistic" lower limit to demandflex as follows, but probably not necessary:
    # testprices[i] <- max(100 * prices[i], 100 * b.prices[i]) # must be large compared to current or baseline prices
    # test <- demandfflex(b[,timeseries,location], testprices, b.prices, Mb, month, scenario)/b.prices
    # demandflexmin[i] = test[i]
    demandflexmin[i] <- 0
    old_price <- testprices[i]
    testprices[i] <- 1
    test <- demandbygroupf(b.loads, testprices, b.prices, 1, month, scenario)
    demandflexmax[i] = test[i]
    testprices[i] <- old_price
  }
  # adjust prices and budget to reflect net price of electricity consumption
  # note: qup = -(qflex-qflexmin), qdown = -(qflexmax-qflex), 
  # so net cost to customer will be
  # qother*prices + qflex*prices + qup*pup + qdown*pdown
  # = qother*prices + qflex*prices - (qflex-qflexmin)*pup - (qflexmax-qflex)*pdown
  # = qother*prices + qflex * (prices - pup + pdown) + qflexmin*pup - qflexmax*pdown
  # so we adjust budget and prices accordingly.
  # ---> Is this the correct treatment (especially the budget adjustment)?
  # ---> How should this be factored into the WTP function?
  netprices <- (prices - pup + pdown)

  # set floors on prices
  netprices[netprices < 1] <- 1
  prices[prices < 1] <- 1

  # find demand levels
  demandflex = demandbygroupf(b.loads, netprices, b.prices, 1, month, scenario)
  demandother = (
    demandbygroupf(b.loads, prices, b.prices, 2, month, scenario) 
    + demandbygroupf(b.loads, prices, b.prices, 3, month, scenario)
  )

  demand = demandflex + demandother
  # note: reserve quantities are negative to indicate sales from the demand side
  demandup = -(demandflex - demandflexmin)
  demanddown = -(demandflexmax - demandflex)
  
  return (list(demand, demandup, demanddown))
}

lmerge <- function (func, a, b) {
  # combine elements of lists a and b, using function func
  # works like mapply, but retains list structure
  list_len <- length(a)
  if (list_len != length(b))
    stop("merge.lists was called with two lists of different lengths")
  the_func <- match.fun(func)
  out <- vector("list", list_len)
  for (i in 1:list_len)
    out[[i]] <- the_func(a[[i]], b[[i]])
  return (out)
}

q.dot.p <- function(q, p) {
  # calculate dot products for each of the components of the q and p lists
  # (energy, up reserves and down reserves), then add them together. 
  # This acts like one dot product across all energy and reserve products.
  return (Reduce("+", lapply(lmerge("*", q, p), sum)))
}

q.dot.p.for.t <- function(t, p0, p1, location, timeseries, month, scenario) {
  # calculate demand * (p1-p0) at a price equal to t*p1 + (1-t)*p0
  # This can be integrated for t running from 0 to 1 to calculate
  # the line integral of q.dp from p0 to p1.

  # make a list of prices for energy and reserve products
  p <- lmerge("+", lapply(p1, "*", t), lapply(p0, "*", 1-t))
  # get demand at these prices
  q <- totaldemand(location, timeseries, p[[1]], p[[2]], p[[3]], month, scenario)
  # calculate p1-p0 for the line integral
  p_diff <- lmerge("-", p1, p0)
  # calculate dot product of q and p_diff
  return (q.dot.p(q, p_diff))
}

get_extra_column_names <- function(){
  return(list("cshighflex", "csmidflex", "csinflex"))
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


#####################################################################
#                  BELOW ARE FOR TESTING      
#####################################################################

test_calib <- function () {
	base.loads <- 1000 * array(rep(1, 5), dim=c(5,1,1), dimnames=list(c(12, 13, 14, 15, 16), c(100), c("oahu")))
	base.prices <- 0.0*base.loads+180
	print(base.prices)
	calibrate(base.loads, base.prices, 1)
	# print_calib()
}

print_calib <- function () {
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

	base.loads <- base.l * array(rep(1, 24), dim=c(24,1,1), dimnames=list(rep(1,24), c(100), c("oahu")))
	base.prices <- 0.0*base.loads+180
	base.prices[,1,1]
	bid(1, 20070115, base.p, base.p+10, base.p-10, 3) #wtp=54,364,398
	#2nd attempt : [1] 80301997
	
  # [[5]]
  # [1] 1054904
  #
  # [[6]]
  # [1] 79247093
  #
  # bid(1, 20070115, base.p*0.5, base.p+10, base.p-10, 3) #wtp=52,035,222
  # #2nd attempt :  [1] 77599278
  #
  # [[5]]
  # [1] 804174.9
  #
  # [[6]]
  # [1] 76795103
	
	bid(1, 20070115, base.p, base.p+10, base.p-10, 1)
	#2nd attempt : [1] 41,931,893 6,469,177 35,462,715
	
	bid(1, 20070115, base.p*0.5, base.p+10, base.p-10, 1)
	#2nd attempt :	[1] 39,126,836 5,036,481 34,090,355
}

