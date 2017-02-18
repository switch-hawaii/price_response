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
 	
alpha <- c(0.9718) #old static alpha
#alpha <- c(0.9727, 0.9744, 0.9720, 0.9735, 0.9711, 0.9716, 0.9698, 0.9693, 0.9693, 0.9696, 0.9741, 0.9744)
# 2. elasticity of subtitution between electricity and other goods
# following Gowrisankaran et al 2016
theta <- c(0.1)

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
# 
# Share of other load
# Highly Flexible	          10	      15%	      8%	      0%
# Somewhat Flexible	        1	        5%	      5%	      5%
# Highly Inflexible	        0.1	      80%	      88%	      95%

set.scenario <- function(scen) {
  ifelse(scen==1, {#optimistic
    shareflex <<- c(0.67, 0.05, 0.28) #sigma level - highly flex, midflex, inflex
    shareother <<- c(0.15, 0.05, 0.8)},
    ifelse(scen==2, 
           {#2nd scenario=baseline/moderate
             shareflex <<- c(0.33, 0.05, 0.62)
             shareother <<- c(0.075, 0.05, 0.875)},
           ifelse(scen==3,
                  {#3rd scenario=pessimistic
                    shareflex <<- c(0.15, 0.05, 0.8)
                    shareother <<- c(0, 0.05, 0.95)},
                  print("Scenario is not available"))))
}

for (i in 1:(ncol(flexshares))){
  ifelse(i==1, flexsharesall <- data.frame(t(rbind(hour=seq(1,24), rbind(month=rep(i,24),flexshare=flexshares[,i])))), 
         flexsharesall <- data.frame(rbind(flexsharesall, t(rbind(hour=seq(1,24), rbind(month=rep(i,24),flexshare=flexshares[,i])))))) 
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

demandbygroupf <- function(xb, pd, pb, sigmath, month, scen) { 
  #pb = price at baseline; pd=new price; Mb=budget at baseline; scen=which scenario
  #sigmath:for each group highlyflex=1; midflex=2; inflex=3
  #calculate load shares that are flexible
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  #set parameters
  p             <- pd/pb
  alpha         <- alphamonthly[month]
  Mb            <- sum(pb*loadshare)/(1-alpha)
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- (sigma-theta)/(1-sigma)
  exp3 <- sum(beta*(p^(1-sigma)))
  e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
  x <- (Mb* e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma)))
  return(x/pb)
}

demandtotf <- function(xb, pd, pb, month, scen){
  tot <- 0
  for (i in (1:3)) {
    ifelse(i==1, tot <- demandbygroupf(xb, pd, pb, i,month,scen),
           tot <- tot + demandbygroupf(xb, pd, pb, i,month,scen))
  }
  return(tot)
}
print("loaded demand functions")


#####################################################################

#willingness to pay function total
wtpbygroupf <- function(xd, xb, pd, pb, sigmath, month, scen) { 
  #pb = price at baseline; pd=new price; Mb=budget at baseline; scen=which scenario
  #sigmath:for each group highlyflex=1; midflex=2; inflex=3
  #calculate load shares that are flexible
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  #set parameters
  p             <- pd/pb
  alpha         <- alphamonthly[month]
  Mb            <- sum(pb*loadshare)/(1-alpha)
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- (sigma-theta)/(1-sigma)
  exp3 <- sum(beta*(p^(1-sigma)))
  e   <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
  cs  <- Mb/e
  cost <- sum(pd*xd)
  return(cs+cost)
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
  alpha         <- alphamonthly[month]
  Mb            <- sum(pb*loadshare)/(1-alpha)
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  #expressions to construct function
  exp1 <- (1-theta)/(1-sigma)
  exp2 <- (sigma-theta)/(1-sigma)
  exp3 <- sum(beta*(p^(1-sigma)))
  e   <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
  cs  <- Mb/e
  return(cs)
}


calibrate <- function(base.loads, base.prices, scenario) {
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
  #for (ts in 1:dims[2]) 
    #for (lo in 1:dims[3])
      #b[,ts,lo] <<- betaf(base.loads[,ts,lo])
}

bid <- function(location, timeseries, prices, pup, pdown, scenario) {
  # we don't have a good way to know the month, but this works for inputs_tiny and inputs_2045_15
  if (nchar(timeseries) == 4) {   # tiny dataset
    month <- as.numeric(substr(timeseries, 3, 4))
  } else {                      # normal dataset
    month <- as.numeric(substr(timeseries, 5, 6))    
  }
  # force strictly positive prices
  prices[prices < 1] <- 1
  b.loads = base.loads[,timeseries,location]
  b.prices = base.prices[,timeseries,location] 

  # M.F. disabled new budget calculation 2016-12-16 because it seems like
  # budget should be based on baseline conditions (in which customers cannot
  # sell reserves) and should not vary from bid to bid

  # ##bid quantity as if no reserves only for flexible shares
  # #max q calculated at min price=1
  # qstar = demandfflex(b[,timeseries,location], prices, b.prices, Mb, month, scenario)/b.prices
  # #calculate max quantity at price=1 on the baseline load
  # qmax = demandfflex(b[,timeseries,location], 1, b.prices, Mb, month, scenario)/b.prices
  #
  # # up-reserves or load down: q purchased (sold)
  # qup = -qstar
  # #down-reserves or load up: q purchased (sold)
  # qdown = -(qmax - qstar)
  #
  # #new budget accounts for reserve
  # Mbnew <- (sum(b.loads*b.prices) + sum(qup * pup) + sum(qdown * pdown)) /(1-alpha)

  # M.F. added this 2016-12-16
  # calculate the highest and lowest loads that could occur in each hour, 
  # holding prices constant in other hours
  demandflexmin <- vector('numeric', length(prices))
  demandflexmax <- vector('numeric', length(prices))
  testprices <- prices
  for (i in (1:length(prices))) {
    # could find the "realistic" lower limit to demandflex as follows, but probably not necessary:
    # testprices[i] <- max(100 * prices[i], 100 * b.prices[i]) # must be large compared to current or baseline prices
    # test <- demandfflex(b[,timeseries,location], testprices, b.prices, Mb, month, scenario)/b.prices
    # demandflexmin[i] = test[i]
    demandflexmin[i] <- 0
    testprices[i] <- 1
    test <- demandbygroupf(b.loads, testprices, b.prices, 1, month, scenario)
    demandflexmax[i] = test[i]
    testprices[i] <- prices[i]
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
  # force strictly positive
  netprices[netprices < 1] <- 1
  demandflex = demandbygroupf(b.loads, netprices, b.prices, 1, month, scenario)
  
  # note: MF replaced code below with code above 2016-12-16
  # # note from MF: shouldn't reserve prices be factored into the net prices for energy consumption?
  # # i.e., if one MWh is purchased, that also allows sale of one MWh of "energy up" and prevents sale
  # # of one MWh of "energy down" reserves.
  # demandflex = demandfflex(b[,timeseries,location], prices, b.prices, Mbnew, month, scenario)/b.prices
  # # note from MF: demandflexmax should probably be calculated separately for each hour by holding other
  # # prices constant and setting that hour's price to 1; this will find the amount of flexible load that
  # # can shift to that particular hour if needed (but it's not very important because down-reserves are
  # # not usually very valuable)
  # demandflexmax = demandfflex(b[,timeseries,location], 1, b.prices, Mbnew, month, scenario)/b.prices

  # sum of midflex and inflex group
  demandother = demandbygroupf(b.loads, prices, b.prices, 2, month, scenario) + demandbygroupf(b.loads, prices, b.prices, 3, month, scenario) 

  demand = demandflex + demandother
  # note: reserve quantities are negative to indicate sales from the demand side
  demandup = -(demandflex - demandflexmin)
  demanddown = -(demandflexmax - demandflex)
  
  wtpflex   <- wtpbygroupf(demandflex, b.loads, netprices, b.prices, 1, month, scenario)
  wtpother  <- wtpbygroupf(demandother, b.loads, prices, b.prices, 2, month, scenario) + wtpbygroupf(demandother, b.loads, prices, b.prices, 3, month, scenario)
  wtp = wtpflex + wtpother
  
  print(sum(demandflex*netprices))
  print(sum(demandother*prices))
  cshighflex <- csbygroupf(demandflex, b.loads, netprices, b.prices, 1, month, scenario)
  csmidflex <- csbygroupf(demandother, b.loads, prices, b.prices, 2, month, scenario)
  csinflex <- csbygroupf(demandother, b.loads, prices, b.prices, 3, month, scenario)
  
  # if (anyNA(demand)) {
  #   browser()
  # }
  return (list(demand, demandup, demanddown, wtp, cshighflex, csmidflex, csinflex))
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

	base.loads <- 1000 * array(rep(1, 5), dim=c(5,1,1), dimnames=list(c(12, 13, 14, 15, 16), c(100), c("oahu")))
	base.prices <- 0.0*base.loads-180
	base.prices[,1,1]
	bid(1, 20070115, base.p, base.p-10, base.p-10, 3)
}

