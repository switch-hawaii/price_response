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
set.scenario(1,5)

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
    #print(paste(scen,month,startrow, sigmarow,sigma, Mbsf,Mbsot,x))
  }
  return(x1)
}
#test
#(demandf(betas, c(base.p[1:5]*2,base.p[6:24]), base.p, M,1,1)/base.p)-(demandf(betas, c(base.p[1:5]*2,base.p[6:24]), base.p, M,1,2)/base.p) #shoould be 0

demandfflex <- function(beta, pd, pb, Mb, month, scen) {
  x1 <- 0
  p <- pd/pb
  set.scenario(scen,month)
  for (i in (1:3)) {
    ifelse (i==1, startrow <- 1, startrow<- ((i-1)*24)+1)
    sigmarow <- i*24
    Mbsf <- paramf[startrow:sigmarow,c("type")]*paramf[startrow:sigmarow,c("share")]*Mb
    sigma <- paramf[sigmarow,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- (sigma-theta)/(1-sigma)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
    x <- (Mbsf * e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma)))
    x1 <- x +x1
    #print(paste(scen,month,startrow, sigmarow,sigma, Mbsf,Mbsot,x))
  }
  return(x1)
}

demandfother <- function(beta, pd, pb, Mb, month, scen) {
  x1 <- 0
  p <- pd/pb
  set.scenario(scen,month)
  for (i in (1:3)) {
    ifelse (i==1, startrow <- 1, startrow<- ((i-1)*24)+1)
    sigmarow <- i*24
    Mbsot <- paramot[startrow:sigmarow,c("type")]*paramot[startrow:sigmarow,c("share")]*Mb
    sigma <- paramf[sigmarow,c("sigma")]
    exp1 <- (1-theta)/(1-sigma)
    exp2 <- (sigma-theta)/(1-sigma)
    exp3 <- sum(beta*(p^(1-sigma)))
    e <- ((alpha+(1-alpha)*(exp3^(exp1)))^(-1))
    x <- (Mbsot * e *((1-alpha)*(exp3)^exp2)*beta*(p^(-1*sigma)))
    x1 <- x +x1
    #print(paste(scen,month,startrow, sigmarow,sigma, Mbsf,Mbsot,x))
  }
  return(x1)
}

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
return(mean(m1)+sum(pd*xb))
}
#test
#wtpf(betas, base.p*0.5, base.p, base.l*20, base.l,M,8,3)-wtpf(betas, base.p*0.5, base.p, base.l*20, base.l,M,8,2) #should be 0

wtpfflex <- function(beta, pd, pb, xd, xb, Mb, month, scen) { #b=baseline, d=newdata 
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
    m <- ((paramf[startrow:sigmarow,c("type")]*paramf[startrow:sigmarow,c("share")]*Mb)/e)
    m1 <- m + m1
    #print(paste(exp1, exp2, exp3, sigma, e, m, m1))
  }
  return(mean(m1)+sum(pd*xb))
}

wtpfother <- function(beta, pd, pb, xd, xb, Mb, month, scen) { #b=baseline, d=newdata 
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
    m <- ((paramot[startrow:sigmarow,c("type")]*paramot[startrow:sigmarow,c("share")]*Mb/e))
    m1 <- m + m1
    #print(paste(exp1, exp2, exp3, sigma, e, m, m1))
  }
  return(mean(m1)+sum(pd*xb))
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
  for (ts in 1:dims[2]) 
    for (lo in 1:dims[3])
      b[,ts,lo] <<- betaf(base.loads[,ts,lo])
}

bid <- function(location, timeseries, prices, pup, pdown, scenario) {
  # we don't have a good way to know the month, but this works for inputs_tiny and inputs_2045_15
  if (nchar(timeseries) == 4) {   # tiny dataset
    month <- as.numeric(substr(timeseries, 3, 4))
  } else {                      # normal dataset
    month <- as.numeric(substr(timeseries, 5, 6))    
  }
  # force strictly positive prices
  #prices[prices < 15] <- 15
  #prices <- prices + 10
  prices[prices < 1] <- 1
  b.loads = base.loads[,timeseries,location]
  b.prices = base.prices[,timeseries,location] 
  Mb <- sum(b.loads*b.prices)/(1-alpha)
  
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
    test <- demandfflex(b[,timeseries,location], testprices, b.prices, Mb, month, scenario)/b.prices
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
  Mbnew <- Mb + sum(demandflexmax*pdown - demandflexmin*pup)  # should cancel against net kWh prices
  netprices <- (prices - pup + pdown)
  # force strictly positive
  netprices[netprices < 1] <- 1
  demandflex = demandfflex(b[,timeseries,location], netprices, b.prices, Mb, month, scenario)/b.prices
  
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

  demandother = demandfother(b[,timeseries,location], prices, b.prices, Mb, month, scenario)/b.prices

  demand = demandflex + demandother
  # note: reserve quantities are negative to indicate sales from the demand side
  demandup = -(demandflex - demandflexmin)
  demanddown = -(demandflexmax - demandflex)
  
  wtpflex<-wtpfflex(b[,timeseries,location], netprices, b.prices, demandflex, b.loads, Mb, month, scenario)
  wtpother<-wtpfother(b[,timeseries,location], prices, b.prices, demandother, b.loads, Mb, month, scenario)
  wtp = wtpflex + wtpother
  
  # if (anyNA(demand)) {
  #   browser()
  # }
  return (list(demand, demandup, demanddown, wtp))
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
	print(demandf(betas, 1, base.p, M, 1, 1)/base.p)
	print(demandfqmax(betas, base.p, base.p, M, 1, 1)/base.p)
	(demandfqmax(betas, 1, base.p, M, 1, 1)/base.p)-(demandfqmax(betas, base.p, base.p, M, 1, 1)/base.p)

	#bid quantity as if no reserves
	#max q calculated at min price=1
	qstar = demandfflex(betas, base.p, base.p, M, 1, 1)/base.p
	#calculate max quantity at price=1 on the baseline load
	qmax = demandfflex(betas, 1, base.p, M, 1, 1)/base.p
	
	# up-reserves or load down
	qup = -qstar 
	#down-reserves or load up 
	qdown = -(qmax - qstar) 
	
	
	pup <- 100
	pdown <- 100
	#new budget accounts for reserve
	Mbnew <- (sum(base.l*base.p) + sum(qup * pup) + sum(qdown * pdown)) /(1-alpha)
	demandflex = demandfflex(betas, base.p , base.p, Mbnew, 1, 1)/base.p
	demandother = demandfother(betas, base.p , base.p, M, 1, 1)/base.p
	demand = demandflex + demandother
	demandup = demandflex
	demanddown = qmax - demandflex
	
	#demand[demand >3000] <- 3000
	#demand[is.nan(demand)] <- 3000
	#prices1 <- prices
	#prices1[prices1 < 5] <- 5
	wtp = wtpf(b[,timeseries,location], prices, base.p, demand, base.l, Mb, month, scenario)
}

reservetest <- function(prices, pup, pdown, scenario) {
  b.loads <-c(718.0014, 680.5867, 662.7549, 665.3259, 706.5587, 796.7969, 890.2706, 962.2579, 1032.4267, 
             1074.0698, 1093.5465, 1099.8633, 1103.0553, 1100.2026, 1097.8984,
             1095.9867, 1088.0893, 1091.6672, 1111.0894, 1094.1057, 1042.7211, 969.1186, 871.5261, 778.1248)
  #2. base price
  b.prices <- rep(180, 24)
  
  #3. total money each agent has
  Mb <- sum(b.prices*b.loads)/(1-alpha)
  
  #4. the calibrated beta is the share of expenditure
  betas <- b.loads/sum(b.loads)
  
  #reset the parameters
  theta <- 0.1
  alpha <- 0.9718

  sigmap <<- c(10, 1.000001, 0.1)
  setwd("C:/Users/new/Google Drive/hpc-switch/outputs_monthlyvariedp1v2")
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
	  test <- demandfflex(betas, testprices, b.prices, Mb, 1, 1)/b.prices
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
	Mbnew <- Mb + sum(demandflexmax*pdown - demandflexmin*pup)  # should cancel against net kWh prices
	netprices <- (prices - pup + pdown)
	# force strictly positive
	netprices[netprices < 1] <- 1
	demandflex = demandfflex(betas, netprices, b.prices, Mb, 1,1)/b.prices
	
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
	
	demandother = demandfother(betas, prices, b.prices, Mb, 1,1)/b.prices
	
	demand = demandflex + demandother
	# note: reserve quantities are negative to indicate sales from the demand side
	demandup = -(demandflex - demandflexmin)
	demanddown = -(demandflexmax - demandflex)
	
	#demand[demand >3000] <- 3000
	#demand[is.nan(demand)] <- 3000
	#prices1 <- prices
	#prices1[prices1 < 5] <- 5
	wtpflex <- wtpfflex(betas, netprices, b.prices, demandflex, b.loads, Mb, 1,1)
	wtpother <- wtpfother(betas, prices, b.prices, demandother, b.loads, Mb, 1,1)
	wtp = wtpflex + wtpother
	demandwithoutreserves <- demandf(betas, prices, b.prices, Mb, 1,1)/b.prices
	print(paste("diffdemandwithandwithoutreserves",sum(demand-demandwithoutreserves)))
	print(paste("WTPwithoutreserves",wtpf(betas, prices, b.prices, demandwithoutreseves, b.loads, Mb, 1,1)))
	#print(paste("netprices",netprices," demandflex",demandflex))
	print(paste("wtpother",wtpother,"wtpflex",wtpflex))
	return(wtp)
}

# reservetest(b.prices, 100, 100, 1)
# reservetest(b.prices, 0, 100, 1)
# reservetest(b.prices, 90, 20, 1)
# reservetest(b.prices, 0, 0, 1)
# reservetest(b.prices, 200, 200, 1)
# wtpf(betas, b.prices, b.prices, b.loads, b.loads, Mb, 1,1)
