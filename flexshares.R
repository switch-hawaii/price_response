library(ggplot2, quietly=TRUE)
library(reshape2, quietly=TRUE)
library(Hmisc, quietly=TRUE) #for number of days in a month
library(stringr, quietly=TRUE) #str_to_title
library(plyr) #applying function to each group

alpha <- c(0.9718)
#alphamonthly <- c(0.9727, 0.9744, 0.9720, 0.9735, 0.9711, 0.9716, 0.9698, 0.9693, 0.9693, 0.9696, 0.9741, 0.9744)
theta <- c(0.1)
sigmap <<- c(10, 1.000001, 0.1)

flexshares <- read.csv(file="flexshares.csv", header=TRUE, sep=",")

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

# for (i in 1:(ncol(flexshares))){


#   ifelse(i==1, flexsharesall <- data.frame(t(rbind(hour=seq(1,24), rbind(month=rep(i,24),flexshare=flexshares[,i])))), 
#          flexsharesall <- data.frame(rbind(flexsharesall, t(rbind(hour=seq(1,24), rbind(month=rep(i,24),flexshare=flexshares[,i])))))) 
# }
print("loaded share parameters for each scenario")

# b.loads <-c(718.0014, 680.5867, 662.7549, 665.3259, 706.5587, 796.7969, 890.2706, 962.2579, 1032.4267, 
#             1074.0698, 1093.5465, 1099.8633, 1103.0553, 1100.2026, 1097.8984,
#             1095.9867, 1088.0893, 1091.6672, 1111.0894, 1094.1057, 1042.7211, 969.1186, 871.5261, 778.1248)
# #2. base price
# b.prices <- rep(180, 24)


betaflexf <- function(xb, sigmath, month, scen) { #sigmath=1:highlyflex, 2:midflex, 3:inflex
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare <- xb*(flexload+otherload)
  betas <- loadshare/sum(loadshare)
  return(betas)
}
print("loaded beta function")


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

demandbygroupf.ces <- function(xb, pd, pb, sigmath, month, scen) { 
  #pb = price at baseline; pd=new price; Mb=budget at baseline; scen=which scenario
  #sigmath:for each group highlyflex=1; midflex=2; inflex=3
  #calculate load shares that are flexible
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  #set parameters
  p             <- pd/pb
  Mb            <- sum(pb*loadshare)
  beta          <- betaflexf(xb, sigmath, month, scen)
  sigma         <- sigmap[sigmath]
  #expressions to construct function
  e <- (sum(beta*(p^(1-sigma))))^(1/(1-sigma))
  v <- Mb/e
  x <- loadshare*(v/Mb)*((e/p)^sigma)
  return(x)
}
# scen=1
# sum(demandbygroupf.ces(b.loads, b.prices, b.prices, 1,1,scen)+ 
#       demandbygroupf.ces(b.loads, b.prices, b.prices, 2,1,scen)+
#       demandbygroupf.ces(b.loads, b.prices, b.prices, 3,1,scen)-b.loads)

demandtotf <- function(xb, pd, pb, month, scen){
  tot <- 0
  for (i in (1:3)) {
    ifelse(i==1, tot <- demandbygroupf(xb, pd, pb, i,month,scen),
           tot <- tot + demandbygroupf(xb, pd, pb, i,month,scen))
  }
  return(tot)
}
print("loaded demand function")

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
  #print(paste("beta:",beta,"e:",e,"cs:",cs,"cost:",cost))
  return(cs+cost)
}

csbygroupf <- function(xb, pd, pb, sigmath, month, scen) { 
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

csbygroupf.ces <- function(xb, pd, pb, sigmath, month, scen) { 
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
  e <- (sum(beta*(p^(1-sigma))))^(1/(1-sigma))
  v <- Mb/e
  return(v)
}


wtpbygroupf.ces <- function(xd, xb, pd, pb, sigmath, month, scen) { 
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
  e <- (sum(beta*(p^(1-sigma))))^(1/(1-sigma))
  v <- Mb/e
  cost <- sum(pd*xd)
  return(v+cost)
}


baseloadbygf <- function(xb, sigmath, month, scen) { 
  #pb = price at baseline; pd=new price; Mb=budget at baseline; scen=which scenario
  #sigmath:for each group highlyflex=1; midflex=2; inflex=3
  #calculate load shares that are flexible
  set.scenario(scen)
  flexload <- flexshares[,month]*shareflex[sigmath]
  otherload <- (1-flexshares[,month])*shareother[sigmath]
  loadshare<- xb*(flexload+otherload)
  return(loadshare)
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

q.dot.p <- function(q, p) {
  # calculate dot products for each of the components of the q and p lists
  # (energy, up reserves and down reserves), then add them together. 
  # This acts like one dot product across all energy and reserve products.
  return (Reduce("+", lapply(lmerge("*", q, p), sum)))
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

q.dot.p.for.t <- function(t, p0, p1, b.loads, b.prices, sigmath, month, scenario) {
  # calculate demand * (p1-p0) at a price equal to t*p1 + (1-t)*p0
  # This can be integrated for t running from 0 to 1 to calculate
  # the line integral of q.dp from p0 to p1.

    # make a list of prices for energy and reserve products
  p <- lmerge("+", lapply(p1, "*", t), lapply(p0, "*", 1-t))
  # get demand at these prices
  q <- demandbytype(b.loads, b.prices, p[[1]], p[[2]], p[[3]], sigmath, month, scenario)
  #print(paste(sum(q[[1]]),"sigma=",sigmath))
  # calculate p1-p0 for the line integral
  p_diff <- lmerge("-", p1, p0)
  # calculate dot product of q and p_diff
  return (q.dot.p(q, p_diff))
}

demandbytype <- function(b.loads, b.prices, prices, pup, pdown, sigmath, month, scenario) {
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
  ifelse(sigmath==0, {
  demandflex = demandbygroupf(b.loads, netprices, b.prices, 1, month, scenario)
  demandother = (demandbygroupf(b.loads, prices, b.prices, 2, month, scenario) + 
                   demandbygroupf(b.loads, prices, b.prices, 3, month, scenario))
  demand = demandflex + demandother
  demandup = -(demandflex - demandflexmin)
  demanddown = -(demandflexmax - demandflex)
  }, ifelse(sigmath==1, {
    demand = demandbygroupf(b.loads, netprices, b.prices, 1, month, scenario)
    demandup = -(demand - demandflexmin)
    demanddown = -(demandflexmax - demand)
  }, ifelse(sigmath==10, {#flexible demand without considering
    demand = demandbygroupf(b.loads, prices, b.prices, 1, month, scenario)
    demandup = 0
    demanddown = 0
    }, ifelse(sigmath==2, {
      demand = demandbygroupf(b.loads, prices, b.prices, 2, month, scenario)
      demandup = 0
      demanddown = 0}, {
        demand = demandbygroupf(b.loads, prices, b.prices, 3, month, scenario)
        demandup = 0
        demanddown = 0}
  ))))
  # note: reserve quantities are negative to indicate sales from the demand side
  return (list(demand, demandup, demanddown))

}

integralcs <- function(p1, p0, q0, sigmath, month, scenario) {
  # calculate the line integral of q dot p from p0 to p1, with the specified demand parameters
  iqp <- integrate(
    Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
    p0, p1, q0[[1]], p0[[1]], sigmath, month, scenario)$value
  # get the quantities bid for energy and up and down reserves (assuming p0 and q0 are baselines)
  q1 <- demandbytype(q0[[1]], p0[[1]], p1[[1]], p1[[2]], p1[[3]], sigmath, month, scenario)
  # calculate wtp for this bid
  wtp <- q.dot.p(q1, p1) - q.dot.p(q0, p0) - iqp
  cs <- wtp - q.dot.p(q1, p1) #CS contains q.dot.p(q0, p0)
  return(list(wtp, cs, q.dot.p(q1, p1), q1))
}


# integralcs(pflat0, p0, q0, 0, 1, 1)[[2]]-integralcs(pflat0, p0, q0, 1, 1, 1)[[2]]-
#   integralcs(pflat0, p0, q0, 2, 1, 1)[[2]]-integralcs(pflat0, p0, q0, 3, 1, 1)[[2]]
# 
# flexprice0 <- list(rnorm(24,125,1), rep(0,24), rep(0,24))
# flexprice1 <- list(rnorm(24,125,20), rep(0,24), rep(0,24))
# flexprice1 <- list(rnorm(24,125*1.1,20), rep(0,24), rep(0,24))
# flexprice0 <- list(flatprice, rep(0,24), rep(0,24))
# flexprice1 <- list(flatprice1, rep(0,24), rep(0,24))
# 
# (integralcs(flexprice1, p0, q0, 1, 1, 1)[[2]]-integralcs(flexprice0, p0, q0, 1, 1, 1)[[2]])/integralcs(flexprice0, p0, q0, 1, 1, 1)[[3]]
# (integralcs(flexprice1, p0, q0, 2, 1, 1)[[2]]-integralcs(flexprice0, p0, q0, 2, 1, 1)[[2]])/integralcs(flexprice0, p0, q0, 2, 1, 1)[[3]]
# (integralcs(flexprice1, p0, q0, 3, 1, 1)[[2]]-integralcs(flexprice0, p0, q0, 3, 1, 1)[[2]])/integralcs(flexprice0, p0, q0, 3, 1, 1)[[3]]
# 
# integralcs(pflat0, p0, q0, 0, 1, 1)[[1]]-integralcs(pflat0, p0, q0, 1, 1, 1)[[1]]-
#   integralcs(pflat0, p0, q0, 2, 1, 1)[[1]]-integralcs(pflat0, p0, q0, 3, 1, 1)[[1]]
# sum(integralcs(pflat0, p0, q0, 0, 1, 1)[[3]][[1]]-integralcs(pflat0, p0, q0, 1, 1, 1)[[3]][[1]]-
#   integralcs(pflat0, p0, q0, 2, 1, 1)[[3]][[1]]-integralcs(pflat0, p0, q0, 3, 1, 1)[[3]][[1]])
# pup = datause$offered.price.energy.up
# pdown = datause$offered.price.energy.down
# tot <- demandbytype(basel, basep, prdyn, pup, pdown, 0,1,1)
# tot0 <- demandbytype(basel, basep, prdyn, pup+150, pdown*0, 0,1,1)
# tot1 <- demandbytype(basel, basep, prdyn, pup, pdown, 1,1,1)
# tot2 <- demandbytype(basel, basep, prdyn, pup, pdown, 2,1,1)
# tot3 <- demandbytype(basel, basep, prdyn, pup, pdown, 3,1,1)
# tot1[[1]]+tot2[[1]]+tot3[[1]]-tot[[1]]

#returns: 
# [[1]] demand highflex [[2]] demand midflex [[3]] demand inflex
# [[4]] delta CS highflex [[5]] delta cs midflex [[6]] delta cs inflex
# [[7]] total delta CS (highflex+midflex+inflex) 
# note: only highflex consider reserves price
# integralcs <- function(b.loads, qup, qdown, b.prices, bpup, bpdown, prices, pup, pdown, month, scenario) {
#   p0 = list(b.prices, bpup, bpdown)
#   p1 = list(prices, pup, pdown)
# 
#   qbflex = list(baseloadbygf(b.loads,1,month,scenario),qup, qdown)
#   qbmid = list(baseloadbygf(b.loads,2,month,scenario),qup, qdown)
#   qbinflex = list(baseloadbygf(b.loads,3,month,scenario),qup, qdown)
#   qbtot = list(b.loads,qup, qdown)
# 
#   qflex = demandbytype(b.loads, b.prices, prices, pup, pdown, 1,month, scenario)
#   qmid = demandbytype(b.loads, b.prices, prices, pup, pdown, 2,month, scenario)
#   qinflex = demandbytype(b.loads, b.prices, prices, pup, pdown, 3,month, scenario)
#   qtot = demandbytype(b.loads, b.prices, prices, pup, pdown, 0,month, scenario)
#   # if (sum(p1[[1]]) == 0) browser()
#   
#   # iqpflex = integrate(
#   #   Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#   #   p0, p1, qbflex[[1]], b.prices, 1, month, scenario)$value
#   # wtpflex = q.dot.p(qflex, p1) - q.dot.p(qbflex, p0) - iqpflex
#   # 
#   # iqpmid = integrate(
#   #   Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#   #   p0, p1, qbmid[[1]], b.prices, 2, month, scenario)$value
#   # wtpmid = q.dot.p(qmid, p1) - q.dot.p(qbmid, p0) - iqpmid
#   # 
#   # iqpinflex = integrate(
#   #   Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#   #   p0, p1, qbinflex[[1]], b.prices, 3, month, scenario)$value
#   # wtpinflex = q.dot.p(qinflex, p1) - q.dot.p(qbinflex, p0) - iqpinflex
# 
#   iqptot = integrate(
#     Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#     p0, p1, b.loads, b.prices, 0, month, scenario)$value
#   wtptot = q.dot.p(qtot, p1) - q.dot.p(qbtot, p0) - iqptot
# 
#   iqptot1 = integrate(
#     Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#     p0, p1, b.loads, b.prices, 1, month, scenario)$value
#   wtptot1 = q.dot.p(qflex, p1) - q.dot.p(qbflex, p0) - iqptot1
# 
#   # iqptot1a = integrate(
#   #   Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#   #   p0, p1, b.loads, b.prices, 10, month, scenario)$value
#   # wtptot1a = q.dot.p(qflex, p1) - q.dot.p(qbflex, p0) - iqptot1a
#   
#   iqptot2 = integrate(
#     Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#     p0, p1, b.loads, b.prices, 2, month, scenario)$value
#   wtptot2 = q.dot.p(qmid, p1) - q.dot.p(qbmid, p0) - iqptot2
# 
#   iqptot3 = integrate(
#     Vectorize(q.dot.p.for.t, vectorize.args=c('t')), 0, 1,
#     p0, p1, b.loads, b.prices, 3, month, scenario)$value
#   wtptot3 =  q.dot.p(qinflex, p1) - q.dot.p(qbinflex, p0)- iqptot3
# 
#   #print(paste(iqptot,iqptot1,iqptot2,iqptot3))
#   #print(paste(  wtptot,  wtptot1, wtptot2, wtptot3))
#   demandeach <- c(qflex[1],qmid[1],qinflex[1])
#   # wtpall <- c(wtpflex, wtpmid, wtpinflex)
#   wtpall <- c(wtptot1, wtptot2, wtptot3)
#   # wtp = -iqp  # q.dot.p terms drop out when including "all other goods"
#   return (c(demandeach, wtpall, wtptot))
# }

# prdyn1 = prdyn+rnorm(24,0)
# mean(prdyn1)
# mean(prdyn)
# flatprice <- rep(125,24)
# flatprice1 <- rep((125*1.1),24)
# cs<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, flatprice, pup*0, pdown*0, 1,1)
# cs1<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, flatprice1, pup*0, pdown*0, 1,1)
# (cs1[[7]]-cs[[7]])/(sum(cs1[[1]]+cs1[[2]]+cs1[[3]])*125)
# 
# #csbygroupf.ces <- function(xb, pd, pb, sigmath, month, scen) 
# totcs1 <- csbygroupf(basel, flatprice1, basep, 1, 1, 1) + csbygroupf(basel, flatprice1, basep, 2, 1, 1)  +csbygroupf(basel, flatprice1, basep, 3, 1, 1) 
# totcs <- csbygroupf(basel, flatprice, basep, 1, 1, 1) + csbygroupf(basel, flatprice, basep, 2, 1, 1)  +csbygroupf(basel, flatprice, basep, 3, 1, 1)
# (totcs1-totcs)/(sum(cs[[1]]+cs[[2]]+cs[[3]])*125)
# 
# cs<-integralcs(basel, basel*0, basel*0, rep(100,24), basep*0, basep*0, rep(110,24), pup*0, pdown*0, 1,1)
# totcs1 <- csbygroupf(basel, rep(110,24), basep, 1, 1, 1) + csbygroupf(basel, rep(110,24), basep, 2, 1, 1)  +csbygroupf(basel, rep(110,24), basep, 3, 1, 1) 
# totcs <- csbygroupf(basel, rep(100,24), basep, 1, 1, 1) + csbygroupf(basel, rep(100,24), basep, 2, 1, 1)  +csbygroupf(basel, rep(100,24), basep, 3, 1, 1)
# (totcs1-totcs)/(sum(cs[[1]]+cs[[2]]+cs[[3]])*100)
# 
# (csbygroupf(basel, flatprice1, basep, 1, 1, 1) - csbygroupf(basel, flatprice, basep, 1, 1, 1))/(sum(cs[[1]]+cs[[2]]+cs[[3]])*125)
# (csbygroupf(basel, flatprice1, basep, 2, 1, 1) - csbygroupf(basel, flatprice, basep, 2, 1, 1))/(sum(cs[[1]]+cs[[2]]+cs[[3]])*125)
# (csbygroupf(basel, flatprice1, basep, 3, 1, 1) - csbygroupf(basel, flatprice, basep, 3, 1, 1))/(sum(cs[[1]]+cs[[2]]+cs[[3]])*125)
# 
# cs<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, basep*1.1, pup*0, pdown*0, 1,1)
# 
# cs<-integralcs(c(50,100), c(0,0), c(0,0), c(100,100), c(0,0), c(0,0), c(110,110), c(0,0), c(0,0), 1,1)
# cs[[7]]/(150*100)
# cs<-integralcs(rep(500,24), basel*0, basel*0, rep(100,24), basep*0, basep*0, rep(150,24), pup*0, pdown*0, 1,1)
# cs[[7]]/(500*24*100)
# cs<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, prdyn, pup, pdown, 1,1)
# cs2<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, prdyn*0.5, prdyn*0, prdyn*0, 1,1)
# cs3<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, prdyn1, prdyn*0, prdyn*0, 1,1)
# cs4<-integralcs(basel, basel*0, basel*0, basep, basep*0, basep*0, prdyn1, prdyn+rnorm(24,5), prdyn*0, 1,1)

# (cs[[4]]-cs3[[4]])/cs[[7]]
# cs2[[4]]
# cs3[[4]]
# 
# (cs[[5]]-cs3[[5]])/cs[[7]]
# cs2[[5]]
# cs3[[5]]
# 
# (cs[[6]]-cs3[[6]])/cs[[7]]
# cs2[[6]]

#> 1494662+3662413+2245582
#[1] -7402657
#total 337324.7
# csbygroupf(b.loads, flexprice, b.prices, 1, 1, scen)
# 
# #test demandf returns base load when prices didnt chance
# round(demandbygroupf(b.loads, b.prices, b.prices, 1,1,3)
#       +demandbygroupf(b.loads, b.prices, b.prices, 2,1,3)
#       +demandbygroupf(b.loads, b.prices, b.prices, 3,1,3)
#       -b.loads,2)
# 
# round(demandbygroupf(b.loads, b.prices, b.prices, 1,1,1)
#       +demandbygroupf(b.loads, b.prices, b.prices, 2,1,1)
#       +demandbygroupf(b.loads, b.prices, b.prices, 3,1,1)
#       -b.loads,2)
# 
# round(demandtotf(b.loads, b.prices, b.prices, 1,1)-b.loads,2)
# round(demandtotf(b.loads, b.prices, b.prices, 1,3)-b.loads,2)
# 
# #test flat price= results show that different scenario leads to same Q
# round(demandbygroupf(b.loads, b.prices-100, b.prices, 1,1,3)
#       +demandbygroupf(b.loads, b.prices-100, b.prices, 2,1,3)
#       +demandbygroupf(b.loads, b.prices-100, b.prices, 3,1,3)
#       -b.loads,2)
# 
# round(demandbygroupf(b.loads, b.prices-100, b.prices, 1,1,1)
#       +demandbygroupf(b.loads, b.prices-100, b.prices, 2,1,1)
#       +demandbygroupf(b.loads, b.prices-100, b.prices, 3,1,1)
#       -b.loads,2)
# 
# #test variable price= results show that 
# #scenario with more flexible shares lead to higher Q
# round(demandbygroupf(b.loads, c(rep(100,5),rep(120,5),rep(250,14)), b.prices, 1,1,3)
#       +demandbygroupf(b.loads, c(rep(100,5),rep(120,5),rep(250,14)), b.prices, 2,1,3)
#       +demandbygroupf(b.loads, c(rep(100,5),rep(120,5),rep(250,14)), b.prices, 3,1,3)
#       -b.loads,2)
# 
# round(demandbygroupf(b.loads, c(rep(100,5),rep(120,5),rep(250,14)), b.prices, 1,1,1)
#       +demandbygroupf(b.loads, c(rep(100,5),rep(120,5),rep(250,14)), b.prices, 2,1,1)
#       +demandbygroupf(b.loads, c(rep(100,5),rep(120,5),rep(250,14)), b.prices, 3,1,1)
#       -b.loads,2)
# 
# 
# scen <- 1
# 
# #test flat price
# flatprice <- b.prices-50
# (wtpbygroupf(demandbygroupf(b.loads, flatprice, b.prices, 1,1,scen), b.loads, flatprice, b.prices, 1, 1, scen) - 
#     wtpbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 1,1,scen), b.loads, b.prices, b.prices, 1, 1, scen) )/  baseloadbygf(b.loads, 1, 1, scen) 
# (wtpbygroupf(demandbygroupf(b.loads, flatprice, b.prices, 2,1,scen), b.loads, flatprice, b.prices, 2, 1, scen) - 
#     wtpbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 2,1,scen), b.loads, b.prices, b.prices, 2, 1, scen) )/  baseloadbygf(b.loads, 2, 1, scen) 
# (wtpbygroupf(demandbygroupf(b.loads, flatprice, b.prices, 3,1,scen), b.loads, flatprice, b.prices, 3, 1, scen) - 
#     wtpbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 3,1,scen), b.loads, b.prices, b.prices, 3, 1, scen) )/  baseloadbygf(b.loads, 3, 1, scen) 
# 
# (csbygroupf(demandbygroupf(b.loads, flatprice, b.prices, 1,1,scen), b.loads, flatprice, b.prices, 1, 1, scen) - 
#     csbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 1,1,scen), b.loads, b.prices, b.prices, 1, 1, scen) )/  baseloadbygf(b.loads, 1, 1, scen) 
# (csbygroupf(demandbygroupf(b.loads, flatprice, b.prices, 2,1,scen), b.loads, flatprice, b.prices, 2, 1, scen) - 
#     csbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 2,1,scen), b.loads, b.prices, b.prices, 2, 1, scen) )/  baseloadbygf(b.loads, 2, 1, scen) 
# (csbygroupf(demandbygroupf(b.loads, flatprice, b.prices, 3,1,scen), b.loads, flatprice, b.prices, 3, 1, scen) - 
#     csbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 3,1,scen), b.loads, b.prices, b.prices, 3, 1, scen) )/  baseloadbygf(b.loads, 3, 1, scen) 
# 
# #test flex price
# flexprice <- b.prices + rnorm(n=24, mean=0, sd=25)
# (wtpbygroupf(demandbygroupf(b.loads, flexprice, b.prices, 1,1,scen), b.loads, flexprice, b.prices, 1, 1, scen) - 
#    wtpbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 1,1,scen), b.loads, b.prices, b.prices, 1, 1, scen) )/  baseloadbygf(b.loads, 1, 1, scen) 
# (wtpbygroupf(demandbygroupf(b.loads, flexprice, b.prices, 2,1,scen), b.loads, flexprice, b.prices, 2, 1, scen) - 
#    wtpbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 2,1,scen), b.loads, b.prices, b.prices, 2, 1, scen) )/  baseloadbygf(b.loads, 2, 1, scen) 
# (wtpbygroupf(demandbygroupf(b.loads, flexprice, b.prices, 3,1,scen), b.loads, flexprice, b.prices, 3, 1, scen) - 
#    wtpbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 3,1,scen), b.loads, b.prices, b.prices, 3, 1, scen) )/  baseloadbygf(b.loads, 3, 1, scen) 
# 
# (csbygroupf(demandbygroupf(b.loads, flexprice, b.prices, 1,1,scen), b.loads, flexprice, b.prices, 1, 1, scen) - 
#    csbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 1,1,scen), b.loads, b.prices, b.prices, 1, 1, scen) )/  baseloadbygf(b.loads, 1, 1, scen) 
# (csbygroupf(demandbygroupf(b.loads, flexprice, b.prices, 2,1,scen), b.loads, flexprice, b.prices, 2, 1, scen) - 
#    csbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 2,1,scen), b.loads, b.prices, b.prices, 2, 1, scen) )/  baseloadbygf(b.loads, 2, 1, scen) 
# (csbygroupf(demandbygroupf(b.loads, flexprice, b.prices, 3,1,scen), b.loads, flexprice, b.prices, 3, 1, scen) - 
#    csbygroupf(demandbygroupf(b.loads, b.prices, b.prices, 3,1,scen), b.loads, b.prices, b.prices, 3, 1, scen) )/  baseloadbygf(b.loads, 3, 1, scen) 


