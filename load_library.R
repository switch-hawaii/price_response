#install.packages(c("xtable", "ggplot2", "reshape2", 
                   #"Hmisc", "stringr", "plyr","data.table",
                   #"RColorBrewer", "gtable", "lubridate")) 

library(xtable, quietly=TRUE)
library(ggplot2, quietly=TRUE)
library(reshape2, quietly=TRUE)
library(Hmisc, quietly=TRUE) #for number of days in a month
library(stringr, quietly=TRUE) #str_to_title
library(plyr) #applying function to each group
#library(dplyr) #applying function to each group
library(data.table, quietly=TRUE)
library(RColorBrewer, quietly=TRUE) #Create a custom color scale
library(gtable, quietly=TRUE)
library(grid, quietly=TRUE)
library(lubridate, quietly=TRUE)
options(scipen=999)  # turn-off scientific notation like 1e+48
sessionInfo()


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

roundUp <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

pathit <- function(FUN, path){
  function(file, ...){
    FUN(file=file.path(path, file), ...)
  }
}