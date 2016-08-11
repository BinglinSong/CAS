rm(list = ls())
### I. Load required packages, functions and objects
require(RCurl)
source("CASobj_prim1.r")#load the objects
source("CASobj_fn1.r")#load the functions

### II. Build the cell
## Set Needed parameters

N = 2#number of sites in each cell

param = data.frame(ICindex = 1:4, segnum = c(4,4,4,4), cenloc = c(2,1,2,1))



##Create Initial Chromosome objects

chrom = sapply(1:dim(param)[1], 
               function(i) do.call(IC, param[i,]))


## Separate the chromosomes into N sites randomly
set.seed(10)
sites = sample(x = 1:N, size = dim(param)[1], replace = T)
chrom.site = split(chrom, f = as.factor(sites))


### III. Make break in sites

## Set mean for total number of breaks
avgBr = 2

## Set parameters in LET
a = 1
L = 2


## Generate the total number of breaks with poission distribution
set.seed(10)
numBr = rpois(n = 1, lambda = avgBr)

## Generate the number of breaks created together with LET effect
set.seed(20)
breaks = sapply(1:numBr,function(i) rpois(1, lambda = a*L)) + 1

## Generate places where first breaks happen
sam.space = rep(1:dim(param)[1], param$segnum)
set.seed(20)
breaks.pl = sample(sam.space, size = numBr, replace = F)

## Count total number of breaks happen in each site
breaks = split(breaks, f = as.factor(sites[breaks.pl]))
breaks = lapply(breaks, sum)
breaks0 = rep(list(0), N)
names(breaks0) = 1:N
breaks = merge.list(breaks, breaks0)






## Generate places where breaks happen in each site
set.seed(20)
breaks = lapply(1:N, site.breaks)
breaks = unlist(breaks, recursive = F)
breaks = breaks[order(as.numeric(names(breaks)))]

## Create classes FE with the breaks generated above



FE.chrom = lapply(1:N, create.FE)


### IV. Make misrejoining in sites

## Set parameters for the Markov process during misrejoining
A = 1#the parameter that effects the rate of restitution, the larger A is, the higher the rate of restitution is.
B = 0.2#the parameter that effects the rate of misrejoining, the larger B is, the higher the rate of misrejoining is.

## Create Misrejoining
set.seed(20)
MR.chrom = lapply(1:N, create.MR)

