rm(list = ls())
require(RCurl)

source('CAS_fn.r')
###I. Build the cell


#set the number of sites in the cell
N = 10

#set length for each chromosome
len = 100

#give names for the 48 chromosomes.
chrom.single = paste(c('t', rep('a', len/2), 'c', rep('b', len/2), 't'),collapse = '')
chrom = rep(chrom.single, 48)
names(chrom) = 1:48

#separate the chromosomes into N sites randomly
set.seed(20)
sites = sample(x = 1:10, size = 48, replace = T)
chrom.site = split(chrom, f = as.factor(sites))


###II. Make break in sites



#set mean for total number of breaks
avgBr = 10

#set parameters in LET
a = 1 
L = 2


#generate the total number of breaks with poission distribution
set.seed(20)
numBr = rpois(n = 1, lambda = avgBr)

#generate the number of breaks created together with LET effect
set.seed(20)
breaks = sapply(1:numBr,function(i) rpois(1, lambda = a*L)) + 1

#generate places where first breaks happen
set.seed(20)
breaks.pl = sample(1:48, size = numBr, replace = T)

#count total number of breaks happen in each site
breaks = split(breaks, f = as.factor(sites[breaks.pl]))
breaks = lapply(breaks, sum)
breaks0 = rep(list(0), N)
names(breaks0) = 1:N
breaks = merge.list(breaks, breaks0)



#generate places where breaks happen in each site
breaks = sapply(1:N, site.breaks)
breaks = unlist(breaks, recursive = F)
breaks = breaks[order(as.numeric(names(breaks)))]






#create breaks. The broken chromosomes are still stored in chrom.site
sapply(1:48, chrom.replace)




