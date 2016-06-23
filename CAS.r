
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

#generate the total number of breaks with poission distribution
set.seed(20)
numBr = rpois(n = 1, lambda = avgBr)
set.seed(20)

#generate the places where the breaks happen
breaks = split(sample(x = c(rep(1, numBr), rep(0, 48*(len+2) - numBr)), replace = F), 
               f = as.factor(rep(1:48, each = len+2)))

#function that breaks the chromosomes
find.break = function(breaks, chrom){
  if(!sum(breaks)){
    return(chrom)
  }else{
    br.pt = c(0, which(breaks == 1), len + 3)
    subchrom = sapply(1:(length(br.pt) - 1), function(i) 
      substr(chrom, start = br.pt[i] + 1, stop =  br.pt[i+1]))
    return(subchrom)
  }
}

#function that create all breaks generated above
chrom.replace = function(chrom.name){
  site = sites[chrom.name]
  chrom = chrom.site[[site]][as.character(chrom.name)]
  chrom.site[[site]] <<- chrom.site[[site]][names(chrom.site[[site]]) != as.character(chrom.name)]
  chrom.site[[site]] <<- c(chrom.site[[site]], find.break(breaks[[chrom.name]], chrom))
}

#create breaks. The broken chromosomes are still stored in chrom.site
sapply(1:48, chrom.replace)


#set parameters in LET
a = 1 
L = 2


