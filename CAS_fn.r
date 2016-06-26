require(RCurl)

#function to generate places that breaks happen in a certain site
site.breaks = function(site.num){
  breaks.num = breaks[[as.character(site.num)]]
  chroms = chrom.site[[site.num]]
  chrom.num = length(chroms)
  chrom.breaks = split(sample(x = c(rep(1, breaks.num), 
                                    rep(0, chrom.num*(len+2) - breaks.num)), replace = F), 
                       f = as.factor(rep(as.numeric(names(chroms)), each = len+2)))
  return(chrom.breaks)
}

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
