#function to generate places that breaks happen in a certain site
site.breaks = function(site.num){
  breaks.num = breaks[[as.character(site.num)]]
  chroms = chrom.site[[site.num]]
  chrom.num = sapply(chroms, getsegnum)
  chrom.breaks = split(sample(x = c(rep(1, breaks.num), 
                                    rep(0, sum(chrom.num) - breaks.num)), replace = F), 
                       f = as.factor(rep(sapply(chroms, getICind), times = chrom.num)))
  return(chrom.breaks)
}

# Function to create FEs
create.FE = function(site.num){
  chroms = chrom.site[[site.num]]
  Ind.num = sapply(chroms, getICind)
  BP.ori = lapply(Ind.num, function(i) which(breaks[[as.character(i)]] == 1))
  BP.num = unlist(BP.ori)
  FE.data = data.frame(param[rep(Ind.num, 2*sapply(BP.ori, length)), ], BP = rep(BP.num, each = 2), hand = c(F, T) )
  site.FE = sapply(1:dim(FE.data)[1], function(i) do.call(FE, FE.data[i, ]))
  return(site.FE)
}


# Functions that creates rejoining in one location.

create.MR = function(site.num){
  FEs = FE.chrom[[site.num]]
  n.fe = length(FEs)
  if(n.fe == 0){
    return(list())
  }
  mr.list = list()
  n.pair = 2*(1:(n.fe/2)) - 1
  n.single = 1:n.fe
  while(length(n.single) > 0){
    restitute = sample(rep(c(1, 0), times = 1000*c(A*length(n.pair), B*(length(n.single)*(length(n.single) - 1))/2)), size = 1)
    if(restitute){
      pair = n.pair[sample.int(length(n.pair), size = 1)]
      n.pair = n.pair[-(which(n.pair == pair))]
      n.single = n.single[-(which(n.single == pair | n.single == (pair + 1)))]
      mr.list = c(mr.list, MR(FE1 = FEs[[pair]], FE2 = FEs[[pair + 1]]))
    }else{
      mispair = sample(n.single, size = 2, replace = F)
      n.single = n.single[-(which(n.single == mispair[1] | n.single == mispair[2]))]
      n.pair = n.pair[!(n.pair %in% mispair | n.pair %in% (mispair - 1))]
      mr.list = c(mr.list, MR(FE1 = FEs[[mispair[1]]], FE2 = FEs[[mispair[2]]]))      
    }
  }
  return(mr.list)
  
}
