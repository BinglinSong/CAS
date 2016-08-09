##implement the class for IC
IC <- setClass(
  "IC",
  
  slots = c(
    ICindex = "numeric",
    segnum   = "numeric",
    cenloc   = "numeric"
  ),
  
  prototype=list(
    ICindex = 1,
    segnum   = 2,
    cenloc = 1
  ),
  
  validity=function(object)
  {
    if(any(c(object@ICindex, object@segnum, object@cenloc) <= 0) || object@cenloc >= object@segnum) {
      return("Invalid Chromosome")
    }
    return(TRUE)
  }
)

#function to set and get ICindex
setGeneric(name="setICind",
           def=function(theObject,ind)
           {
             standardGeneric("setICind")
           }
)

setMethod(f="setICind",
          signature="IC",
          definition=function(theObject,ind)
          {
            theObject@ICindex <- ind
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="getICind",
           def=function(theObject)
           {
             standardGeneric("getICind")
           }
)

setMethod(f="getICind",
          signature="IC",
          definition=function(theObject)
          {
            return(theObject@ICindex)
          }
)

#function to set and get segnum
setGeneric(name="setsegnum",
           def=function(theObject, segnum)
           {
             standardGeneric("setsegnum")
           }
)

setMethod(f="setsegnum",
          signature="IC",
          definition=function(theObject,segnum)
          {
            theObject@segnum <- segnum
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="getsegnum",
           def=function(theObject)
           {
             standardGeneric("getsegnum")
           }
)

setMethod(f="getsegnum",
          signature="IC",
          definition=function(theObject)
          {
            return(theObject@segnum)
          }
)

#function to set and get cenloc
setGeneric(name="setcenloc",
           def=function(theObject, cenloc)
           {
             standardGeneric("setcenloc")
           }
)

setMethod(f="setcenloc",
          signature="IC",
          definition=function(theObject,cenloc)
          {
            theObject@cenloc <- cenloc
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="getcenloc",
           def=function(theObject)
           {
             standardGeneric("getcenloc")
           }
)

setMethod(f="getcenloc",
          signature="IC",
          definition=function(theObject)
          {
            return(theObject@cenloc)
          }
)



# Create the DSB class by inheriting from IC

DSB <- setClass(
  "DSB",
  
  slots = c(
    BP = "numeric"
  ),
  
  prototype=list(
    BP = 1
  ),
  
  validity=function(object)
  {
    if(any(c(object@ICindex, object@segnum, object@cenloc) <= 0) || 
         object@cenloc >= object@segnum) {
      return("Invalid IC")
    }
    if(object@BP > object@segnum){
      return("BP out of range")
    }
    return(TRUE)
  },

  
  contains = "IC"
)

#function to set and get BP
setGeneric(name="setBP",
           def=function(theObject, BP)
           {
             standardGeneric("setBP")
           }
)

setMethod(f="setBP",
          signature="DSB",
          definition=function(theObject,BP)
          {
            theObject@BP <- BP
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="getBP",
           def=function(theObject)
           {
             standardGeneric("getBP")
           }
)

setMethod(f="getBP",
          signature="DSB",
          definition=function(theObject)
          {
            return(theObject@BP)
          }
)


# Create the FE class by inheriting from DSB

FE <- setClass(
  "FE",
  # Here I used logical True and False to indicate the right and left hand respectively
  slots = c(
    hand = "logical"
  ),
  
  prototype=list(
    hand = T
  ),
  
  validity=function(object)
  {
    if(any(c(object@ICindex, object@segnum, object@cenloc) <= 0) || 
         object@cenloc >= object@segnum) {
      return("Invalid IC")
    }
    if(object@BP > object@segnum){
      return("BP out of range")
    }
    return(TRUE)
  },
  
  
  contains = "DSB"
)

#function to set and get hand of FE
setGeneric(name="sethand",
           def=function(theObject, hand)
           {
             standardGeneric("sethand")
           }
)

setMethod(f="sethand",
          signature="FE",
          definition=function(theObject,hand)
          {
            theObject@hand <- hand
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="gethand",
           def=function(theObject)
           {
             standardGeneric("gethand")
           }
)

setMethod(f="gethand",
          signature="FE",
          definition=function(theObject)
          {
            return(theObject@hand)
          }
)


# Create the MR class

MR <- setClass(
  "MR",
  slots = c(
    FE1 = "FE",
    FE2 = "FE"
  ),
  
  validity=function(object)
  {
    return(TRUE)
  }
)

#function to set and get FEs of MR
setGeneric(name="setFE1",
           def=function(theObject, FE1)
           {
             standardGeneric("setFE1")
           }
)

setMethod(f="setFE1",
          signature="MR",
          definition=function(theObject,FE1)
          {
            theObject@FE1 <- FE1
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="setFE2",
           def=function(theObject, FE2)
           {
             standardGeneric("setFE2")
           }
)

setMethod(f="setFE2",
          signature="MR",
          definition=function(theObject,FE2)
          {
            theObject@FE2 <- FE2
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="getFE1",
           def=function(theObject)
           {
             standardGeneric("getFE1")
           }
)

setMethod(f="getFE1",
          signature="MR",
          definition=function(theObject)
          {
            return(theObject@FE1)
          }
)

setGeneric(name="getFE2",
           def=function(theObject)
           {
             standardGeneric("getFE2")
           }
)

setMethod(f="getFE2",
          signature="MR",
          definition=function(theObject)
          {
            return(theObject@FE2)
          }
)

