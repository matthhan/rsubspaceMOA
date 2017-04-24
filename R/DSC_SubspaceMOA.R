#'@import rJava
#'@export
get_centers.DSC_SubspaceMOA <- function(x,type=c("auto","micro","macro"),...) {
  methodToCall <- ""
  if(("auto" %in% type) || "macro" %in% type) {
    methodToCall <- "getMacroclusteringCenters"
  } else if ("micro" %in% type) {
    methodToCall <- "getMicroclusteringCenters"
  } else {
    stop("Not implemented yet")
  }
  res <- rJava::.jcall(x$javaObj,"[[D",methodToCall,evalArray=T,simplify=T)
  if(length(as.vector(res))==1 & as.vector(res)[1]==0)return(NULL)
  return(data.frame(res))
}
#'@import rJava
#'@export
get_weights.DSC_SubspaceMOA <- function(x,type=c("auto","micro","macro"),scale=NULL,...) {
  methodToCall <- ""
  if(("auto" %in% type) || "macro" %in% type) {
    methodToCall <- "getMacroclusteringWeights"
  } else if ("micro" %in% type) {
    methodToCall <- "getMicroclusteringWeights"
  } else {
    stop("Not implemented yet")
  }
  res <- rJava::.jcall(x$javaObj,"[D",methodToCall,evalArray=T)
  if(length(as.vector(res))==1 & as.vector(res)[1]==0)return(NULL)
  return (scale_weights(res,scale=scale))
}

#'@export
#'@import stream
#'@import rJava
update.DSC_SubspaceMOA <- function(object,dsd,n = 1, verbose = FALSE, ...) {
  points <- get_points(dsd,n)
  dsc <- object
  apply(points,1,function(row){
    rJava::.jcall(dsc$javaObj,"V","trainOn",row)
  })
}
 
#'@import rJava
isClusterInDim <- function(dsc,clusteri,dim) {
  return(rJava::.jcall(dsc$javaObj,"Z","isClusterInDimension",as.integer(clusteri-1),as.integer(dim-1)))
}
 
#'@import rJava
getRadiusOfCluster <- function(dsc,clusteri) {
  res <- rJava::.jcall(dsc$javaObj,"D","getRadiusOfCluster",as.integer(clusteri-1),evalArray = T)
  return(res)
}
 
#'@import rJava
getBoundariesOfClusterInDim <- function(dsc,clusteri,dim) {
  return(rJava::.jcall(dsc$javaObj,"[D","getBordersOfClusterInDimension",as.integer(clusteri-1),as.integer(dim-1)))
}
