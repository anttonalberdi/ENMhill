#' Spatial breadth computation based on Hill numbers
#' @title Spatial breadth
#' @author Antton Alberdi, \email{anttonalberdi@gmail.com}
#' @keywords breadth diversity hill
#' @description Compute Hill numbers-based spatial breadths from one or multiple Environmental Niche Model projection rasters.
#' @param raster A RasterLayer (single projection) or RasterStack (multiple projections) object containing ENM projection(s) with suitability scores.
#' @param qvalue A positive number, usually between 0 and 5, but most commonly 0, 1 or 2. It can be an integer or contain decimals.
#' @param relative Whether to compute absolute or relative breadths. Default=FALSE
#' @param threshold Suitability value(s) below which all values are converted into zeros. If a RasterStack (multiple projections) is used, the argument should contain a vector of threshold values.
#' @return A single or multiple spatial breadth measures.
#' @seealso \code{\link{breadth_profile}}
#' @examples
#' data()
#' breadth(rasters[[1]],qvalue=1)
#' breadth(rasters,qvalue=2,relative=TRUE)
#' breadth(rasters,qvalue=2,relative=TRUE)
#' breadth(raster[[1]],qvalue=2,relative=TRUE,threshold=100)
#' breadth(rasters,qvalue=2,relative=FALSE,threshold=c(82,98,102,98,150,34)
#' @references
#' Alberdi, A., Novella-Fernandez R., Aizpurua, O., Razgour, O. (2019). Measuring breadth and overlap of spatial projections of environmental niche models based on Hill numbers.\cr\cr
#' Alberdi, A., Gilbert, M.T.P. (2019). A guide to the application of Hill numbers to DNA-based diversity analyses. Molecular Ecology Resources, 19, 804-817.\cr\cr
#' @export

breadth <- function(raster,qvalue,relative,threshold){

#Quality-check and warnings
if(missing(raster)) stop("Spatial projection data are missing. Please, provide them as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(class(raster) != "RasterLayer" && class(raster) != "RasterStack") stop("Spatial projection data are incorrect. Please, provide ENM projections as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(missing(qvalue)) stop("q value is missing")
if(qvalue == 1){qvalue <- 0.999999}
if(missing(relative)) {relative=FALSE}
if(relative == TRUE && qvalue == 0) stop("The order of diversity needs to be q>0 (usualy q=1 or q=2) in order to obtain meaningful relative breadth measures")

#Functions
neutralbreadth <- function(vector,qvalue){
    pi <- vector[vector!=0]
    sum(pi^qvalue)^(1/(1-qvalue))
    }

rel <- function(vector){
  vector/sum(vector)
  }

#One projections
if(class(raster) == "RasterLayer"){
if(missing(threshold)) {threshold=0}  
if(length(threshold) != 1) stop("A single threshold value must be provided when using a RasterLayer object")

raster.vector <- as.vector(raster)
raster.vector <- raster.vector[!is.na(raster.vector)]
raster.vector[raster.vector < threshold] <- 0
raster.vector.relative <- rel(raster.vector)
breadth <- neutralbreadth(raster.vector.relative,qvalue)

if(relative == FALSE){
  return(breadth)
  }
if(relative == TRUE){
  breadth.rel <- breadth / neutralbreadth(raster.vector.relative,qvalue=0)
  return(breadth.rel)
  }

}

#Multiple projections
if(class(raster) == "RasterStack"){
raster.names <- names(raster)
if(missing(threshold)) {threshold=rep(0,length(raster.names))}
if(length(threshold) != length(raster.names)) stop("The amount of threshold values is different to the number of projections. Provide a vector of threshold values.")

  breadths.vector <- c()
  for (i in c(1:length(raster.names))){
    raster.vector <- as.vector(raster[[i]])
    raster.vector <- raster.vector[!is.na(raster.vector)]
    raster.vector[raster.vector < threshold] <- 0
    raster.vector.relative <- rel(raster.vector)
    breadth <- neutralbreadth(raster.vector.relative,qvalue)
    if(relative == FALSE){
      breadths.vector <- c(breadths.vector, breadth)
      }
    if(relative == TRUE){
      breadth.rel <- breadth / neutralbreadth(raster.vector.relative,qvalue=0)
      breadths.vector <- c(breadths.vector, breadth.rel)
      }

  }
names(breadths.vector) <- raster.names
return(breadths.vector)
}


}
