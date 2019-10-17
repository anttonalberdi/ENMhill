#' Spatial breadth profile computation based on Hill numbers
#' @title Spatial breadth profile
#' @author Antton Alberdi, \email{anttonalberdi@gmail.com}
#' @keywords breadth diversity hill profile
#' @description Compute profiles of Hill numbers-based spatial breadths from one or multiple Environmental Niche Model projection rasters.
#' @param raster A RasterLayer (single projection) or RasterStack (multiple projections) object containing ENM projection(s) with suitability scores.
#' @param qvalues A vector of sequential orders of diversity (default from 0 to 5). qvalues=seq(from = 0, to = 5, by = (0.1))
#' @param relative Whether to compute absolute or relative breadths. Default=FALSE
#' @param threshold Suitability value(s) below which all values are converted into zeros. If a RasterStack (multiple projections) is used, the argument should contain a vector of threshold values.
#' @return A vector of breadth values at different orders of diversity or a matrix containing breadth values at different orders of diversity (columns) per projection (rows).
#' @seealso \code{\link{breadth}}
#' @examples
#' data()
#' breadth_profile(rasters[[1]])
#' breadth_profile(rasters[[1]],qvalues=seq(from = 0, to = 5, by = 1))
#' breadth_profile(rasters[[1]],qvalues=seq(from = 0, to = 5, by = 1), relative=TRUE)
#' breadth_profile(rasters,qvalues=seq(from = 0, to = 5, by = 1), relative=TRUE)
#' @references
#' Alberdi, A., Novella-Fernandez R., Aizpurua, O., Razgour, O. (2019). Measuring breadth and overlap of spatial projections of environmental niche models based on Hill numbers.\cr\cr
#' Alberdi, A., Gilbert, M.T.P. (2019). A guide to the application of Hill numbers to DNA-based diversity analyses. Molecular Ecology Resources, 19, 804-817.\cr\cr
#' @export

breadth_profile <- function(raster,qvalues,relative,threshold){

#Quality-check and warnings
if(missing(raster)) stop("Spatial projection data are missing. Please, provide them as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(class(raster) != "RasterLayer" && class(raster) != "RasterStack") stop("Spatial projection data are incorrect. Please, provide ENM projections as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(missing(qvalues)) {qvalues = seq(from = 0, to = 5, by = (0.1))}
if(missing(relative)) {relative=FALSE}

#One projections
if(class(raster) == "RasterLayer"){
if(missing(threshold)) {threshold=0}
if(length(threshold) != 1) stop("A single threshold value must be provided when using a RasterLayer object")

raster.vector <- as.vector(raster)
raster.vector <- raster.vector[!is.na(raster.vector)]
raster.vector[raster.vector < threshold] <- 0
raster.vector.relative <- rel(raster.vector)

breadth.profile <- c()
  for (q in qvalues){
    breadth <- neutralbreadth(raster.vector.relative,qvalue=q)
    breadth.profile <- c(breadth.profile,breadth)
  }
names(breadth.profile) <- qvalues

if(relative == FALSE){
  return(breadth.profile)
  }
if(relative == TRUE){
  breadth.profile.rel <- breadth.profile / neutralbreadth(raster.vector.relative,qvalue=0)
  return(breadth.profile.rel)
  }

}

#Multiple projections
if(class(raster) == "RasterStack"){
raster.names <- names(raster)
if(missing(threshold)) {threshold=rep(0,length(raster.names))}
if(length(threshold) != length(raster.names)) stop("The amount of threshold values is different to the number of projections. Provide a vector of threshold values.")

breadth.profiles.matrix <- c()
  for (i in c(1:length(raster.names))){
    raster.vector <- as.vector(raster[[i]])
    raster.vector <- raster.vector[!is.na(raster.vector)]
    raster.vector[raster.vector < threshold] <- 0
    raster.vector.relative <- rel(raster.vector)

    breadth.profile <- c()
      for (q in qvalues){
        breadth <- neutralbreadth(raster.vector.relative,qvalue=q)
        breadth.profile <- c(breadth.profile,breadth)
      }

      if(relative == FALSE){
        breadth.profiles.matrix <- rbind(breadth.profiles.matrix,breadth.profile)
        }
      if(relative == TRUE){
        breadth.profile.rel <- breadth.profile / neutralbreadth(raster.vector.relative,qvalue=0)
        breadth.profiles.matrix <- rbind(breadth.profiles.matrix,breadth.profile.rel)
        }
  }
rownames(breadth.profiles.matrix) <- raster.names
colnames(breadth.profiles.matrix) <- qvalues
return(breadth.profiles.matrix)
}

}
