#' Convert raster(s) into cell vector or matrix
#' @title Convert raster(s) into cell vector or matrix
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

cell_table <- function(raster,threshold,normalise){
if(missing(raster)) stop("Spatial projection data are missing. Please, provide them as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(class(raster) != "RasterLayer" && class(raster) != "RasterStack") stop("Spatial projection data are incorrect. Please, provide ENM projections as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(missing(normalise)) {normalise=TRUE}

  if(class(raster) == "RasterLayer"){
    if(missing(threshold)) {threshold=0}
    if(length(threshold) != 1) stop("A single threshold value must be provided when using a RasterLayer object")

    raster.vector <- as.vector(raster)
    raster.vector <- raster.vector[!is.na(raster.vector)]
    raster.vector[raster.vector < threshold] <- 0
    if(normalise == TRUE){raster.vector <- raster.vector/sum(raster.vector)}
    return(raster.vector)
  }

  if(class(raster) == "RasterStack"){
    raster.names <- names(raster)
    if(missing(threshold)) {threshold=rep(0,length(raster.names))}
    if(length(threshold) != length(raster.names)) stop("The amount of threshold values is different to the number of projections. Provide a vector of threshold values.")
    raster.matrix <- c()
    for (i in c(1:length(raster.names))){
      raster.vector <- as.vector(raster[[i]])
      raster.vector <- raster.vector[!is.na(raster.vector)]
      raster.vector[raster.vector < threshold[i]] <- 0
      #if(normalise == TRUE){raster.vector <- raster.vector/sum(raster.vector)}
      raster.matrix <- cbind(raster.matrix,raster.vector)
    }
    raster.matrix <- raster.matrix[apply(raster.matrix, 1, function(z) !all(z==0)),]
    colnames(raster.matrix) <- raster.names
    return(raster.matrix)
  }
}
