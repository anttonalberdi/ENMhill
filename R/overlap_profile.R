#' Spatial overlap profile computation based on Hill numbers
#' @title Spatial overlap profile
#' @author Antton Alberdi, \email{anttonalberdi@gmail.com}
#' @keywords overlap turnover diversity hill profile
#' @description Compute profiles of Hill numbers-based spatial overlaps from multiple Environmental Niche Model projection rasters.
#' @param raster A RasterStack (multiple projections) object containing ENM projections with suitability scores.
#' @param qvalues A vector of sequential orders of diversity (default from 0 to 5). qvalues=seq(from = 0, to = 5, by = (0.1))
#' @param metric A vector indicating the similarity metrics to be computed. Default: metric=c("C","U","V","S")
#' @param threshold Suitability value(s) below which all values are converted into zeros. If a RasterStack (multiple projections) is used, the argument should contain a vector of threshold values.
#' @return A matrix of different similarity metrics at different orders of diversity .
#' @seealso \code{\link{breadth}}
#' @examples
#' data()
#' overlap_profile(rasters)
#' overlap_profile(rasters,qvalues=seq(from = 0, to = 5, by = 1))
#' breadth_profile(rasters,qvalues=seq(from = 0, to = 5, by = 1), metric=C)
#' @references
#' Alberdi, A., Novella-Fernandez R., Aizpurua, O., Razgour, O. (2019). Measuring breadth and overlap of spatial projections of environmental niche models based on Hill numbers.\cr\cr
#' Alberdi, A., Gilbert, M.T.P. (2019). A guide to the application of Hill numbers to DNA-based diversity analyses. Molecular Ecology Resources, 19, 804-817.\cr\cr
#' @export

overlap_profile <- function(rasters,qvalues,metric,thresholds){

#Quality-check and warnings
if(missing(rasters)) stop("Spatial projection data are missing. Please, provide them as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(class(rasters) != "RasterStack") stop("Overlap function requires a RasterStack (multiple projections) object containing at least two rasters")
if(length(names(rasters)) < 2) stop("Overlap function requires a RasterStack (multiple projections) object containing at least two rasters")
if(missing(qvalues)) stop("q value is missing")

if(missing(metric)) {metric = c("C","U","V","S")}
if(missing(thresholds)) {thresholds = rep(0,length(names(rasters)))}

#Overlap profile
overlap.profile <- c()
  for (q in qvalues){
    cat("q =",q,"\n")
    if(q== 1){q=0.999999}
    overlap <- overlap(rasters,qvalue=q,metric=metric,thresholds=thresholds)
    row <- unlist(overlap)
    overlap.profile <- rbind(overlap.profile,row[-c(1:3)])
  }
  rownames(overlap.profile) <- qvalues

return(overlap.profile)

}
