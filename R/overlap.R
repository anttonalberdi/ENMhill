#' Spatial overlap computation based on Hill numbers
#' @title Spatial turnover
#' @author Antton Alberdi, \email{anttonalberdi@gmail.com}
#' @keywords breadth diversity hill
#' @description Compute Hill numbers-based spatial overlap on multiple Environmental Niche Model projection rasters.
#' @param raster A RasterLayer (single projection) or RasterStack (multiple projections) object containing ENM projection(s) with suitability scores.
#' @param qvalue A positive number, usually between 0 and 5, but most commonly 0, 1 or 2. It can be an integer or contain decimals.
#' @param relative Whether to compute absolute or relative breadths. Default=FALSE
#' @param threshold Suitability value(s) below which all values are converted into zeros. If a RasterStack (multiple projections) is used, the argument should contain a vector of threshold values.
#' @return A single or multiple spatial breadth measures.
#' @seealso \code{\link{breadth}}, \code{\link{turnover}}
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

overlap <- function(){
}
