#' Spatial overlap and turnover-complement computation based on Hill numbers
#' @title Spatial overlap and turnover-complement
#' @author Antton Alberdi, \email{anttonalberdi@gmail.com}
#' @keywords breadth diversity hill
#' @description Compute Hill numbers-based spatial overlap and turnover-complement metrics on multiple Environmental Niche Model projection rasters.
#' @param raster A RasterStack (multiple projections) object containing ENM projections with suitability scores.
#' @param qvalue A positive number, usually between 0 and 5, but most commonly 0, 1 or 2. It can be an integer or contain decimals.
#' @param metric A vector indicating the similarity metrics to be computed. Default: metric=c("C","U","S","V")
#' @param threshold Suitability value(s) below which all values are converted into zeros. The argument should contain a vector of threshold values sorted as the RasterStack model projections.
#' @return Alpha, gamma and beta values and similarity metrics.
#' @seealso \code{\link{breadth}}
#' @examples
#' data()
#' overlap(rasters,qvalue=1)
#' overlap(rasters,qvalue=1,metric="C",thresholds=thresholds)
#' @references
#' Alberdi, A., Novella-Fernandez R., Aizpurua, O., Razgour, O. (2019). Measuring breadth and overlap of spatial projections of environmental niche models based on Hill numbers.\cr\cr
#' Alberdi, A., Gilbert, M.T.P. (2019). A guide to the application of Hill numbers to DNA-based diversity analyses. Molecular Ecology Resources, 19, 804-817.\cr\cr
#' @export

overlap <- function(rasters,qvalue,metric,thresholds){

#Quality-check and warnings
if(missing(rasters)) stop("Spatial projection data are missing. Please, provide them as RasterLayer (single projection) or RasterStack (multiple projections) objects")
if(class(rasters) != "RasterStack") stop("Overlap function requires a RasterStack (multiple projections) object containing at least two rasters")
if(length(names(raster)) < 2) stop("Overlap function requires a RasterStack (multiple projections) object containing at least two rasters")
if(missing(qvalue)) stop("q value is missing")
if(qvalue == 1){qvalue=0.999999}
if(missing(metric)) {metric = c("C","U","V","S")}
if(missing(thresholds)) {thresholds = rep(0,length(names(rasters)))}

#Convert to cell matrix
cellmatrix <- cell_table(raster=rasters,threshold=thresholds)
weight= rep(1/ncol(cellmatrix),ncol(cellmatrix))
cellmatrix.weight <- sweep(cellmatrix,2,weight,"*")
N <- length(weight)

#Functions
alphafun <- function(cellmatrix.weight,qvalue,weight){
  cellmatrix.weight.q <- cellmatrix.weight^qvalue
  cellmatrix.weight.q[!cellmatrix.weight] <- 0
  N <- length(weight)
  sum(rowSums(cellmatrix.weight.q))^(1/(1-qvalue))/N
}
gammafun <- function(cell.matrix,qvalue,weight){
  sum(rowSums(cellmatrix.weight)^qvalue)^(1/(1-qvalue))
}

CqN <- function(beta,qvalue,N){
if(qvalue==1){qvalue=0.99999}
value = ((1/beta)^(qvalue-1) - (1/N)^(qvalue-1)) / (1 - (1/N)^(qvalue-1))
return(value)
}

SqN <- function(beta,N){
value = ((1/beta) - 1/N)/(1-1/N)
return(value)
}

UqN <- function(beta,qvalue,N){
if(qvalue==1){qvalue=0.99999}
value = ((1/beta)^(1-qvalue) - (1/N)^(1-qvalue)) / (1 - (1/N)^(1-qvalue))
return(value)
}

VqN <- function(beta,N){
value = (N - beta)/(N-1)
return(value)
}

#Diversity partitioning
alpha <- alphafun(cellmatrix.weight,qvalue,weight)
gamma <- gammafun(cellmatrix.weight,qvalue,weight)
beta <- gamma/alpha

results <- list(alpha=alpha,gamma=gamma,beta=beta)
if ("C" %in% metric){results <- append(results, list(CqN=CqN(beta,qvalue,N)))}
if ("S" %in% metric){results <- append(results, list(SqN=SqN(beta,N)))}
if ("U" %in% metric){results <- append(results, list(UqN=UqN(beta,qvalue,N)))}
if ("V" %in% metric){results <- append(results, list(VqN=VqN(beta,N)))}

return(results)
}
