#' Run SOM
#'
#' @export
runSOM <- function(x, xdim=15, ydim=15, iter=20){

    x <- as.matrix(x[,setdiff(colnames(x), "sampleID")])
    somGrid <- somgrid(xdim, ydim, topo="hexagonal")
    somModel <- som(x,
                    grid=somGrid,
                    rlen=iter,
                    alpha=c(0.05,0.01),
                    keep.data=TRUE)

    return(somModel)
}
