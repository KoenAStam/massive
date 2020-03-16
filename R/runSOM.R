#'
#'
runSOM <- function(FCSData, xdim=15, ydim=15, iter=10){

    x <- as.matrix(fcsData[,setdiff(colnames(x), "sampleID")])
    somGrid <- somgrid(xdim, ydim, topo="hexagonal")
    somModel <- som(x,
                    grid=somGrid,
                    rlen=iter,
                    alpha=c(0.05,0.01),
                    keep.data=FALSE)

    return(somModel)
}
