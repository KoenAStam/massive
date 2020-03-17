#' Reads .fcs files
#'
#' @description
#' A wrapper function around \code{\link[flowCore]{read.FCS}} to read .fcs
#' files. It will only keep relevant channels and renames them according to
#' the `FCSMeta` parameter.
#'
#'
#' @param path A character string indicating the location of the .fcs files. The
#' default corresponds to the working directory
#' @param FCSMeta A list holding meta information on the .fcs files. The default
#' is set to \code{\link[massive]{readFCSMeta}}
#' @param asinh5 Logical, should the data be transformed with the arc-sinh
#' cofactor 5
#' @param downsampling An integer that indicates the size to downsample to.
#' If not supplied, no downsampling will be performed.
#'
#' @keywords read, data, FCS, files
#'
#' @examples
#' FCSDir <- system.file("extdata", package="massive")
#' FCSData <- readFCSFiles(path=FCSDir)
#'
#' @export
readFCSFiles <- function(path=".", FCSMeta, asinh5=TRUE, downsampling){
    FCSPaths <- list.files(path=path, pattern=".fcs", full.names=TRUE)
    FCSNames <- list.files(path=path, pattern=".fcs", full.names=FALSE)

    if(length(FCSPaths) == 0){
        stop("path contains no .fcs files")
    }

    FCSexpr <- data.frame()

    if(missing(FCSMeta)){
        warning("FCSMeta is missing and set to default: readFCSMeta()")
        FCSMeta <- readFCSMeta(path)
    }

    channels <- FCSMeta$channels
    keepChannels <- channels[channels[,"markerClass"] %in%
        c("lineage", "type", "cytokine"),]

    for(i in seq_along(FCSNames)){

        cat("\rreading FCS file: ", i)

        if(!missing(downsampling)){
            nSample <- min(FCSMeta[["FCSTable"]][i,"events"],
                           downsampling)

            if(nSample < downsampling){
                warning("downsampling size exceeds number of events, set to max")
            }
        } else {
            nSample = NULL
        }

        FCSFile <- flowCore::read.FCS(FCSPaths[[i]],
                                      transformation=FALSE,
                                      truncate_max_range=FALSE,
                                      which.lines=nSample)

        exprs <- FCSFile@exprs[,keepChannels[,"channelID"]]
        if(asinh5 == TRUE){
            exprs <- asinh(exprs/5)
        }
        colnames(exprs) <- keepChannels[,"markerID"]

        FCSexpr <- rbind(FCSexpr,
                         data.frame(sampleID=FCSNames[i],
                                    exprs))

        if(i == length(FCSNames)){
            cat("\rreading FCS file: done")
        }
    }

    return(FCSexpr)
}


