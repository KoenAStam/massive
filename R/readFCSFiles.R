#'
#'
readFCS <- function(path=".", downsampling, FCSMeta, asinh5=TRUE){
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


