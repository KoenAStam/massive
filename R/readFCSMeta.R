#' Create data table from .fcs files
#'
#' @export
readFCSMeta <- function(path = ".", panel){
    FCSPaths <- list.files(path=path, pattern=".fcs", full.names=TRUE)
    FCSNames <- list.files(path=path, pattern=".fcs", full.names=FALSE)

    if(length(FCSPaths) == 0){
        stop("path contains no .fcs files")
    }

    FCSMeta <- data.frame(FCSFiles=FCSNames,
                           date=character(length(FCSNames)),
                           channels=numeric(length(FCSNames)),
                           events=numeric(length(FCSNames)),
                           stringsAsFactors=FALSE)

    FCSHeaders <- flowCore::read.FCSheader(files=FCSNames,
                                          path=path)

    channelID <- paste0("$P", seq_len(FCSHeaders[[1]]["$PAR"]), "N")
    markerID <- paste0("$P", seq_len(FCSHeaders[[1]]["$PAR"]), "S")

    for(i in seq_along(FCSNames)){
        FCSMeta[i, 2] <- FCSHeaders[[i]]["$DATE"]
        FCSMeta[i, 3] <- FCSHeaders[[i]]["$PAR"]
        FCSMeta[i, 4] <- FCSHeaders[[i]]["$TOT"]
    }

    channels <- data.frame(channelID=unname(FCSHeaders[[1]][channelID]),
                           markerLabel=unname(FCSHeaders[[1]][markerID]),
                           markerClass="unknown",
                           stringsAsFactors=FALSE)

    for(i in seq_len(nrow(channels))){

        if(missing(panel)){
            panel <- data(markers)
        }

        markerClean <- gsub("[[:punct:]]|[[:space:]]", "",
                            channels[i,"markerLabel"])
        markerNr <- match(toupper(markerClean), panel[,1])

        if(is.na(markerNr)){
            warning(markerClean, " could not be matched to the panel")
            next
        }
        channels[i, 2:3] <- as.character(panel[markerNr, 2:3])
    }

    return(list(path=path,
                FCSMeta=FCSMeta,
                channels=channels))
}

