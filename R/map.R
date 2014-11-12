map <- function(...){
    ret = list(...)
    ret$layers = list()
    class(ret) <- c("gis","map")
    ret
}

base_layer = function(...){
    ret = list(...)
    class(ret) <- c("gis","base")
    ret
}

layer <- function(obj, extend=FALSE, style=plain(), ...){
    ret = list(obj=obj, extend=extend, style=style)
    class(ret) <- c("gis","layer")
    ret
}


"+.gis" <- function(e1,e2){
    if(inherits(e2,"layer")){
        e1$layers = c(e1$layers, list(e2))
    }
    if(inherits(e2,"base")){
        if(!is.null(e1$base)){
            warning("Replacing base layer")
        }
        e1$base = e2
    }
    
    class(e1) <- c("gis","map")
    e1
}

print.gis <- function(x,...){
    cat("Map\n\n")
    cat("Layers: ",length(x$layers),"\n")
    if(!is.null(x$base)){
        cat("Base Layer\n")
    }else{
        cat("No Base Layer\n")
    }
    
}

setOldClass("gis")

setMethod("extent", signature(x="gis"),  function(x,...){
    
}
)
mapbasic <- function(map,...){
    cat("plotting map using base graphics...\n")
    first=TRUE
    for(layer in map$layers){
        if(inherits(layer$obj,"Spatial")){
            style=layer$style(layer$obj)
            if(first){
                plot(layer$obj, col=style$colour )
                first = FALSE
            }else{
                plot(layer$obj,add=TRUE, col=style$colour)
            }
        }else if(inherits(layer,"Raster")){
            stop("cant plot raster")
        }
            
    }
}

mapgg <- function(map,...){
    cat("plotting map using ggplot2 graphics...\n")
}

mapleaflet <- function(map,...){
    cat("plotting map using leaflet web...\n")
}


    
    
