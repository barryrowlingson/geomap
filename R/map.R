map <- function(...){
    ret = list(...)
    ret$layers = list()
    ret$extent = NULL
    class(ret) <- c("geomap","map")
    ret
}

base_layer <- function(...){
    ret = list(...)
    class(ret) <- c("geomap","base")
    ret
}

layer <- function(obj, extend=FALSE, style=plain(), ...){
    ret = list(obj=obj, extend=extend, style=style)
    class(ret) <- c("geomap","layer")
    ret
}


"+.geomap" <- function(e1,e2){
    if(inherits(e2,"layer")){
        e1$layers = c(e1$layers, list(e2))
    }
    if(inherits(e2,"base")){
        if(!is.null(e1$base)){
            warning("Replacing base layer")
        }
        e1$base = e2
    }
    
    class(e1) <- c("geomap","map")
    e1
}

print.geomap <- function(x,...){
    cat("Map\n\n")
    cat("Layers: ",length(x$layers),"\n")
    if(!is.null(x$base)){
        cat("Base Layer\n")
    }else{
        cat("No Base Layer\n")
    }
}

#' "geomap" class
#'
#' @name geomap-class
#' @aliases geomap
#'
#' @exportClass geomap
#' 
setOldClass("geomap")


setMethod("extent", signature(x="geomap"),
          function(x,...){
              ret = NULL
              for(layer in x$layers){
                  if(layer$extend){
                      ret = raster::merge(extent(layer$obj),ret)
                  }
              }
              ret
          }
          )

mapbasic <- function(map,...){
    cat("plotting map using base graphics...\n")
    e = extent(map)
    plot(e, type="n", axes=FALSE, xlab="", ylab="", asp=1)
    for(layer in map$layers){
        if(inherits(layer$obj,"Spatial")){
            style=layer$style(layer$obj)
            plot(layer$obj,add=TRUE, col=style$colour)
        }else if(inherits(layer$obj,"Raster")){
            plot(layer$obj, add=TRUE, legend=FALSE)
        }
            
    }
}

mapgg <- function(map,...){
    cat("plotting map using ggplot2 graphics...\n")
}

mapleaflet <- function(map,...){
    cat("plotting map using leaflet web...\n")
}


    
    
