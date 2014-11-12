
#' style map
#'
#'  function mapping features to style
#'

plain <- function(point=list(colour="black", size=1, symbol=1),
                  poly = list(colour="white",pattern=1),
                  line = list(colour="black", width=1, pattern=1)
                  ){
    force(point);force(poly);force(line)
    style = function(features){
        n = nrow(features)
        if(inherits(features,"SpatialPoints")){
            ret = list(
                colour = rep(point$colour, n),
                size = rep(point$size, n),
                symbol = rep(point$symbol, n)
                )
        }else if(inherits(features,"SpatialPolygons")){
            ret = list(
                outline=list(
                    colour = rep(line$colour, n),
                    width = rep(line$width, n),
                    pattern = rep(line$pattern, n)
                    ),
                fill = list(
                    colour = rep(poly$colour, n),
                    pattern = rep(poly$pattern, n)
                    )
                )
        }else if(inherits(features, "SpatialLines")){
            ret = list(
                colour = rep(line$colour, n),
                width = rep(line$width, n),
                pattern = rep(line$pattern, n)
                )
        }
    
        ret
                
    }
    return(style)
}

