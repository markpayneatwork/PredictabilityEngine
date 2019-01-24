#' Title
#'
#' @export PredEng.list
#'
PredEng.list <- setClass("PredEng.list", contains="list",
                         validity = function(object) {
                           if(any(is.null(names(object)))) {
                              "Object must have names"
                           } else { TRUE}
                         })

# setMethod("names", signature(x="PredEng.list"),
#           function(x)
#             sapply(x, slot,'name')
# )
# 
setMethod("[", signature(x="PredEng.list", i="ANY", j="missing", drop="ANY"),
          function(x, i, drop) {
            lst <- as(x, "list")
            # names dropped!
            names(lst) <- names(x)
            lst <- lst[i]
            new(is(x), lst)
          }) # }}}

setReplaceMethod("[", signature(x="PredEng.list", i="ANY", j="missing", value="ANY"),
                 function(x, i, j, value)
                 {
                   nms <- names(x)
                   idx <- seq(length(x))

                   x@.Data[i] <- value
                   names(x)[idx] <- nms

                   return(x)
                 }
) # }}}

