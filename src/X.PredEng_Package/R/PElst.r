#' PElst class
#'
#' Provides a list-like class that can be used to control the contents more strictly. Gratutiously stolen
#' from FLR's FLlst concept.
#'
#' @exportClass PElst
#'
setClass("PElst", 
         contains="list",
         validity=function(object){
           msg <- NULL
           #Duplicated names check
           if(any(duplicated(purrr::map(object@.Data,slot,"name")))) {
             msg <- c(msg,"Names of objects in  must be unique.")} 
           #All elements of the same class
           if(length(unique(purrr::map(object, is))) > 1)
           {msg <- c(msg,"PElst elements must be of the same class")}
           # ALL elements in the list are validObjects themselves
           if(!all(purrr::map_lgl(object, validObject))) {
             {msg <- c(msg,"Components must be valid objects themselves (validObject == TRUE)")}
           }
           # Return
           if(length(msg)==0) return(TRUE) else msg
         }
) # }}}



#' @export
setMethod("c", signature(x="PElst"),
          function(x,...) {
            x@.Data <- c(x@.Data,...)
            return(x)
          }
) 

#' @export
setMethod("names", signature(x="PElst"),
          function(x) {
            return(purrr::map_chr(x@.Data,slot,"name"))
          }
) 

#' @export
setMethod("[", signature(x="PElst",i="character"),
                 function(x, i){
                   lst <- x@.Data
                   names(lst) <- purrr::map_chr(x@.Data,slot,"name")
                   lst <- lst[i]
                   names(lst) <- NULL
                   x@.Data <- lst
                   return(x)})

#' @export
setMethod("[", signature(x="PElst",i="numeric"),
          function(x, i){
            x@.Data <- x@.Data[i]
            return(x)})


setGeneric("PElst",function(x,...) standardGeneric("PElst"))

#' @export
setMethod("PElst",signature(x="list"),
          function(x) {
            rtn <- PElst()
            rtn@.Data <- x
            return(rtn)
          })

#' @export
setMethod("PElst",signature(x="missing"),
          function(x) {
            rtn <- new("PElst")
            return(rtn)
          })
