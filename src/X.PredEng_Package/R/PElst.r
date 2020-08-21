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
           #Check all elements of the same (root) class
           lst.classes <- purrr::map(object, is)
           unique.classes <- unique(unlist(lst.classes))
           exists.common.class <- #Is there at least one class common to all?
             map_lgl(unique.classes,function(cls) {
               all(map_lgl(object,is,cls))
             })
           
           err.msg <- list(
             #Duplicated names check
             validate_that(all(!duplicated(purrr::map(object@.Data,slot,"name"))),
                           msg="Names of objects must be unique."),
             validate_that(any(exists.common.class) | length(object)==0,
                           msg="PElst elements must be of the same class"),
             # ALL elements in the list are validObjects themselves
             validate_that(all(purrr::map_lgl(object, validObject)),
                           msg="Components must be valid objects themselves (validObject == TRUE)"))
           
           #Return
           err.idxs <- map_lgl(err.msg,is.character)
           if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
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

#' @export
setMethod("[[", signature(x="PElst",i="character"),
          function(x, i){
            lst <- x@.Data
            names(lst) <- purrr::map_chr(x@.Data,slot,"name")
            lst <- lst[[i]]
            names(lst) <- NULL
            return(lst)})

#' @export
setMethod("[[", signature(x="PElst",i="numeric"),
          function(x, i){
            return(x@.Data[[i]])})


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
