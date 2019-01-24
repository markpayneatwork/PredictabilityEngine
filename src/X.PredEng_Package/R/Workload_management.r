#' Manage job distribution
#'
#' @param fname Filename (incluiding path) to read / write job configuration to/from
#'
#' @param obj A PredEng.config object
#' @param src.slots Vector containg the name(s) of the slot(s) over which to partition 
#' the workload
#' @param partition.by.space Should the work be split up by spatial subdomain as well as data.src? The default
#' behaviour is to follow the use.global.ROI slot in obj i.e. when use.global.ROI is TRUE, we don't want to 
#' partition by space. However, in some cases (e.g. calculation of summary statistics) it is useful to have this 
#' behaviour anyway.
#' @param include.ensmeans Should ensemble means be included as well?
#' @param include.persistence Should persistence forecasts be added?
#'
#' @export
#' @name job_management
partition.workload <- function(fname,
                               obj,
                               src.slots,
                               partition.by.space=!obj@use.global.ROI,
                               include.ensmeans=FALSE,
                               include.persistence=FALSE) {
  #Extract lists
  if(all(is.na(src.slots))) {
    dat.srcs <- tibble(src.type=NA,src.name=NA)
    
  } else {
    dat.srcs.l <- unlist(lapply(src.slots,slot,object=obj))
    dat.srcs <- tibble(src.type=sapply(dat.srcs.l,slot,"type"),
                       src.name=sapply(dat.srcs.l,slot,"name"))
    
    #Handle ensemble means here
    if(include.ensmeans & "Decadal" %in% src.slots) {
      dat.srcs <- rbind(dat.srcs,
                        tibble(src.type="Decadal",src.name=PE.cfg$files$ensmean.name))
    }
    if(include.ensmeans & "NMME" %in% src.slots) {
      dat.srcs <- rbind(dat.srcs,
                        tibble(src.type="NMME",src.name=PE.cfg$files$ensmean.name))  }
    if(include.persistence ) {
      dat.srcs <- rbind(dat.srcs,
                        tibble(src.type="Persistence",src.name=pcfg@Observations@name))
    }
  }
  dat.srcs$src.id <- seq(nrow(dat.srcs))
  
  #Setup spatial domains
  if(partition.by.space ) { #Overrides the use.global.ROI switch
    sp.subdomains <- names(obj@spatial.subdomains)
  } else if (obj@use.global.ROI) {
    sp.subdomains <- as.character(NA)
  } else {
    sp.subdomains <- names(obj@spatial.subdomains)
  }
  
  #Do the expansion
  work.cfg <- expand.grid(src.id=dat.srcs$src.id,
                          sp=sp.subdomains) %>%
    left_join(dat.srcs,by="src.id") %>%
    select(-src.id) %>%
    add_column(cfg.id=seq(nrow(.)),.before=1) %>%
    as.tibble()
  
  #Save file
  write_csv(work.cfg,path = fname)
}

#' @param cfg.idx Configuration index, indicating which job configration to extract
#'
#' @export
#' @rdname job_management
get.this.src <- function(fname,cfg.idx,obj){
  cfgs <- get.this.cfgs(fname)
  this.cfg <- cfgs[cfg.idx,]
  if(is.na(this.cfg$src.name) | is.na(this.cfg$src.type)) {
    stop("Source not defined for this configuration")
  }
  if(this.cfg$src.type=="Persistence"|this.cfg$src.name==PE.cfg$files$ensmean.name) {
    this.src <- data.source(name=this.cfg$src.name,type=this.cfg$src.type)  
  } else if(this.cfg$src.type=="Observations") {
    this.src <- obj@Observations
  } else  {
    srcs <- slot(obj,this.cfg$src.type)
    this.src <- srcs[[as.character(this.cfg$src.name)]]
  }
  return(this.src)
  
}

#' @export
#' @rdname job_management
get.this.sp <- function(fname,cfg.idx,obj){
  cfgs <- get.this.cfgs(fname)
  this.cfg <- cfgs[cfg.idx,]
  if(is.na(this.cfg$sp)) {
    this.sp  <- spatial.domain(obj@global.ROI,name="")
  } else { #Working with subdomains
    this.sp <- obj@spatial.subdomains[[as.character(this.cfg$sp)]]
  }
  return(this.sp)
}

#' @export
#' @rdname job_management
get.this.cfgs <- function(fname){
  cfgs <- read.csv(fname)
  return(cfgs)
}
