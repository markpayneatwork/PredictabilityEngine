#' Manage job distribution
#'
#' @param fname Filename (incluiding path) to read / write job configuration to/from
#'
#' @param obj A PredEng.config object
#' @param src.slot Vector containg the name of the slot over which to partition 
#' the workload. In the case where this is set to "SumStats", work is partitioned
#' over all relevant model data slots
#' @param ensmean Treat this as an ensemble mean?
#' @param partition.by.space Should the work be split up by spatial subdomain as well as data.src? The default
#' behaviour is to follow the use.global.ROI slot in obj i.e. when use.global.ROI is TRUE, we don't want to 
#' partition by space. However, in some cases (e.g. calculation of summary statistics) it is useful to have this 
#' behaviour anyway.
#'
#' @export
#' @name job_management
partition.workload <- function(obj,
                               src.slot,
                               ensmean=FALSE,
                               partition.by.space=!obj@use.global.ROI) {
  
  #Check inputs
  if(length(src.slot)!=1) stop("Can only partition a single slot at a time")
  
  #Get the list of all valid types
  dat.srcs.l <- unlist(lapply(c("Decadal","NMME","Observations","CMIP5"),slot,object=obj))
  dat.srcs <- tibble(src.type=sapply(dat.srcs.l,slot,"type"),
                     src.name=sapply(dat.srcs.l,slot,"name"))
  other.srcs.l <- list()
  other.srcs.l[[1]] <- tibble(src.type="Persistence",src.name=obj@Observations@name)
  if(length(obj@NMME)!=0) other.srcs.l[[2]] <- tibble(src.type="NMME",src.name=PE.cfg$files$ensmean.name)
  if(length(obj@Decadal)!=0) other.srcs.l[[3]] <- tibble(src.type="Decadal",src.name=PE.cfg$files$ensmean.name)
  all.srcs <- bind_rows(dat.srcs,other.srcs.l)
  
  #Retain the data sources requested
  if(src.slot=="SumStats") {
    dat.srcs <- all.srcs
    out.prefix <- src.slot
  } else if(ensmean) {
    dat.srcs <- filter(all.srcs,src.type==src.slot,src.name==PE.cfg$files$ensmean.name)
    out.prefix <- sprintf("%s_Ensmean",src.slot)
  } else {
    dat.srcs <- filter(all.srcs,src.type==src.slot,src.name!=PE.cfg$files$ensmean.name)
    out.prefix <- src.slot
  }
  if(nrow(dat.srcs)==0) return(NULL)  #Catch blanks
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
    as_tibble()
  
  #Save file
  out.dir <- define_dir(PE.cfg$dirs$job.cfg,out.prefix)
  
  write_csv(work.cfg,path = file.path(PE.cfg$dirs$job.cfg,sprintf("%s.cfg",out.prefix)))
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
