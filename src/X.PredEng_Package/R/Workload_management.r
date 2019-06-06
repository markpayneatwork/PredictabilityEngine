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
#' partition by space. However, in some cases (e.g. calculation of statistics) it is useful to have this 
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
  
  #Get the list of all valid data types
  dat.srcs.l <- unlist(lapply(c("Decadal","NMME","Observations","CMIP5"),slot,object=obj))
  dat.srcs <- tibble(src.type=sapply(dat.srcs.l,slot,"type"),
                     src.id=sapply(dat.srcs.l,slot,"id"))
  other.srcs.l <- list()
  other.srcs.l[[1]] <- tibble(src.type="Persistence",src.id=obj@Observations@id)
  if(length(obj@NMME)!=0) other.srcs.l[[2]] <- tibble(src.type="NMME",src.id=PE.cfg$files$ensmean.name)
  if(length(obj@Decadal)!=0) other.srcs.l[[3]] <- tibble(src.type="Decadal",src.id=PE.cfg$files$ensmean.name)
  all.srcs <- bind_rows(dat.srcs,other.srcs.l)
  
  #If we have global statistics in the mix, then we need to handle this accordingly
  global.stats.present <- any(sapply(obj@statistics,slot,name="is.global.stat"))
  
  #Retain the data sources requested
  if(src.slot=="Stats") {
    dat.srcs <- all.srcs
    out.prefix <- src.slot
  } else if(ensmean) {
    dat.srcs <- filter(all.srcs,src.type==src.slot,src.id==PE.cfg$files$ensmean.name)
    out.prefix <- sprintf("%s_Ensmean",src.slot)
  } else {
    dat.srcs <- filter(all.srcs,src.type==src.slot,src.id!=PE.cfg$files$ensmean.name)
    out.prefix <- src.slot
  }
  if(nrow(dat.srcs)==0) return(NULL)  #Catch blanks
  dat.srcs$src.num <- seq(nrow(dat.srcs))
    
  #Setup spatial domains
  if(partition.by.space ) { #Overrides the use.global.ROI switch
    if(global.stats.present & src.slot=="Stats") {
      sp.subdomains <- c(names(obj@spatial.subdomains),NA)  #Add a global spatial config
    } else {
      sp.subdomains <- names(obj@spatial.subdomains)
    }
  } else if (obj@use.global.ROI) {
    sp.subdomains <- as.character(NA)
  } else {
    sp.subdomains <- names(obj@spatial.subdomains)
  }
  
  #Do the expansion
  work.cfg <- expand.grid(src.num=dat.srcs$src.num,
                          sp=sp.subdomains) %>%
    left_join(dat.srcs,by="src.num") %>%
    select(-src.num) %>%
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
  if(is.na(this.cfg$src.id) | is.na(this.cfg$src.type)) {
    stop("Source not defined for this configuration")
  }
  if(this.cfg$src.type=="Persistence"|this.cfg$src.id==PE.cfg$files$ensmean.name) {
    this.src <- data.source(name=this.cfg$src.id,type=this.cfg$src.type)  
  } else if(this.cfg$src.type=="Observations") {
    this.src <- obj@Observations
  } else  {
    srcs <- slot(obj,this.cfg$src.type)
    this.src <- srcs[[as.character(this.cfg$src.id)]]
  }
  return(this.src)
  
}

#' @export
#' @rdname job_management
get.this.sp <- function(fname,cfg.idx,obj){
  cfgs <- get.this.cfgs(fname)
  this.cfg <- cfgs[cfg.idx,]
  if(is.na(this.cfg$sp)) {
    this.sp  <- spatial.domain(obj@global.ROI,name="",desc="global.ROI")
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
