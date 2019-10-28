#' Manage job distribution
#'
#' @param fname Filename (incluiding path) to read / write job configuration to/from
#'
#' @param obj A PredEng.config object
#' @param src.slot Name of the slot over which to partition the workload. In the case where
#'  this is set to "Stats", work is partitioned over all relevant model data slots
#' @param data.partition.type "ensmean", "source", or "chunk"
#' @param space.partition Should the work be split up by spatial subdomain? The default
#' behaviour is to follow the use.global.ROI slot in obj i.e. when use.global.ROI is TRUE, we don't want to 
#' partition by space. However, in some cases (e.g. calculation of statistics) it is useful to have this 
#' behaviour anyway.
#'
#' @export
#' @name job_management
partition.workload <- function(obj,
                               src.slot="missing",
                               data.partition.type="",
                               space.partition=!obj@use.global.ROI) {
  
  #Check inputs
  if(length(src.slot)!=1) stop("Can only partition a single slot at a time")
  
  #Get the list of all valid data types and chunks
  obj.srcs <- 
    tibble(src.type=c("Decadal","NMME","Observations","CMIP5")) %>%
    mutate(src=map(src.type,~slot(.x,object=obj)),
           n.objs=map_int(src,length),
           src=map2(src,n.objs,~ if(.y<=1) {list(.x)} else {.x})) %>%
    filter(n.objs!=0)%>%
    unnest(src) %>%
    mutate(src.name=map_chr(src,slot,"name"),
           n.chunks=map_dbl(src,slot,"n.chunks")) %>%
    select(-src,-n.objs)
    
  #Manually add other sources  
  other.srcs.l <- list()
  other.srcs.l[[1]] <- tibble(src.type="Persistence",src.name=obj@Observations@name)
  if(length(obj@NMME)!=0) other.srcs.l[[2]] <- tibble(src.type="NMME",src.name=PE.cfg$files$ensmean.name)
  if(length(obj@Decadal)!=0) other.srcs.l[[3]] <- tibble(src.type="Decadal",src.name=PE.cfg$files$ensmean.name)
  all.srcs <- bind_rows(obj.srcs,other.srcs.l) 

  #If we want to use some stats globally, then we need to make sure to include the Global spatial domain
  using.stats.globally <- any(map_lgl(obj@statistics,slot,name="use.globally"))
  
  #Retain the data sources requested
  if(missing(src.slot)) { #If nothing, then return all sources
    these.srcs <- all.srcs
    out.prefix <- src.slot
  } else if(src.slot %in% c("Observations") | is.na(data.partition.type)) {
    these.srcs <- 
      all.srcs %>%
      filter(src.type==src.slot)
    out.prefix <- src.slot
  } else if(toupper(data.partition.type)=="ENSMEAN") {
    these.srcs <- 
      all.srcs %>%
      filter(src.type==src.slot,
             src.name==PE.cfg$files$ensmean.name)
    out.prefix <- sprintf("%s_ensmean",src.slot)
  } else if(toupper(data.partition.type)=="CHUNKS") {
    these.srcs <- 
      all.srcs %>%
      filter(src.type==src.slot,
             src.name!=PE.cfg$files$ensmean.name) %>%
      mutate(chunk.id=map(n.chunks,~seq(.x))) %>%
      unnest(chunk.id)
    out.prefix <- sprintf("%s_by_chunks",src.slot)
  } else if(toupper(data.partition.type)=="SOURCES") {
    these.srcs <- filter(all.srcs,src.type==src.slot,src.name!=PE.cfg$files$ensmean.name)
    out.prefix <- sprintf("%s_by_sources",src.slot)
  } else {
    stop(sprintf('Unknown data.partition.type "%s"',data.partition.type))
  }
  if(nrow(these.srcs)==0) return(NULL)  #Catch blanks
  these.srcs$src.num <- seq(nrow(these.srcs))
    
  #Setup spatial domains
  if(space.partition ) { #Overrides the use.global.ROI switch
    if(using.stats.globally & src.slot=="Stats") {
      #Then need to add a global spatial config as well
      sp.subdomains <- c(names(obj@spatial.subdomains),PE.cfg$misc$global.sp.name)
    } else {
      #Otherwise just use the supplied spatial domains
      sp.subdomains <- names(obj@spatial.subdomains)
    }
  } else if (obj@use.global.ROI) {
    #Spatial domains are not relevant in this case - flag with an NA
    sp.subdomains <- as.character(NA)
  } else {
    sp.subdomains <- names(obj@spatial.subdomains)
  }
  
  #Do the expansion
  work.cfg <- expand.grid(src.num=these.srcs$src.num,
                          sp=sp.subdomains) %>%
    left_join(these.srcs,by="src.num") %>%
    dplyr::select(-src.num,-n.chunks) %>%
    add_column(cfg.id=seq(nrow(.)),.before=1) %>%
    as_tibble()
  
  #Save file / return value
  if(missing(src.slot)) {
    return(work.cfg)
  } else {
    out.dir <- define_dir(PE.cfg$dirs$job.cfg,out.prefix)
    write_csv(work.cfg,path = file.path(PE.cfg$dirs$job.cfg,sprintf("%s.cfg",out.prefix)))
  }
}

#' @param cfg.idx Configuration index, indicating which job configration to extract
#'
#' @export
#' @rdname job_management
configure.src <- function(fname,cfg.idx,obj){
  cfgs <- get.cfgs(fname)
  this.cfg <- 
    cfgs %>%
    filter(cfg.id==cfg.idx)
  stopifnot(nrow(this.cfg)==1)
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

#' @param cfg.idx Configuration index, indicating which job configration to extract
#'
#' @export
#' @rdname job_management
configure.chunk <- function(fname,cfg.idx,obj){
  #Get the source
  this.src <- configure.src(fname,cfg.idx,obj)
  
  #Now configure the chunks as well
  cfgs <- get.cfgs(fname)
  this.cfg <- 
    cfgs %>%
    filter(cfg.id==cfg.idx)
  stopifnot(nrow(this.cfg)==1)

  #Take care of the chunking, if necessary - only return the
  #sources in the corresponding chunk
  if(!("chunk.id"%in% names(this.cfg))) stop("Chunking not defined for this configuration")
  this.src@sources <- this.src@sources[this.cfg$chunk.id]
  this.src@chunk.id <- names(this.src@sources)
  return(this.src)
}


#' @export
#' @rdname job_management
configure.sp <- function(fname,cfg.idx,obj){
  cfgs <- get.cfgs(fname)
  this.cfg <- 
    cfgs %>%
    filter(cfg.id==cfg.idx)
  stopifnot(nrow(this.cfg)==1)
  if(is.na(this.cfg$sp) | this.cfg$sp==PE.cfg$misc$global.sp.name) {
    this.sp  <- global.ROI(obj)
  } else { #Working with subdomains
    this.sp <- obj@spatial.subdomains[[as.character(this.cfg$sp)]]
  }
  return(this.sp)
}

#' Create a global ROI
#'
#' @param obj 
#'
#' @export
#'
global.ROI <- function(obj) {
  spatial.domain(obj@global.ROI,name=PE.cfg$misc$global.sp.name,desc="global.ROI")
}

#' @export
#' @rdname job_management
get.cfgs <- function(fname){
  cfgs <- read_csv(fname,col_types = cols())
  return(cfgs)
}

#' Get Subdomain directory
#' 
#' Gets the scratch directory in which to place any subdomain processing
#'
#' @param cfg A PredEng config object 
#' @param sp  A Spatial configuration object
#'
#' @return Path to the scratch directory
#' @export
get.subdomain.dir <- function(cfg,sp) {
  if(cfg@use.global.ROI) {
    return(cfg@scratch.dir)
} else {
    return(file.path(cfg@scratch.dir,sp@name))}
}
