#/*##########################################################################*/
#' Calculate skill metrics
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 25 14:09:14 2016
#'
#' Calculates the forecast skill of the models
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#   * Add Persistence forecasts to plots
#   * Anomaly persistence forecasts?
#   * Taylor diagrams for model skill?
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#    where it can be compiled in a meaningful manner
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Calculate Skill Metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

library(reshape2)
library(ggplot2)
library(scales)

# ========================================================================
# Configuration
# ========================================================================
base.dir <- file.path("processing",pcfg@name)
indicator.dir <- file.path(base.dir,"indicators")

max.lead <- 10
# ========================================================================
# Import indicator data sets
# ========================================================================
#Extract list of potential indicator sources, to be used as a 
#filter on the actual sets of indicators available
ind.srcs <- c(sprintf("%s_hindcast",names(pcfg@hindcast.models)),
              sprintf("%s_uninit",names(pcfg@uninit.models)),
              names(pcfg@observations),
              "Ensmean_hindcast","PWW","CMIP5")

#Load indicators
indicator.fnames.all <- dir(indicator.dir,full.names = TRUE)
indicator.fnames <- grep(paste(ind.srcs,collapse="|"),
                         basename(indicator.fnames.all),
                         value = TRUE)
inds.l <- lapply(file.path(indicator.dir,indicator.fnames),function(f){
            obj.name <- load(f)
            return(get(obj.name))
})
inds.all <- do.call(rbind,inds.l)

#Tidy up the dates. When calculating averages over multiple months, CDO
#simply averages the dates. We apply the convention here that lead time is
#the time from when the forecasts is issued, to the start of the forecast
#period, irrespective of the forecast periods length - this is the WMO
#standard by the looks of it. We therefore have to correct the dates 
#accordingly. Note that we assume that our forecast periods do not cross
#the new-year boundary. Also need to check how this works with NMME.
inds.all$fperiod.start <- date(ISOdate(year(inds.all$date),min(pcfg@MOI),1))
inds.all$date <- NULL

# LOCALHACK, 20161027 - Request from Daniela to recalculate the mean
# MPI-ESM-LR mean across five realisations, to make it comparable with
# the MPI-ESM-MR mean. I have done this here by simply averaging over the
# indicator values, which is idnntical for mean temperatures, but slightly
# different for the area metrics
# MPILR.dat.subset <- subset(inds.all,src=="MPI-ESM-LR" &
#                              type=="hindcast"&
#                              real %in% sprintf("r%ii1p1",1:5))
# MPILR.indave <- melt(tapply(MPILR.dat.subset$value,
#                             MPILR.dat.subset[,c("indicator","forecast.init","fperiod.start")],
#                             mean))
# MPILR.indave <- subset(MPILR.indave,!is.na(MPILR.indave$value))
# MPILR.indave <- transform(MPILR.indave,src="MPI-ESM-LR",
#                           real="realmean",type="hindcast")
# inds.all <- subset(inds.all,!(src=="MPI-ESM-LR" & type=="hindcast"))
# inds.all <- rbind(inds.all,MPILR.indave)
# /LOCALHACK, 20161027

# ========================================================================
# Persistence forecast
# ========================================================================
#Extract out values to work with
ref.inds <- subset(inds.all,src==pcfg@observations[[1]]@name)

#Create the persistence forecast data
pers.dat.l <- lapply(0:max.lead,function(l) {
  d <- ref.inds
  d$forecast.init <- d$fperiod.start
  d$fperiod.start <- d$forecast.init + years(l)
  return(d)
})
pers.dat <- do.call(rbind,pers.dat.l)
pers.dat$type <- "pers"

#Add back into the mix
inds.all <- rbind(inds.all,pers.dat)
# 
# inds.all$indicator <- "spatial mean"
# ref.inds$indicator <- "spatial mean"

# ========================================================================
# Create an average across all indicators
# ========================================================================
#Extract out values to work with
hindcast.inds <- subset(inds.all, type=="hindcast" & real=="realmean")

#Create an indicator average  over the initialised models only and
#add into the total mix
indave <- melt(tapply(hindcast.inds$value,
                            hindcast.inds[,c("indicator","forecast.init","fperiod.start")],
                            mean))
indave <- subset(indave,!is.na(indave$value))
indave <- transform(indave,src="Ave.Ind",
                          real="aveind",type="hindcast")
#inds.all <-rbind(inds.all,indave)

# ========================================================================
# Merge observations with model outputs
# ========================================================================
#Merge all them with observations
dat <- merge(inds.all,ref.inds,
             suffixes=c("",".ref"),
             by=c("fperiod.start","indicator"))
dat <- subset(dat,select=-c(src.ref,forecast.init.ref,real.ref,type.ref))

#Setup timeframes correctly
dat$lead.days <- difftime(dat$fperiod.start,dat$forecast.init,units="days")
dat$lead.months <- round(as.numeric(dat$lead.days)/365*12*2)/2 #Round to nearest half month
dat$lead.years <- round(as.numeric(dat$lead.days)/365*10)/10 #Round to nearest half month

# ========================================================================
# Calculate  metrics
# ========================================================================
#Limit comparison to those years within the climatology to start with
ind.comp.dat <- subset(dat,year(fperiod.start) %in% pcfg@comp.years) 
comp.dat <- list()
comp.dat$hindcast <- subset(ind.comp.dat,type=="hindcast" &
                          real%in%c("realmean","ensmean","aveind"))
comp.dat$PWW <- subset(ind.comp.dat,type=="PWW")
comp.dat$obs <- subset(ind.comp.dat,type=="obs")
comp.dat$CMIP5 <- subset(ind.comp.dat,type=="CMIP5" & grepl("historical",src) &
                                 year(fperiod.start)<=2005)  #Restrict to historical
comp.dat$uninit <- subset(ind.comp.dat,type=="uninit" &real=="realmean")
if(nrow(comp.dat$uninit)!=0) {
  comp.dat$uninit$lead.years <- 0}
comp.dat$pers  <- subset(ind.comp.dat,type=="pers")# & year(fperiod.start)%in%pcfg@clim.years
                          #& year(forecast.init)%in%pcfg@clim.years)

#Calculate skill metrics
skill.mets <- function(d,by.cols=c("indicator","lead.years","src")) {
  #Break up into processing units
  d.l <- split(d,drop=TRUE,d[,by.cols])
  
  #Correlation coefficient
  cor.fn <- function(a) {
    res <- a[1,by.cols]
    res$skill.metric <- "correlation"
    res$value <- cor(a$value,a$value.ref,method = "pearson") 
    return(res)}
  cor.skill.l <- lapply(d.l,cor.fn)
  cor.skill <- do.call(rbind,cor.skill.l)
  
  #RMS error
  RMS.err <- function(a){
    res <- a[1,by.cols]
    res$skill.metric <- "RMSE"
    res$value <- sqrt(mean((a$value-a$value.ref)^2))
    return(res)}
  RMS.skill.l <- lapply(d.l, RMS.err)
  RMS.skill <- do.call(rbind,RMS.skill.l)
  
  return(rbind(cor.skill,RMS.skill))
}

met.l <- lapply(comp.dat,skill.mets)

# #Calculate persistence forcast
# #This is a bit tricky to achieve matching over the time series properly. Two choices
# #1. take the values over the full observational time series
# #2. subset and live with it
# #Running with number two, as it is more consistent.
# acf.dat <- subset(ref.inds,year(fperiod.start)%in%pcfg@clim.years)
# acf.l <- tapply(acf.dat$value,acf.dat$indicator,acf,plot=FALSE,
#                 lag.max=max.lead,
#                 demean=TRUE)
# acf.df <- melt(id="lag",
#                lapply(acf.l,function(a) data.frame(lag=a$lag,acf=a$acf)))
# #For plotting prettiness, set the acf at lead 0 to the same as lead one
# acf.mat <- acast(acf.df,L1~lag,value.var="value")
# acf.mat[,1] <- acf.mat[,2]
# acf.dat <- melt(acf.mat)
# colnames(acf.dat) <- c("indicator","lead.years","value")

# ========================================================================
# Significance threshold
# ========================================================================
#Calculate threshold cor following
#https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
#We use a one-tailed test at 5% CL
signif.cor <- tanh(qnorm(0.95)/sqrt(length(pcfg@clim.years)-3))

## ========================================================================
# Outputs
# ========================================================================
#Save
save(met.l,signif.cor,max.lead,file=file.path(base.dir,"skill_metrics.RData"))
save(inds.all,file=file.path(base.dir,"combined_indicators.RData"))

#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

#' -----------
#' <small>*This work by Mark R Payne is licensed under a  Creative Commons
#' Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
#' For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
#' Basically, this means that you are free to "share" and "remix" for 
#' non-commerical purposes as you see fit, so long as you "attribute" me for my
#' contribution. Derivatives can be distributed under the same or 
#' similar license.*</small>
#'
#' <small>*This work comes with ABSOLUTELY NO WARRANTY or support.*</small>
#'
#' <small>*This work should also be considered as BEER-WARE. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#' 
#' -----------
#
# Fin
