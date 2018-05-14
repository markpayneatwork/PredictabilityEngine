#/*##########################################################################*/
#' Visualise CMIP5 metrics
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 25 14:09:14 2016
#'
#' Visualises the forecast skill and outputs from the CMIP5 ensemble
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
cat(sprintf("\n%s\n","Visualise CMIP5 Metrics"))
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
library(mgcv)

# ========================================================================
# Configuration
# ========================================================================
base.dir <- file.path("processing",pcfg@name)
load(file.path(base.dir,"indicators","CMIP5.RData"))
date.range <- c(1960,2100)

# ========================================================================
# Setup
# ========================================================================
#Extract metadata
mdl.inds$model <- underscore_field(mdl.inds$src,1)
mdl.inds$expt <- underscore_field(mdl.inds$src,2)
mdl.inds$year <- year(mdl.inds$date)



#Restrict to date range
mdl.inds <- subset(mdl.inds,year(date)>= min(date.range) &
                     year(date)<= max(date.range))

#Historical data post 2005 is likely a partial year - drop it
hist.overrun <- mdl.inds$expt=="historical" & year(mdl.inds$date)>2005
rcp.early.start <- grepl("rcp",mdl.inds$expt) & year(mdl.inds$date)<=2005
mdl.inds <- subset(mdl.inds,!hist.overrun & !rcp.early.start)

#Calculate a relative anomaly
mdl.inds$log.pp <- log(mdl.inds$value)
clim.dat <- subset(mdl.inds,year%in%pcfg@clim.years)
mdl.clim <- tapply(clim.dat$log.pp,clim.dat$model,mean)
mdl.inds$mdl.clim <- mdl.clim[mdl.inds$model]
mdl.inds$rel.anom <- mdl.inds$log.pp-mdl.inds$mdl.clim  
mdl.inds$value <- exp(mdl.inds$rel.anom)

#Setup smoothers by experiment
expt.l <- split(mdl.inds,mdl.inds$expt)
mdl.pred.l <- lapply(expt.l,function(d) {
            d$year <- year(d$date)
            mdl <- gam(value~s(year),data=d)
            pred.df <- data.frame(year=seq(min(d$year),max(d$year),by=1))
            pred.df$value <- predict(mdl,newdata=pred.df)
            pred.df$expt <- unique(d$expt)
            return(pred.df)})
mdl.pred <- do.call(rbind,mdl.pred.l)

# ========================================================================
# Interact with external data-sources - if available
# ========================================================================
has.obs <- length(pcfg@observations)>0
if(has.obs) {
  #Load observational data
  obs.dat.l <- lapply(pcfg@observations,function(od) {
      load(file.path(base.dir,"indicators",sprintf("%s.RData",od@name)))
      return(obs.inds)
  })
  obs.inds <- do.call(rbind,obs.dat.l)

  #CMIP5 model metrics
  load(file.path(base.dir,"skill_metrics.RData"))
  mdl.mets <- met.l$CMIP5
  mdl.mets$model <- underscore_field(mdl.mets$src,1)
}

# ========================================================================
# Plots
# ========================================================================
#Plot time series of experiments
g.proj <- ggplot(mdl.inds,aes(year,value,group=src))+
  geom_line(aes(col=expt),alpha=0.15)+
  ggtitle(pcfg@name) +
  geom_line(data=mdl.pred,aes(x=year,y=value,group=expt,col=expt),
            lwd=1,inherit.aes=FALSE)+
  coord_cartesian(xlim=date.range,expand=FALSE)+
  scale_color_discrete(name="Scenario")+
  xlab("Year")+ylab("Relative value")
print(g.proj)

#Plot time series of experiments with observations as well
if(has.obs) {
  obs.inds$year <- year(obs.inds$date)
  g.proj.obs <- ggplot(mdl.inds,aes(year,value,group=src))+
    geom_line(aes(col=expt),alpha=0.15)+
    ggtitle(pcfg@name) +
    geom_line(data=subset(mdl.pred,expt!="historical"),
              aes(x=year,y=value,group=expt,col=expt),
              lwd=2,inherit.aes=FALSE)+
    geom_point(data=obs.inds,aes(shape=src))+
    geom_line(data=obs.inds,aes(linetype=src))+
    coord_cartesian(xlim=date.range,expand=FALSE)+
    scale_color_discrete(name="Model Scenario")+
    scale_linetype(name="Observations")+
    scale_shape(name="Observations")+
    xlab("Year")+ylab("value")
  print(g.proj.obs)

  #Plot distribution of model skills over historical period
  g.skill <- ggplot(mdl.mets,aes(x=cor)) + 
    stat_bin()+
    stat_density(fill=NA,col="red")+
    coord_cartesian(xlim=c(0,1),expand = FALSE)+
    xlab("Coefficient of correlation (r)")
  print(g.skill)
}

#Write to file
pdf(file.path("plots",sprintf("%s_G4.CMIP5_metrics_%s.pdf",
                              pcfg@name,datetime())))
print(g.proj)
if(has.obs) {
  print(g.skill)
  print(g.proj.obs)
}
dev.off()

# ========================================================================
# Complete
# ========================================================================
#+ results='asis'
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
