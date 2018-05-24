#/*##########################################################################*/
#' Visualise skill metrics
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 25 14:09:14 2016
#'
#' Visualises the forecast skill of the models
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
cat(sprintf("\n%s\n","Visualise Skill Metrics"))
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
#Forecast skill
# ========================================================================
# Configuration
base.dir <- file.path("processing",pcfg@name)
load(file.path(base.dir,"skill_metrics.RData"))

sel.ind <- "Irminger Sea mean temp"
sel.ind <- c("South of Iceland habitat > 11 deg","Irminger Sea mean temp")
sel.skill.metric <- "correlation"
sel.models <- c("MPI-ESM-LR","MPI-ESM-MR","HadISST")
met.l <- lapply(met.l,function(d) subset(d,d$indicator %in% sel.ind & 
                                           d$skill.metric %in% sel.skill.metric &
                                           d$src %in% sel.models))

#Plot skill
g.skill <- ggplot(met.l$hindcast,aes(x=lead.years,y=value,col=src,shape=src))+
  geom_line()+geom_point(size=3) +
  geom_hline(aes(yintercept=signif.cor,linetype="Signif. (95%, one tailed)"))+
  geom_step(data=met.l$pers,aes(x=lead.years,y=value,linetype="Persistence"),
            inherit.aes = FALSE,direction="vh")+
  # geom_line(data=met.l$PWW,inherit.aes=FALSE,
  #            aes(x=lead.years,y=cor,linetype="PWW"))+
  facet_wrap(~indicator) + 
  coord_cartesian(ylim=c(0,1),expand=FALSE)+
  xlab("Forecast lead (years)") + ylab("Pearson correlation coefficient (r)")+
  #ggtitle(paste(pcfg@name,"Forecast Skill"))+
  scale_color_discrete(name="Model") +
  scale_shape_manual(name="Model",values=as.character(1:10))+
  scale_linetype_manual(name="Reference",values=c(1,2,4)) +
  xlim(c(0,max.lead))
print(g.skill)
ggsave("plots/prod_plots/Bluefin_skill.png",
       width=23,height=13,units="cm",scale=0.85)
stop()
# ========================================================================
# Forecasts
# ========================================================================
#Select a specific set of years for the forecasts
forecast.sel.yrs <-2016:2025

#Import indicators
load(file.path(base.dir,"combined_indicators.RData"))

#Split and merge
model.inds <- subset(inds.all,type=="hindcast" & real=="realmean" & 
                       indicator==sel.ind)
obs.inds <- subset(inds.all,type=="obs" & indicator==sel.ind &
                     year(fperiod.start) > min(pcfg@comp.years))

#Extract most recent forecasts
forecast.newest <- lapply(split(model.inds,model.inds$src),function(d){
  subset(d,year(d$forecast.init) ==max(year(d$forecast.init)))
})
forecast.newest <- do.call(rbind,forecast.newest)
forecast.sel <- subset(forecast.newest,year(fperiod.start) %in% forecast.sel.yrs)

#Forecast development
g.forecast <- ggplot(obs.inds,mapping=aes(x=fperiod.start,y=value))+
  #facet_wrap(~indicator,scale="free_y")+
  stat_smooth(data=forecast.sel)+
  geom_line(data=forecast.sel,aes(col=src))+
  #geom_line(data=ensmean,col="black",size=2)+
  xlab("Year") +ylab("Area of Habitat (km²)")+
  geom_line(data=obs.inds,aes(linetype=src))+
  geom_point(data=obs.inds,aes(shape=src))+
  scale_linetype(name="Observations") +
  scale_color_discrete(name="Model")+
  scale_shape(name="Observations")
  #ggtitle(pcfg@name)
print(g.forecast)
ggsave("plots/prod_plots/Bluefin_forecast.png",
       width=23,height=13,units="cm",scale=0.85)

# ========================================================================
# Forecasting specific years
# ========================================================================
focus.yrs <- c(2012,2014)

#Extract all predictions for the years in question
forecast.dat <- subset(inds.all,year(inds.all$fperiod.start) %in% focus.yrs &
                         real=="realmean" & 
                         type=="hindcast" & indicator==sel.ind)
forecast.dat$year <- year(forecast.dat$fperiod.start)

obs.dat <- subset(obs.inds,year(obs.inds$forecast.init)%in%focus.yrs)
obs.dat$year <- year(obs.dat$forecast.init)

#Setup timeframes correctly
forecast.dat$lead.days <- difftime(forecast.dat$fperiod.start,forecast.dat$forecast.init,units="days")
forecast.dat$lead.years <- round(as.numeric(forecast.dat$lead.days)/365*10)/10 #Round to nearest half month

#Plot
ggplot(forecast.dat,aes(lead.years,value,col=src))+geom_line()+
  facet_wrap(~year) + ylab("Area of Habitat (km²)")+
  scale_color_discrete(name="Model")+
  scale_shape(name="Observations")+
  xlab("Forecast lead (years)")+
  scale_x_reverse()+
  stat_smooth(aes(lead.years,value),inherit.aes = FALSE)+
  geom_hline(data=obs.dat,aes(yintercept=value))

# ========================================================================
# Observational time series
# ========================================================================
obs.inds <- subset(inds.all,type=="obs" & indicator==sel.ind &
                     year(fperiod.start) > min(pcfg@comp.years))

g.obs <- ggplot(obs.inds,aes(forecast.init,value))+
  xlab("Year")+ylab("Area of Habitat (km²)")+
  stat_smooth()+
  geom_point()+geom_line()
print(g.obs)
ggsave("plots/prod_plots/Bluefin_observations.png",
       width=13,height=12,units="cm",scale=0.85)


# ========================================================================
# Spatial forecasts
# ========================================================================
#Load anomaly data and average
for.fnames <- dir("processing/Bluefin/MPI-ESM-LR-hindcast/6.realmean/",
                  pattern="decs4e2014",full.names = TRUE)
for.s <- stack(for.fnames)
for.anom <- mean(for.s)

#Add in observational data
obs.clim <- raster("processing/Bluefin/EN4/obs_climatology.nc")

#Make the forecast and crop
for.temp <- for.anom+obs.clim

#Plot with ggplot
plt.dat <- cbind(coordinates(for.temp),as.data.frame(for.temp))
ggplot(plt.dat,aes(x,y))+geom_raster(aes(fill=layer))+
  geom_contour(aes(z=layer,linetype=factor(..level..)),
               breaks=c(10,11,12),col="white")

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
