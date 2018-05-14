#/*##########################################################################*/
#' Visualise Forecasts
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu May 26 00:34:40 2016
#'
#' Visualises the current set of forecasts from NMME and MPI-OM 
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#    where it can be compiled in a meaningful manner
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Visualise Forecasts"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

library(ggplot2)

# ========================================================================
# Configuration
# ========================================================================
base.dir <- file.path("processing",pcfg@name)
indicator.dir <- file.path(base.dir,"indicators")

#Select a specific set of years for the forecasts
forecast.sel.yrs <-2016:2025

#Import indicators
load(file.path(base.dir,"combined_indicators.RData"))

# ========================================================================
# Import and setup indicators
# ========================================================================
#Split and merge
model.inds <- subset(inds.all,type=="hindcast" & real=="realmean")
obs.inds <- subset(inds.all,type=="obs" & 
                            year(fperiod.start) > min(pcfg@comp.years))

#Extract most recent forecasts
forecast.newest <- lapply(split(model.inds,model.inds$src),function(d){
                       subset(d,year(d$forecast.init) ==max(year(d$forecast.init)))
})
forecast.newest <- do.call(rbind,forecast.newest)
forecast.sel <- subset(forecast.newest,year(fperiod.start) %in% forecast.sel.yrs)

# ========================================================================
# Make plots
# ========================================================================
#Forecast development
g.forecast <- ggplot(obs.inds,mapping=aes(x=fperiod.start,y=value))+
  # facet_wrap(~indicator,scale="free_y")+
  stat_smooth(data=forecast.sel)+
  geom_line(data=forecast.sel,aes(col=src))+
  #geom_line(data=ensmean,col="black",size=2)+
  xlab("Date") +
  geom_line(data=obs.inds,aes(linetype=src))+
  geom_point(data=obs.inds,aes(shape=src))+
  scale_linetype(name="Observations") +
  scale_color_discrete(name="Model")+
  scale_shape(name="Observations")+
  ggtitle(pcfg@name)
print(g.forecast)

#Write to file
pdf(file.path("plots",sprintf("%s_G3.Forecasts_%s.pdf",
                              pcfg@name,datetime())))
print(g.forecast)
dev.off()

#ggsave("plots/Bluefin_forecasts.png",width=18,height=12,units="cm",scale=0.85)

# g <- ggplot(obs.mets,aes(x=z,y=value))+geom_line()+#geom_point()+
#   facet_wrap(~metric,scale="free_y")+
#   stat_summary(data=forecast.newest,aes(x=forecast.date,y=value),
#                fun.data=mean_se,fun.args=1.96)+
#   #stat_boxplot(data=NMME.newest,aes(x=forecast.date,y=value))+
#   geom_point(data=forecast.newest,aes(x=forecast.date,y=value,col=src))+
#   xlab("Date")
# print(g)
# 
# #As a histogram
# g <- ggplot(obs.mets,aes(x=value))+stat_bin(bins=10)+
#       facet_wrap(~metric,scale="free_x") + 
#   geom_point(data=forecast.newest,aes(x=value,y=0,col=src))
# print(g)

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
