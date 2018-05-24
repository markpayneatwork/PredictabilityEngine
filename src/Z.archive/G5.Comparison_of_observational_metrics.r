#/*##########################################################################*/
#' Comparison of observations
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Fri Sep 16 13:26:12 2016
#'
#' Compares observational metrics with each other
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
cat(sprintf("\n%s\n","Compare observations"))
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

#Import externals
load(file.path(base.dir,"combined_indicators.RData"))
load(file.path(base.dir,"skill_metrics.RData"))

# ========================================================================
# Import and setup indicators
# ========================================================================
#Split and merge
obs.inds <- subset(inds.all,type=="obs" & 
                            year(fperiod.start) > min(pcfg@comp.years))
obs.inds$year <- year(obs.inds$fperiod.start)

#Extract out reference observations
ref.inds <- subset(obs.inds,src==pcfg@observations[[1]]@name)

#Merge into the rest
dat <- merge(obs.inds,ref.inds,
               suffixes=c("",".ref"),
               by=c("fperiod.start","indicator"))
dat <- subset(dat,select=-c(src.ref,forecast.init.ref,real.ref,type.ref))


# ========================================================================
# Make plots
# ========================================================================
#Time series of the individual metrics
g.ts <- ggplot(obs.inds,aes(year,value,shape=src,col=src))+
        geom_point()+geom_line()
print(g.ts)

#Scatter plots
g.scat <- ggplot(dat,aes(x=value.ref,y=value))+
  geom_point()+
  facet_wrap(~src,scale="free_y")
print(g.scat)

#Correlation coefficients
g.cor <- ggplot(met.l$obs,aes(x=src,y=cor,fill=src))+
            geom_bar(stat="identity",show.legend = FALSE)+
          ggtitle(sprintf("Correlation vs %s SST",pcfg@observations[[1]]@name)) +
          xlab("Observation source")+ylab("Pearson correlation (r)")
print(g.cor)

#Write to file
pdf(file.path("plots",sprintf("%s_G5.Observation_comparison_%s.pdf",
                               pcfg@name,datetime())))
print(g.ts)
print(g.scat)
print(g.cor)

plt.dat <- subset(dat,src=="EN4-deep")
g <- ggplot(plt.dat,aes(value,value.ref))+
  geom_point()+
  xlab("Mean temp 60-150m (degC")+
  ylab("SST (degC)")+
  stat_smooth(method="lm")
print(g)
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
