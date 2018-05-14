#/*##########################################################################*/
#' Skill comparison
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 25 14:09:14 2016
#'
#' Calculates and compares the forecast skill of the models
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
cat(sprintf("\n%s\n","Forecast Skill Comparison"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

library(lubridate)
library(reshape2)
library(ggplot2)
library(scales)

# ========================================================================
# Configuration
# ========================================================================
base.dir <- file.path("processing",pcfg@name)
indicator.dir <- file.path(base.dir,"indicators")

# ========================================================================
# Merge observations with model outputs
# ========================================================================
#Extract list of potential indicator sources, to be used as a 
#filter on the actual sets of indicators available
ind.srcs <- c(sprintf("%s_hindcast",names(pcfg@hindcast.models)),
              sprintf("%s_uninit",names(pcfg@uninit.models)),
              names(pcfg@observations),
              "Ensmean_hindcast","PWW")

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

#Extract out values to work with
hindcast.inds <- subset(inds.all, type=="hindcast" & real=="realmean")
ref.inds <- subset(inds.all,src==pcfg@observations[[1]]@name)

#Create an indicator average  over the initialised models only and
#add into the total mix
indave <- melt(tapply(hindcast.inds$value,
                            hindcast.inds[,c("indicator","forecast.init","fperiod.start")],
                            mean))
indave <- subset(indave,!is.na(indave$value))
indave <- transform(indave,src="Ave.Ind",
                          real="aveind",type="hindcast")
inds.all <-rbind(inds.all,indave)

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
# Calculate and plot correlation coefficients
# ========================================================================
#Limit comparison to those years within the climatology to start with
ind.comp.dat <- subset(dat,year(fperiod.start) %in% pcfg@comp.years) 
hindcast.comp.dat <- subset(ind.comp.dat,type=="hindcast" &
                          real%in%c("realmean","ensmean"))
PWW.comp.dat <- subset(ind.comp.dat,type=="PWW")
obs.comp.dat <- subset(ind.comp.dat,type=="obs")
uninit.comp.dat <- subset(ind.comp.dat,type=="uninit" &real=="realmean")

#Calculate correlation coefficient
cor.met <- function(d,by.cols) {
  d.l <- split(d,drop=TRUE,d[,by.cols])
  cor.fn <- function(a,meta.cols) {
    res <- a[1,by.cols]
    res$cor <- cor(a$value,a$value.ref,method = "pearson") 
    return(res)}
  init.cor.coef.l <- lapply(d.l,cor.fn,meta.cols=by.cols)
  rtn <- do.call(rbind,init.cor.coef.l)
  return(rtn)
}

#Calculate correlation coefficient metrics
hindcast.cor<- cor.met(hindcast.comp.dat,c("indicator","lead.years","src"))
PWW.cor <- cor.met(PWW.comp.dat,c("indicator","lead.years","src"))
uninit.cor <- cor.met(uninit.comp.dat,c("indicator","src"))
obs.cor <- cor.met(obs.comp.dat,c("indicator","src"))

#Calculate threshold cor following
#https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
#We use a one-tailed test at 5% CL
thresh.cor <- tanh(qnorm(0.95)/sqrt(length(pcfg@clim.years)-3))

#Calculate persistence forcast
#This is a bit tricky to achieve matching over the time series properly. Two choices
#1. take the values over the full observational time series
#2. subset and live with it
#Running with number two, as it is more consistent.
acf.dat <- subset(ref.inds,year(fperiod.start)%in%pcfg@clim.years)
acf.l <- tapply(acf.dat$value,acf.dat$indicator,acf,plot=FALSE,
                lag.max=length(unique(hindcast.cor$lead.years)),
                demean=TRUE)
acf.df <- melt(id="lag",
              lapply(acf.l,function(a) data.frame(lag=a$lag,acf=a$acf)))
#For plotting prettiness, set the acf at lead 0 to the same as lead one
acf.mat <- acast(acf.df,L1~lag,value.var="value")
acf.mat[,1] <- acf.mat[,2]
acf.dat <- melt(acf.mat)
colnames(acf.dat) <- c("indicator","lead.years","value")

# ========================================================================
# Plots
# ========================================================================
#Plot
g.skill <- ggplot(hindcast.cor,
                         aes(x=lead.years,y=cor,col=src,shape=src))+
  geom_line()+geom_point(size=3) +
  geom_hline(aes(yintercept=thresh.cor,linetype="Signif. (95%, one tailed)"))+
  geom_step(data=acf.dat,aes(x=lead.years,y=value,linetype="Persistence"),
            inherit.aes = FALSE,direction="vh")+
  # geom_line(data=PWW.cor,inherit.aes=FALSE,
  #           aes(x=lead.years,y=cor,linetype="PWW"))+
  facet_wrap(~indicator) + 
  coord_cartesian(ylim=c(0,1),expand=FALSE)+
  xlab("Forecast lead (years)") + ylab("Pearson correlation coefficient (r)")+
  ggtitle(paste(pcfg@name,"Forecast Skill"))+
  scale_color_discrete(name="Model") +
  scale_shape_manual(name="Model",values=as.character(1:10))+
  scale_linetype_manual(name="Reference",values=c(1,2,4)) +
  xlim(range(pretty(hindcast.cor$lead.years)))
print(g.skill)
#ggsave("plots/Bluefin_skill.png",width=18,height=12,units="cm")

g.skill.uninit <- g.skill +
  geom_hline(data=uninit.cor,aes(yintercept=cor,col=src,shape=src),
             alpha=0.25,size=0.5) +
  geom_point(data=uninit.cor,aes(x=0.1,y=cor,col=src,shape=src),
         inherit.aes = FALSE,size=2)+
  geom_point(data=uninit.cor,aes(x=9.9,y=cor,col=src,shape=src),
           inherit.aes = FALSE,size=2)

print(g.skill.uninit)

#Comparison of observations
g.obs.cor <- ggplot(obs.cor,aes(src,cor,fill=src))+facet_wrap(~indicator)+
  geom_bar(stat="identity",position="dodge") + 
  ggtitle("Correlations between observations")
print(g.obs.cor)


#Comparison of observed with uninitialised
# g.uninit <- ggplot(uninit.comp.dat,aes(value.ref,value))+
#   geom_point()+facet_wrap(src~indicator,scales="free")+stat_smooth(method="lm")+
#   ggtitle("Comparison wtih")
# print(g.uninit)

g.uninit.cor <- ggplot(uninit.cor,aes(src,cor,fill=src))+facet_wrap(~indicator)+
  geom_bar(stat="identity",position="dodge")
print(g.uninit.cor)


# plt.dat <- model.inds
# plt.dat$lead.days <- difftime(plt.dat$date,plt.dat$forecast.init,unit="days")
# plt.dat$lead.years <- round(as.numeric(plt.dat$lead.days)/365*10)/10 #Round to nearest 0.1 year

# g.ts <- ggplot(mapping=aes(x=date,y=value))+ facet_wrap(~src)+
#   geom_line(data=plt.dat,aes(col=lead.years,group=forecast.init),lwd=0.25) + 
#   geom_line(data=obs.inds,lwd=1,col="black")+
#   ggtitle(paste(pcfg@name,"Hindcasts"))+
#   scale_colour_continuous(name="Lead (years)") 
# print(g.ts)        

 # ggplot(dat,aes(x=date,y=value.obs,col=src.mdl))+
 #   facet_wrap(~indicator,scales="free_y")+geom_line()

#Presentation plot, CLIVAR ENSO workshop
# cor.dat <- subset(cor.dat,indicator=="threshold.area_11_deg")
# acf.df <- subset(acf.df,indicator=="threshold.area_11_deg")
# g.skill <- ggplot(subset(cor.dat,src.mdl!="PWW"),
#                   aes(x=lead.years,y=cor,col=src.mdl,shape=src.mdl))+
#   geom_line()+geom_point(size=3) +
#   geom_hline(aes(yintercept=thresh.cor,linetype="Signif. (95%, one tailed)"))+
#   geom_step(data=acf.df,aes(x=lead.years,y=value,linetype="Persistence"),
#             inherit.aes = FALSE,direction="vh")+
#   # geom_line(data=subset(cor.dat,src.mdl=="PWW"),inherit.aes=FALSE,
#   #           aes(x=lead.years,y=cor,linetype="PWW"))+
#   # facet_wrap(~indicator) + 
#   coord_cartesian(ylim=c(0,1),expand=FALSE)+
#   xlab("Forecast lead (years)") + ylab("Pearson correlation coefficient (r)")+
#   #ggtitle(paste(pcfg@name,"Forecast Skill"))+
#   scale_color_discrete(name="Model") +
#   scale_shape_manual(name="Model",values=as.character(1:10))+
#   scale_linetype_manual(name="Reference",values=c(1,2,4)) +
#   xlim(range(cor.dat$lead.years))
# print(g.skill)
# ggsave("plots/Bluefin_skill.png",width=18,height=12,units="cm",scale=0.85)

#Write to file
pdf(file.path("plots",sprintf("%s_G1.Skill_comparison_%s.pdf",
                              pcfg@name,datetime())),
    width=210/25.4,height=297/25.4)
print(g.skill)
print(g.uninit.cor)
#print(g.uninit)
print(g.obs.cor)
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
