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
# Configuration
# ========================================================================
base.dir <- file.path("processing",pcfg@name)
load(file.path(base.dir,"skill_metrics.RData"))

# ========================================================================
# Plots
# ========================================================================
mets.present <- unique(do.call(rbind,met.l)$skill.metric)

#Plot skill
for(met in mets.present) {
  plt.dat <- lapply(met.l,function(d) subset(d,d$skill.metric==met))
  g.skill <- ggplot(plt.dat$hindcast,aes(x=lead.years,y=value,col=src,shape=src))+
    geom_line()+geom_point(size=3) +
    geom_step(data=plt.dat$pers,aes(x=lead.years,y=value,linetype="Persistence"),
              inherit.aes = FALSE,direction="vh")+
    # geom_line(data=met.l$PWW,inherit.aes=FALSE,
    #            aes(x=lead.years,y=cor,linetype="PWW"))+
    xlab("Forecast lead (years)") + ylab("Skill score")+
    ggtitle(sprintf("%s Forecast skill, %s",pcfg@name,met))+
    scale_color_discrete(name="Model") +
    scale_shape_manual(name="Model",values=as.character(1:10))+
    scale_linetype_manual(name="Reference",values=c(1,2,4)) +
    xlim(c(0,max.lead)) 

  
    #stop("Change this to plt.dat instead of  met.l")
  # geom_hline(data=met.l$uninit,aes(yintercept=value,col=src,shape=src),
    #          alpha=0.25,size=0.5) +
    # geom_point(data=met.l$uninit,aes(x=0.1,y=value,col=src,shape=src),
    #            inherit.aes = FALSE,size=2)+
    # geom_point(data=met.l$uninit,aes(x=9.9,y=value,col=src,shape=src),
    #            inherit.aes = FALSE,size=2)
  
  if(met=="correlation") {
    g.skill <- g.skill + 
      coord_cartesian(ylim=c(0,1),expand=FALSE)+
      geom_hline(aes(yintercept=signif.cor,linetype="Signif. (95%, one tailed)")) +
      facet_wrap(~indicator)
      
  } else {
    g.skill <- g.skill + 
      facet_wrap(~indicator,scales = "free_y")
    
  }
  print(g.skill)
}
#
g.uninit.cor <- ggplot(met.l$uninit,aes(src,value,fill=src))+
  facet_wrap(skill.metric~indicator,scales="free_y")+
  geom_bar(stat="identity",position="dodge")
print(g.uninit.cor)


# With the uninitialised plotted on top
# g.skill.uninit <- g.skill +
#   geom_hline(data=met.l$uninit,aes(yintercept=cor,col=src,shape=src),
#              alpha=0.25,size=0.5) +
#   geom_point(data=met.l$uninit,aes(x=0.1,y=cor,col=src,shape=src),
#              inherit.aes = FALSE,size=2)+
#   geom_point(data=met.l$uninit,aes(x=9.9,y=cor,col=src,shape=src),
#              inherit.aes = FALSE,size=2)
# 
# print(g.skill.uninit)

#Plot of the uninitialised skill
g.uninit.cor <- ggplot(met.l$uninit,aes(src,cor,fill=src))+facet_wrap(~indicator)+
  geom_bar(stat="identity",position="dodge") + 
  coord_cartesian(ylim=c(0,1),expand=FALSE)
print(g.uninit.cor)

#Compare the skill for the different metrics
ensmean.skill <- subset(met.l$hindcast,src=="Ensmean")
g.ensmean.skill <- ggplot(ensmean.skill,
                          aes(lead.years, cor,col=indicator,shape=indicator))+
  geom_line()+geom_point(size=3)+coord_cartesian(ylim = c(0,1),expand=FALSE)+
  scale_shape_manual(name="Indicator",values=as.character(1:10))+
  scale_color_discrete(name="Indicator")
print(g.ensmean.skill)

#Write to file
pdf(file.path("plots",sprintf("%s_G2.Visualise_skill_%s.pdf",
                              pcfg@name,datetime())))
print(g.skill)
print(g.uninit.cor)
print(g.skill.uninit)
print(g.ensmean.skill)
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
