#'========================================================================
# Figure_for_Daniela
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Jun 19 11:33:28 2019
#
# A figure for Daniela Matei showing the comparison between MPI-ESM-MR and 
# MPI-ESM-LR MikKlip baseline 1
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Figure_for_Daniela"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(scales)

#'========================================================================
# Setup ####
#'========================================================================
#Take input arguments, if any
all.skill.m <- readRDS("scratch/Bluefin/Skill_metrics.rds")
plt.dat <- filter(all.skill.m,
                       src.name %in% c("HadISST","MPI-ESM-LR","MPI-ESM-MR"),
                       src.type!="Observations",
                       sp.subdomain=="South.of.Iceland",
                       skill.metric=="cor",
                       skill.type=="mean.skill")

#Rescale time axis
rescale.fn <- function(m) {  #Converts a leadtime in months into a rescale value
  ifelse(m>12,
         rescale(m,from=c(12,120),to=c(1,2)),
         rescale(m,from=c(0,12),to=c(0,1)))
}
plt.dat <- plt.dat %>%
            mutate(t=rescale.fn(lead),
                   lead.yrs=lead/12,
                   src.label=ifelse(src.type=="Persistence",src.type,src.name))


#Plot!
x.lbls <- tibble(months=c(seq(0,12,by=3),seq(24,120,by=24)),t=rescale.fn(months)) %>%
  mutate(lbl=ifelse(months<=12,sprintf("%g m",months),
                    sprintf("%g y",months/12)))
x.minor <- tibble(months=c(0:12,seq(24,120,by=12)),t=rescale.fn(months))

#'========================================================================
# Plot ####
#'========================================================================
g <- ggplot(plt.dat,aes(x=lead.yrs,y=value,group=src.name))+
  geom_hline(yintercept=0,show.legend=FALSE)+
  geom_line(aes(colour=src.label))+
  coord_cartesian(xlim=c(0,10),ylim=c(0,1),expand=FALSE)+
  guides(size=FALSE)+
  # scale_x_continuous(breaks=x.lbls$t,
  #                    labels=x.lbls$lbl,
  #                    minor_breaks = x.minor$t)+
  labs(x="Lead time (years)",y="Pearson correlation (r)",colour="Forecast System")+
  theme_bw()
print(g)

#'========================================================================
# And Go ####
#'========================================================================


#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# .............
# This work by Mark R Payne is licensed under a  Creative Commons
# Attribution-NonCommercial-ShareAlike 3.0 Unported License.
# For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for
# non-commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or
# similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# This work should also be considered as BEER-WARE. For details, see
# http://en.wikipedia.org/wiki/Beerware
# .............
