#'========================================================================
# I.Calculate_skill_metrics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Jun  1 15:53:49 2018
#
# Calculates indicator skill metrics based on the database of indicators
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
cat(sprintf("\n%s\n","I.Calculate_skill_metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tibble)
library(dplyr)
library(reshape2)
library(lubridate)
load("objects/configuration.RData")
  
#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.debug.level(0)  #Non-zero lets us run with just a few points
} else {
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Directory setup
base.dir <- pcfg@scratch.dir
ind.dir <- define_dir(base.dir,"indicators")

#'========================================================================
# Select input data ####
#'========================================================================
#Import full set of indicator metrics
ind.met.fnames <- dir(ind.dir,full.names = TRUE)

#Loop over them individually and import
ind.met.l <- list()
for(f in ind.met.fnames){
  var.names <- load(f)
  ind.met.l[[f]]   <- get(var.names)
}

#Now we have one big list of indicator data. We could try and merge it
#all together straight away, but there will be a problem with the names of the 
#various columns. Lets see what they have in common and retain them
ind.res.colnames <- lapply(ind.met.l,names)
col.tbl <- melt(sort(table(unlist(ind.res.colnames))/length(ind.met.l)))
print(col.tbl)

#Choosing the variables gets a bit messy, and highlights the fact that my 
#programming is not at all systematic :-( Nevertheless, there are some 
#consistent values that we want to keep. We express this in terms of a
#list of synonyms, that will be merged at a later date. 
keep.cols <- list(data.src=c("data.src","model"),
                  "data.type",
                  date=c("forecast.date","date"),
                  "start.date",
                  "realization",
                  "indicator","value","fname")
keep.tbl <- melt(keep.cols,value.name="from.col") %>%
            mutate(to.col=ifelse(L1=="",from.col,L1),
                   L1=NULL)
print(subset(col.tbl,!(Var1 %in% keep.tbl$from.col)))

#Now we loop over the data sets doing the merging as appropriate
common.cols.l <- ind.met.l
for(i in seq(common.cols.l)){
  im <- common.cols.l[[i]]
  #Drop all columns that are not in the wish-list
  col.present <- has_name(im,keep.tbl$from.col)
  im <- im[,keep.tbl$from.col[col.present]] 
  #Rename retained columns
  new.names <- keep.tbl$to.col[col.present]
  if(any(duplicated(new.names))) {
    stop("Duplicated names")
  }
  colnames(im) <- new.names  
  #Now, what about the missing names. These should be added as NAs
  missing.cols <- keep.tbl$to.col[!has_name(im,keep.tbl$to.col)]
  for(n in missing.cols){
    im[,n] <- NA
  }
  #Store the results
  common.cols.l[[i]] <- im
  
}

#Merge into one big object and add meta information
all.res <- bind_rows(common.cols.l) %>% 
            mutate(year=year(date),
                   lead=as.numeric(round(difftime(date,start.date,units="days") /15)*15))

#'========================================================================
# Split and Merge ####
#'========================================================================
#Drop years that are not to be included in the evaluation of skill metrics
sel.res <-  all.res %>% filter(year %in% pcfg@comp.years) 

#Extract out the observational data
obs.dat <- subset(sel.res,data.type=="obs") %>%
           select(year,indicator,value)

#And merge it back into the comparison dataframe. This way we have both the
#modelled and the observed results together in the same dataframe. We note
#that we do the merging by year - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 
comp.dat <- left_join(sel.res,obs.dat,
                      by=c("year","indicator"),
                      suffix=c(".mdl",".obs"))

#'========================================================================
# Calculate the metrics ####
#'========================================================================
#Now calculate the metrics
RMSE <- function(x,y) { sqrt(mean((x-y)^2))}
skill.m <- comp.dat %>%
           group_by(data.src,indicator,lead) %>%
           summarize(cor=cor(value.mdl,value.obs,use="pairwise.complete"),
                     RMSE=RMSE(value.mdl,value.obs))


#'========================================================================
# Complete ####
#'========================================================================
#Save results
save(skill.m, file=file.path(base.dir,"Skill_metrics.RData"))
save(all.res, file=file.path(base.dir,"All_indicators.RData"))


#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
