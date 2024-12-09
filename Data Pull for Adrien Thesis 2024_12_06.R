# load packages
library(RMySQL)
library(lubridate)
library(tidyverse)
library(patchwork)
library(marked)
library(RMark)

#code for Roxanne to run
# 
# getMySQLTable=function(db='elephantseal',tbl='resight') #function to read a database table
# {
#   #open the connection
#   conn=dbConnect(MySQL(),user="root",password="",dbname=db)
#   #guarantees that it disconnects whatever happens when you exit R
#   on.exit(dbDisconnect(conn))
#   
#   fullqry=paste0("SELECT * FROM ", db, ".", tbl)
#   #send query that will return results as R dataframe
#   full=suppressWarnings(dbGetQuery(conn,fullqry))
#   return(full)
# }
# 
# Resight=getMySQLTable(db='elephantseal',tbl='resight')
# Animal=getMySQLTable(db='elephantseal',tbl='viewfullanimalrecord')
# PupObs=getMySQLTable(db='elephantseal',tbl='pupobs')
# 
# setwd("C:/Users/roxan/Documents/Collaborations/Adrien Bastidas")
# write.csv(Resight,'resightforadrien.csv',row.names=FALSE)
# write.csv(Animal,'animalforadrien.csv',row.names=FALSE)
# write.csv(PupObs,'pupobsforadrien.csv',row.names=FALSE)

animal <- read_csv("animalforadrien.csv") 
resight <- read_csv("resightforadrien.csv") 

# specify which animals (pups) to select
animals_to_select <- animal %>%
  filter(
    # from Año Nuevo
    region == "AN",
    # tagged as pup/wean
    broad == "YOY",
    # with a flipper tag
    !is.na(origtag),
    # with a correct date
    !is.na(year(tagdate))
  ) #both sex

pups=subset(animals_to_select,momID!=0) #subset of pups where mom ID is known
pups=pups[,c("animalID","tagsex","momID","tagdate")] #some columns from the pup dataset
colnames(pups)=c("pupID","pupsex","animalID","tagdate") #rename the columns
pups$year=year(pups$tagdate) #find year that the pup was tagged

#get rid of pups with no tag sex (149 cases out of ~4,000)
pups=pups %>% subset(pupsex=="M"|pupsex=="F")

# final table for moms
outall <- merge(# merge resight
  resight,
  # and animals to select (only tagdate, tagsex, pupID, and animalID columns)
  animals_to_select %>% dplyr::select(tagdate,animalID,momID,tagsex),
  # by animalID, which is the mom's ID
  by = "animalID") %>%
  # create a new column yearborn
  mutate(yearborn = year(tagdate),
         yday=yday(date), #day of year for observations
         timeofyear=if_else(yday>274|yday<74,"Breeding", #between Dec 1 and Mar 15
                            if_else(yday>=74&yday<182,"Molt","Other"))) %>%   #between Mar 15 and July 1
  arrange(date)

outall$calyear=year(outall$date) #make a new column for the calendar year of that observation

#correct pup status for weird character values
outall$withpupCor=NA
outall$withpupCor[outall$withpup%in%c(as.character(1:8),"2+","3+",">8","1 or 2",">1")]=1
outall$withpupCor[outall$withpup==0]=0

#make encounter History about whether each seal was seen or not in each year
g1=as.data.frame.matrix(with(outall,table(animalID,calyear))) #table : combinations of observation year and animal ID
#presence/absence with only encounter history (no breeding info, and no multiple sightings per year)
g1[g1>0]=1 #seen at least one time, change to 1 for present

#make goal dataframe that starts the "final" dataframe...
#one row for each animal-year combo observation.
goal=data.frame(animalID=rep(as.numeric(rownames(g1)),each=ncol(g1)),
                year=rep(as.numeric(colnames(g1)),times=nrow(g1)),
                observed=as.vector(t(g1)))

#find first date of haulout for all the b years
dateobsbreed=outall %>%
  group_by(season,animalID) %>% #by season, so that it doesn't pick up end of year
  subset(timeofyear=="Breeding") %>%
  summarise(firstobsbreed=min(date),
            lastobsbreed=max(date),
            calyear=first(calyear)) %>% 
  mutate(firstobsbreed=if_else(yday(firstobsbreed)>70,NA,firstobsbreed)) %>%
  mutate(firstobsbreeddoy=yday(as.Date(firstobsbreed))) %>%
  mutate(firstobsbreeddoy=if_else(firstobsbreeddoy>300,firstobsbreeddoy-365,firstobsbreeddoy)) %>%
  mutate(calyear=calyear+1)

#need to add how many pup observations she had, first pup observation
obswithpup=outall %>%   
  filter(timeofyear=="Breeding"&withpupCor>0) %>%
  group_by(season,animalID) %>% 
  summarise(n=length(withpupCor),
            firstobswithpup=min(date))

#change column names so the merge will work
colnames(dateobsbreed)[1]="year"
colnames(obswithpup)[1]="year"

dateobsbreed=left_join(dateobsbreed,obswithpup,by=c("year","animalID"))

#check 1885
subset(obswithpup,animalID==1561)

#add in tagsex
tagsexdat=outall %>%
  group_by(animalID) %>%
  count(tagsex) %>%
  top_n(1)

goal=merge(goal,tagsexdat[,c("animalID","tagsex")],by="animalID")

#merge goal matrix (observations) with dates
goal=left_join(goal,dateobsbreed,by=c("year","animalID"))

#now, merge pup information
goal=left_join(goal,
               subset(pups[,c("pupID","pupsex","animalID","year")]),
               by=c("year","animalID"))

age=outall %>%
  group_by(animalID) %>%
  select(animalID,yearborn) %>%
  summarize(yearborn=unique(yearborn))
#warning message because animalID 17 has two entries for 1984?

goal=left_join(goal,
               age,
               by=c("animalID"))
goal$age=goal$year-goal$yearborn

goal=subset(goal,age>=0) #get rid of years that occurred before each seal's birth year


#add wean weight from https://datadryad.org/stash/dataset/doi:10.7291/D1D973; note that this only goes until 2021. lmk if you need the last 3 years of data. 
weight=read.delim("ElephantSealWeanlingWeightArchive.tab")
weight$year=year(as.Date(weight$weighingdate,format="%Y-%m-%d"))
colnames(weight)[1]="pupID"
goal=left_join(goal,weight[,c("year","pupID","Wt")],by=c("year","pupID"))

#check 1885
view(subset(goal,animalID==1561))

write.csv(goal,"Adrien Data Pull 2024_12_06.csv", row.names=FALSE)
