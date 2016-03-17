#create maps

#create maps and figures for analysis

#-- import data and packages
library(DT)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(countrycode)
library(scales)
library(igraph)
library(gisfao)
library(sp)

sys <- Sys.info()

if(sys[5] == "x86_64"){
  dir = "~/Dropbox/agSTATS" #Mac
  data.dir = "~/Dropbox/agSTATS/data/" 
} else if (sys[5] == "Michael"){
  dir = "C:/Users/Michael/Dropbox/agSTATS"#HOME PC
  data.dir = "C:/Users/Michael/Dropbox/agSTATS/data/"
} else if (sys[6]=="Rahija") {
  dir = "C:/Users/rahija/Dropbox/agSTATS" #FAO PC
  data.dir = "C:/Users/rahija/Dropbox/agSTATS/data/"
} else {
  stop("Implement location for current user!")
}

setwd(dir)

data <- read.csv(paste0(data.dir,"agstats_final_for narrative_03_2016.csv"), 
                 stringsAsFactors = FALSE)

source("R/cleanFigures.R")
data <- cleanFigures(data) 


#Map with including all donors, and not dividing by years
source("R/mapAllYears.R")
mapAllYears(df=data, include.donors = TRUE)


#Map not including donors, and not dividing by years
source("R/mapAllYears.R")
mapAllYears(df=data, include.donors = FALSE)

#Map of only FAO as implementer
source("R/mapAllYears.R")
mapAllYears(df = data, only.FAO = TRUE)


#Map dividing by number of years, fixed scale
source("R/mapDivideYears.R")
mapDivideYears(df = data, range.fixed = TRUE)

#Map dividing by number of years, NOT fixed scale
source("R/mapDivideYears.R")
mapDivideYears(df = data, range.fixed = FALSE)
