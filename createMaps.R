#create maps

#create maps and figures for analysis

#-- import data and packages
library(dplyr)
library(ggplot2)
library(countrycode)
library(gisfao)
library(sp)
library(tidyr)
library(grid)

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


#need to figure out way to keep scale for bubble sizes

#dataset = donors, use "," for seperating, not ";"
setwd(dir)

data <- read.csv(paste0(data.dir,"agstats_final_for narrative_V5.csv"), 
                 stringsAsFactors = FALSE)

source("R/cleanFigures.R")
data <- cleanFigures(data) 

#create estimated flow map
source("R/flowMap.R")
flowMap(data = data, include.donors = TRUE,fixed.range = c(1,9))
flowMap(data = data, include.donors = FALSE)

#create maps by implementer - FAO
source("R/implementerMap.R")
fao <- implementerMap(df = data,
               implementing.institution = "FAO")

wb <- implementerMap(df = data,
                     implementing.institution = "WB")

usda <- implementerMap(df = data,
               implementing.institution = "USDA")

eu <- implementerMap(df = data,
                     implementing.institution = "EU")

eu
#create implementer table
impl <- select(data, agId, implementer)
temp <- separate(impl,implementer, 
                 sep =",",
                 into = c("imp1","imp2","imp3","imp4","imp5"),
                 extra = "drop")

temp <- gather(temp,agId, na.rm = TRUE)
colnames(temp) <- c("agId","useless","implementer")
tab <- temp %>%
            group_by(implementer) %>%
            summarize(NoProjects = length(unique(agId)))
arrange(tab, NoProjects)











# 
# 
# 
# #Map with including all donors, and not dividing by years
# source("R/mapAllYears.R")
# mapAllYears(df=data, 
#                  include.donors = TRUE,
#                  only.FAO = FALSE)
# 
# 
# #Map not including donors, and not dividing by years
# source("R/mapAllYears.R")
# mapAllYears(df=data, include.donors = FALSE)
# 
# #Map of only FAO as implementer
# source("R/mapAllYears.R")
# mapAllYears(df = data, only.FAO = TRUE)
# 
# 
# #Map dividing by number of years, fixed scale
# source("R/mapDivideYears.R")
# mapDivideYears(df = data, range.fixed = TRUE)
# 
# #Map dividing by number of years, NOT fixed scale
# source("R/mapDivideYears.R")
# mapDivideYears(df = data, range.fixed = FALSE)
