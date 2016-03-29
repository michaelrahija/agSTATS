#create maps and figures for analysis
#http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf



##FIX DUPLICATED BUDGET ISSUE 
#HACK TO REMOVE DUPS



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

data <- read.csv(paste0(data.dir,"agstats_final_for narrative_v2.csv"), 
                 stringsAsFactors = FALSE)


source("R/cleanFigures.R")
data <- cleanFigures(data) 



#---CONFIGURE DATAFRAME FOR ANALYSIS, LIMIT TO ONLY PROJECTS ALLOCATED TO COUNTRIES

#Get expenditure data 
test.c <- colnames(data) %in% NA
data <- data[!test.c]

data.c <- filter(data, assCountry == 1)
########################################################################################################

##########################
# -- CREATE BIPARTITE -- #
##########################

data.c$donor[data.c$donor %in% c("USAID","U.S. State Department")] <- "USA"
temp <- data.c
#select only needed variables
temp <- select(temp,donor,country,budget)
temp$country[temp$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
temp$country[temp$country == "Cameroun"] <- "Cameroon"

#remove NA budgets
temp <- filter(temp, !is.na(budget))

#add unique key to each project for merging later
temp$key <- paste0("key",1:nrow(temp))

#split countries for more than one country, two times for using
# key as name. Then merge on key later to apply budget to country
x1 <- strsplit(temp$country,",")

#add a denominator to temp based on # of countries
temp$denom <- sapply(x1,length) 
temp$totl <- temp$budget/temp$denom #changing totl to be divided by number of countries

#get rid of scientific notation
options(scipen = 999)

names(x1) <- temp$key

x1 = lapply(names(x1), function(name){
  data.frame(key = name,
             country = x1[[name]])
})

countries <- do.call("rbind", x1)

budget <- select(temp,key,totl, donor)

master <- merge(countries,budget, by = "key", all = TRUE)
master <- select(master,country,donor,totl,key)
master <- arrange(master, country)
master$country <- countrycode(master$country,
                              origin = "country.name",
                              destination = "country.name")



#################
##CREATE NODES ##
#################
donors.df <- data.frame(name = unique(master$donor),
                        type = rep('donor', n = nrow(master)))

countries.df <- data.frame(name = unique(as.character(master$country)),
                           type = rep('country', 
                                      n = length(unique(master$country))))

nodes <- rbind(donors.df,countries.df)
nodes$name <- as.character(nodes$name)

nodes$id <- paste0("id",1:nrow(nodes))
nodes <- select(nodes, id, name, type)

nodes$type <- as.character(nodes$type)
nodes$name <- as.character(nodes$name)
nodes$name.type[nodes$type == "donor"] <- 1
nodes$name.type[nodes$type == "country"] <- 2

nodes$name1[nodes$name.type == 2] <- countrycode(nodes$name[nodes$name.type == 2], 
                                                 origin = "country.name",
                                                destination = "country.name")



nodes$name1[nodes$name.type == 1] <- nodes$name[nodes$name.type == 1]

nodes$name <- nodes$name1
nodes <- select(nodes, - length(nodes))


#add continent column

region.df <- select(countrycode_data,country.name,continent)


nodes.country <- merge(nodes[nodes$name.type == 2,], region.df, by.x = "name", by.y = "country.name")

nodes.donor <- filter(nodes, name.type == 1)
nodes.donor$continent  <- rep("Donor",n = nrow(nodes.donor))

nodes <- rbind(nodes.country,nodes.donor)

nodes <- select(nodes,id,name,name.type,type,continent)
unique(nodes$continent)


nodes$continent.type[nodes$continent == "Asia"] <- 1
nodes$continent.type[nodes$continent == "Africa"] <- 2
nodes$continent.type[nodes$continent == "Americas"] <- 3
nodes$continent.type[nodes$continent == "Oceania"] <- 4
nodes$continent.type[nodes$continent == "Europe"] <- 5
nodes$continent.type[nodes$continent == "Donor"] <- 6

##################
##CREATE LINKS
###################
links <- select(master,country,donor,totl)
temp <- select(nodes,id, name)

##merge ids for countries & rename
links <- merge(links,temp, by.x = "country", by.y = "name")
names(links)[names(links)== "id"] = "to"

##merge ids for donors
links <- merge(links,temp, by.x = "donor", by.y = "name")
names(links)[names(links)== "id"] = "from"


links <- select(links, to, from, totl)
names(links)[names(links)== "totl"] = "weight"
links <- select(links, from, to, weight)

#nrow(links); nrow(unique(links[,c("from", "to")])) #MORE LINKS THAN UNIQUE TO/FR
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[3] <- "weight"
rownames(links) <- NULL

########################
## add faoClass       ##
########################
source("R/faoClass.R")
nodes <- faoClass(nodes = nodes, links = links)

####################################
## CREATE OBJECT GLOBAL BIPARTITE ##
##        NO WEIGHTS              ##
####################################
net <- graph.data.frame(links, nodes, directed=T)
net <- simplify(net, remove.multiple = F, remove.loops = T)

#set colors by region and layout
pal <- terrain.colors(6, alpha = .8)
colrs <- pal
V(net)$color <- colrs[V(net)$continent.type]

l <- layout.fruchterman.reingold(net, repulserad=vcount(net)^4,
                                 area=vcount(net)^2.4)
set.seed(28)
# plot(net, layout=layout.fruchterman.reingold,
#           edge.arrow.size=.2,
#           edge.arrow.mode = 0)
# legend(x=1, y= -.5,c("Asia","Africa","Americas",
#                          "Oceania","Europe","DONOR"),
#       pch=21,col="#777777",pt.bg=colrs,pt.cex=2,cex=.8,bty="n",ncol=1, 
#       box.lwd = .1, x.intersp = .2, y.intersp = .2)
# 


####INSERT ISO3 CODES 
source("R/convertIso.R")
nodes <- convertIso(df = nodes)


####################################
## CREATE OBJECT AFRICA BIPARTITE ##
##        WEIGHTED LINKS          ##
####################################
nodes.af <- filter(nodes, continent.type == 2 | continent.type == 6)
links.af <- links

#HACK TO REMOVE TURKEY and congo B/C LISTED AS DONOR AND COUNTRY
congo.id <- nodes.af$id[nodes.af$name == "Congo"]

nodes.af <- filter(nodes.af, name != "Turkey")
nodes.af <- filter(nodes.af, name != "Congo")
nodes.af <- filter(nodes.af, name != "COG")


links.af <- filter(links.af, from != congo.id)

#filter links to be sure links are in nodes
links.test <- links.af$to %in% nodes.af$id 
links.af <- links.af[links.test,]

#filter out unused donors
nodes.test <- (nodes.af$continent.type == 6) & !(nodes.af$id %in% links.af$from)
nodes.af <- filter(nodes.af, !nodes.test)

#create bipartite object
net <- graph.data.frame(links.af, nodes.af, directed=T)
net <- simplify(net, remove.multiple = F, remove.loops = T)

#set colors
colrs = adjustcolor(c("blue", "lightblue","grey","red"))

#set weights
E(net)$width <-E(net)$weight/(exp(13)) # set weights!

#define shaded region
fao.test <- nodes.af$name[nodes.af$faoClass %in% c(1,2)]
vert <- names(V(net))
mark <- grep("TRUE",vert %in% fao.test)


# set layout 
set.seed(3200)
#set.seed(31)
set.seed(30)
l <- layout.fruchterman.reingold(net, repulserad=vcount(net),
                                  area=vcount(net))
# plot
plot(net,
     edge.arrow.size=.2,
     edge.arrow.mode = 0,
     layout = l,
     vertex.color = colrs[V(net)$faoClass],
     vertex.size = 11,
     vertex.label.family = "Arial",
     vertex.label.color = "black",
     mark.groups = mark, mark.col = "#C5E5E7", mark.border = NA,
     main = "Network of Agricultural Statistical \n Capacity Development \n in Africa")
legend(x=.8, y= .5,c("FAO only","FAO and other donors","No FAO involvement",
                      "Donor"),
                        pch=21,col="#777777",pt.bg=colrs,pt.cex=2,cex=.8,bty="n",ncol=1, 
                        box.lwd = .1, x.intersp = .1, y.intersp = .4)

####################################
## CREATE OBJECT ASIA BIPARTITE   ##
##        WEIGHTED LINKS          ##
####################################
nodes.as <- filter(nodes, continent.type == 1 | continent.type == 6)
links.as <- links

#HACK TO REMOVE TURKEY and congo B/C LISTED AS DONOR AND COUNTRY
congo.id <- nodes.as$id[nodes.as$name == "Congo"]

#nodes.as <- filter(nodes.as, name != "Turkey")
#nodes.as <- filter(nodes.as, name != "TUR")
nodes.as <- filter(nodes.as, name != "Congo")
nodes.as <- filter(nodes.as, name != "COG")


links.as <- filter(links.as, from != congo.id)

#links.as <- filter(links.as, from != "id29")
#links.as <- filter(links.as, from != "id2")


#filter links to be sure links are in nodes
links.test <- links.as$to %in% nodes.as$id 
links.as <- links.as[links.test,]

#filter out unused donors
nodes.test <- (nodes.as$continent.type == 6) & !(nodes.as$id %in% links.as$from)
nodes.as <- filter(nodes.as, !nodes.test)

#create bipartite object
net <- graph.data.frame(links.as, nodes.as, directed=T)
net <- simplify(net, remove.multiple = F, remove.loops = T)

#set colors
colrs = adjustcolor(c("blue", "lightblue","grey","red"))

#set weights
E(net)$width <-E(net)$weight/(exp(13)) # set weights!

#define shaded region
fao.test <- nodes.af$name[nodes.af$faoClass %in% c(1,2)]
vert <- names(V(net))
mark <- grep("TRUE",vert %in% fao.test)


# set layout 
#set.seed(31)
set.seed(30)
l <- layout.fruchterman.reingold(net, repulserad=vcount(net)^4,
                                 area=vcount(net)^2.4)
# plot
plot(net,
     edge.arrow.size=.2,
     edge.arrow.mode = 0,
     layout = l,
     vertex.color = colrs[V(net)$faoClass],
     vertex.size = 11,
     vertex.label.family = "Arial",
     vertex.label.color = "black",
     mark.groups = mark, mark.col = "#C5E5E7", mark.border = NA,
     main = "Network of Agricultural Statistical \n Capacity Development \n in Africa")
    legend(x=.8, y= .5,c("FAO only","FAO and other donors","No FAO involvement",
                     "Donor"),
       pch=21,col="#777777",pt.bg=colrs,pt.cex=2,cex=.8,bty="n",ncol=1, 
       box.lwd = .1, x.intersp = .1, y.intersp = .4)
