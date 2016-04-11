##' Create maps which shows the estimated flow of assistance for ag stats
##' 
##' The function takes a clean df (after running cleanFigures.R)
##' and returns a bubble map with the amount of funding to each country
##' 
##' @param data is a cleaned data frame
##' @param include.donors if = TRUE, includes the # of donors in each country, if = FALSE, does not
##'   
##' @return a ggplot
##'
##' @export
##' 

implementerMap <- function(df= data, 
                           implementing.institution = c("FAO","USDA",
                                                      "EU","World Bank","WB")){
  
  if(!(implementing.institution %in% c("FAO","USDA",
                                     "EU","World Bank","WB"))){
    stop(paste0(implementing.institution," ","is not an option."))
  }
  
  
  if(!(colnames(df)[1] == "agId")){
    stop("Run cleaning script!")
  }
  
  #filter for implementer
  df <- filter(df, implementer == implementing.institution)
  
  source("R/flowMap.R")
  map <- flowMap(data = df,
                 include.donors = FALSE,
                 fixed.range = c(1,5))
  
  map <- map + ggtitle(paste("Implemented by",
                             implementing.institution))

map    
}