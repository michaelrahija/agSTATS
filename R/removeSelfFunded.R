##' Remove country self-funded projects
##' 
##' The function takes a clean df (after running cleanFigures.R)
##' and returns a df removing the self-funded projects. Specifically,
##' 
##' @param data is a cleaned data frame
##' 
##' @return a data frame
##'
##' @export
##'

removeSelfFunded <- function(df = data){
  
  if(!("donor" %in% colnames(df))){
    stop("No donor column")
  }
  
  if(!("country" %in% colnames(df))){
    stop("No country column")
  }
  
  self.funded <- df$donor == df$country
  
  df <- df[!self.funded,]
  
  #remove 25% budget for agId 74
  if(!("Turkey" == df$donor[df$agId == "74"])){
    stop("agId 74 isn't for Turkey. agIds may have changed")
  }
  
  df$budget[df$agId == "74"] <-  df$budget[df$agId == "74"] - (.25 * df$budget[df$agId == "74"])
  df$country[df$agId == "74"] <- gsub("Turkey, ","", df$country[df$agId == "74"])

df    
}