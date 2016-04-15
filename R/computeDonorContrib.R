##' Seperates donors into unique rows with a column for percentage of
##' budget
##' 
##' The function takes a clean df (after running cleanFigures.R)
##' and returns a df with country, budget, agId, key (donor#), donor, percent
##' 
##' @param data is a cleaned data frame
##' 
##' @return a data frame
##'
##' @export
##'

computeDonorContrib <- function(df = data){
  

  #take only relevent columns
  df <- select(df, donor, country, budget,agId)
  
  #create column names for spreading donor
  donor.cols <- paste0("Donor",1:11)
  
  #seperate donor column
  test <- separate(df, donor,
                   into = donor.cols,
                    sep = ",",
                    extra = 'drop')
  
  #gather into one column called donor. keep relevant columns
  x <- test %>%
          gather(key, donor,-country,-budget, - agId, na.rm = TRUE)
  
  
  x.arr <- arrange(x,agId)
  
  #create percentage variable for budget allocation
  x.arr <- separate(x.arr, donor,
                    into = c("donor","percent"),
                    sep = "\\(",
                    extra = "drop")
  
  x.arr$percent <- gsub("\\%","",x.arr$percent)
  x.arr$percent <- gsub("\\)","",x.arr$percent)
  x.arr$percent[is.na(x.arr$percent)] <- 100
  x.arr$percent <- as.numeric(x.arr$percent)/100
  
  #trim white space from donor names 
  #taken from http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  x.arr$donor <- trim(x.arr$donor)

x.arr    

}  