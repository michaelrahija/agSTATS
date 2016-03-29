##' Converts df to long format w/ seperate donors in seperate rows
##' 
##' 
##' 
##' @param df is a dataframe w/ donor and country columns
##' @param donor.names.only = TRUE, excludes percentages, donor.names.only = FALSE includes %s
##'   
##' @return a data frame
##'
##' @export
##' 

sepDonors <- function(df = master.c,
                      donor.names.only = TRUE){
  
  
  #create column for each unique donor, make df wide based on donor
  test <- separate(df, donors, 
                   into = c("Donor1","Donor2","Donor3"),#, "Donor4","Donor5"), 
                   sep = ",", 
                   extra ="drop")
  
  #convert df long based on donors
  df <- gather(test,country, na.rm = TRUE)
  colnames(df) <- c("country","donor_id","donors")
  
  if(donor.names.only == TRUE){
    
    df$donors <- gsub("[0-9]+","",df$donors)
    df$donors <- gsub("%+","",df$donors)
    df$donors <- gsub("\\(","",df$donors)
    df$donors <- gsub("\\)","",df$donors)
    df$donors <- gsub("\\.","",df$donors)
    df$donors <- gsub("^\\s+|\\s+$", "", df$donors)

  }

df  
}

