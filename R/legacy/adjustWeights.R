#create weight grouping based on quantiles

adjustWeights <- function(df = NA){
  
  quant <- quantile(links$weight, probs = seq(0,1,.2))
  
  df$Cat[df$weight >= quant[1] & df$weight < quant[2]] <- 1
  df$Cat[df$weight >= quant[2] & df$weight < quant[3]] <- 2
  df$Cat[df$weight >= quant[3] & df$weight < quant[4]] <- 4
  df$Cat[df$weight >= quant[4] & df$weight < quant[5]] <- 6
  df$Cat[df$weight >= quant[5]] <- 10

  if(!(nrow(df) == sum(table(df$Cat)))) {
    stop("Some NAs in weights")
  } 

df  
  
}