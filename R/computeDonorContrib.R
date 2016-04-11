#function to take data frame containing donor, country, budget, and agId
#return dataframe with 1 row for each combination of 1 donor w/ column for 
#% contribution to the agId

##https://blog.rstudio.org/2014/07/22/introducing-tidyr/

df <- select(data, donor, country, budget,agId)


donor.cols <- paste0("Donor",1:11)

test <- separate(df, donor,
                 into = donor.cols,
                  sep = ",",
                  extra = 'drop')

head(test)
tail(test)
x <- test %>%
        gather(key, donor,-country,-budget, - agId)

x <- filter(x, !is.na(donor))

x.arr <- arrange(x,agId)



head(x.arr)
test2 <- gather(test, key = agId, country,value = budget)
head(test2)

test2 <- gather(test,country,budget,agId, na.rm = TRUE)


names(test2) <- c("countries","budget","Donor")
head(test2)

ar.test <- arrange(test2,country)
