#Prepare df for map


mapAllYears_TEMP <- function(df = data, 
                        include.donors = TRUE,
                        only.FAO = FALSE){
  
  
  #--------CREATE DF FOR EXPENDITURES BY COUNTRY AND MERGE CENTROIDS
  
  if (only.FAO == FALSE){
  country <- filter(data, assCountry == 1)
  
  
  #Get expenditure data 
  test.c <- colnames(country) %in% NA
  country <- country[!test.c]
  
  data.d <- select(country, country, budget, donor)
  data.d <- filter(data.d, !is.na(budget))
  
  x <- strsplit(data.d$country,",")
  
  data.d$denom <- sapply(x,length)
  
  options(scipen = 999)
  
  data.d$totl <- data.d$budget/data.d$denom
  
  #ADD LETTERS TO REMOVE DUPLICATES! ##
  test <- duplicated(data.d$totl)
  n <- 1:length(data.d$totl[test])
  
  sam <- sample(letters[1:26], length(n), FALSE)
  
  data.d$totl[test] <- paste0(data.d$totl[test], sam)
  
  if(sum(duplicated(data.d$totl)) > 1 ) print("STOP NAMES DUPLICATED!")
  ####################################################
  names(x) <- data.d$totl
  
  
  
  x1 = lapply(names(x), function(name){
    data.frame(amount = name,
               country = x[[name]])
  })
  
  master <- do.call("rbind", x1)
  master$country <- as.character(master$country)
  
  ##Remove letter used to create unique values
  master$amount <- gsub(pattern = "[a-z]+$","",master$amount)
  master$amount <- as.numeric(as.character(master$amount))
  master$country[master$country == "Cameroun"] <- "Cameroon"
  master$country <- countrycode(master$country, origin = "country.name",
                                destination = "country.name", warn = TRUE)
  
  master <- master %>% 
    group_by(country) %>%
    dplyr::summarize(amount = sum(amount))
  
  #Get data frame w/ polygons and centroids
  centroid <- as.data.frame(fao_world_centroids)
  centroid <- select(centroid,ADM0_NAME,x,y)
  colnames(centroid) <- c("country","clong","clat")
  centroid$country <- as.character(centroid$country)
  
  temp <- data.frame(country = c("Cabo Verde","Comoros", "Kiribati", "Tonga", "Maldives"),
                     clong = c(-23.627155,43.335468,-157.409388,-175.161552,73.5361),
                     clat = c(15.088946,-11.664959,1.881687,-21.172421,1.97724))
  centroid <- rbind(temp,centroid)
  
  centroid$country <- countrycode(centroid$country, origin = "country.name",
                                  destination = "country.name", warn = TRUE)
  
  master <- merge(master,centroid, by = "country", all = TRUE)
  
  master <- filter(master, !is.na(amount))
  master <- filter(master, !(country == "India" & clat > 23))
  
  if(sum(duplicated(master$country)) > 0) print("duplicated country!")
  
  
  fao_world2 = fortify(fao_world)
  countryName = fao_world@data
  countryName$id = rownames(countryName)
  
  fao_world2 = merge(fao_world2, countryName, by = "id")
  
  #filter data
  master.t <- filter(master,!is.na(amount))
  fao_world2 <- filter(fao_world2, ADM0_NAME != "Antarctica")
  fao_world2 <- arrange(fao_world2,order)
  
  master.t$country  <- gsub("^ +","", master.t$country)
  
  
  ##---CREATE DF FOR DONORS BY COUNTRY AND MERGE
  donors <- select(data.d,country,donor)
  x <- strsplit(donors$country,",")
  
  names <- donors$donor
  test <- data.frame(names = as.character(names), rep = sapply(x,length))
  
  y <- c()
  
  #create vector of donors
  for(i in 1:nrow(test)){
    temp <- rep(as.character(test[i,1]), times = test[i,2])
    y <- append(y,temp)
  }
  
  #bind donors w/ unlisted countries
  master.c <- data.frame(donors = y, country = unlist(x))
  master.c$country <- as.character(master.c$country)
  master.c$country[master.c$country == "Cameroun"] <- "Cameroon"
  master.c$country <- countrycode(master.c$country, origin = "country.name",
                                  destination = "country.name", warn = TRUE)
  

  ##For some projects, there are > 1 donors. need to split and make more rows
  source("R/sepDonors.R")
  master.c <- sepDonors(master.c,
                        donor.names.only = TRUE)
  
  #create df w/ # of donors for merger
  master.c <- master.c %>%
      group_by(country) %>%
      summarize(donors = length(unique(donors)))
  
  
  master <- merge(master, master.c, all = TRUE)
  
  
  ## PLOT
  
  breaks = c(1000000,5000000,10000000,15000000)
  
  #format donors variables
  names(master)[names(master) == "donors"] <- "Donors"
  
  if(include.donors == TRUE){
  map <- ggplot() +
            geom_polygon(data=fao_world2, aes(x=long, y=lat, group = group),colour="darkgrey", fill="white" ) +
            theme_classic() +
            theme(axis.line = element_blank(), 
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank()) +
            geom_point(data=master, aes(x=clong, y=clat, size = amount, fill = Donors), 
                       shape = 21, color = "lightgrey") +
    scale_fill_gradient(low = "whitesmoke", high = "black") +
          scale_size(#max_size = 10,
                    range = c(3,12),
                    breaks = breaks,
                    name = "Total Funding \n(million USD)",
                    labels = c("1", "5",
                             "10","15")) + 
                    geom_text(size=4) 
  
  } else if (include.donors == FALSE){
    
  map <- ggplot() +
          geom_polygon(data=fao_world2, aes(x=long, y=lat, group = group),colour="darkgrey", fill="white" ) +
          theme_classic() +
          theme(axis.line = element_blank(), 
                axis.text.x = element_blank(), 
                axis.text.y = element_blank(),
                axis.ticks = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank()) +
          geom_point(data=master, aes(x=clong, y=clat, size = amount)) +
          scale_size(#max_size = 10,
                    range = c(3,12),
                    breaks = breaks,
                    name = "Total Funding \n(million USD)",
                    labels = c("1", "5",
                               "10","15")) + 
          geom_text(size=4) +
          ggtitle(label = "Sum of total budgets of Statistical Capacity Building Projects \n active in 2015")
    
  } 
  } else if (only.FAO == TRUE){
    
    ##--- SHOWING FAO AS IMPLEMENTER
    impl <- filter(data, assCountry == 1)
    impl <- select(impl, implementer, country, budget)
    
    #filter for only project where FAO is the implementer
    fao.test <- grepl("FAO", impl$implementer)
    impl <- impl[fao.test,]
    
    #remove multiple implementers
    impl$implementer <- gsub(".+, ", "",impl$implementer)
    
    x <- strsplit(impl$country,",")
    
    impl$denom <- sapply(x,length)
    
    options(scipen = 999)
    
    impl$totl <- impl$budget/impl$denom
    
    #ADD LETTERS TO REMOVE DUPLICATES! ##
    test <- duplicated(impl$totl)
    n <- 1:length(impl$totl[test])
    
    sam <- sample(letters[1:26], length(n), FALSE)
    
    impl$totl[test] <- paste0(impl$totl[test], sam)
    
    if(sum(duplicated(impl$totl)) > 1 ) print("STOP NAMES DUPLICATED!")
    ####################################################
    names(x) <- impl$totl
    
    
    
    x1 = lapply(names(x), function(name){
      data.frame(amount = name,
                 country = x[[name]])
    })
    
    master <- do.call("rbind", x1)
    master$country <- as.character(master$country)
    
    ##Remove letter used to create unique values
    master$amount <- gsub(pattern = "[a-z]+$","",master$amount)
    master$amount <- as.numeric(as.character(master$amount))
    master$country[master$country == "Cameroun"] <- "Cameroon"
    master$country <- countrycode(master$country, origin = "country.name",
                                  destination = "country.name", warn = TRUE)
    
    
    master <- master %>% 
      group_by(country) %>%
      dplyr::summarize(amount = sum(amount))
    
    #Get data frame w/ polygons and centroids
    centroid <- as.data.frame(fao_world_centroids)
    centroid <- select(centroid,ADM0_NAME,x,y)
    colnames(centroid) <- c("country","clong","clat")
    centroid$country <- as.character(centroid$country)
    
    temp <- data.frame(country = c("Cabo Verde","Comoros", "Kiribati", "Tonga", "Maldives"),
                       clong = c(-23.627155,43.335468,-157.409388,-175.161552,73.5361),
                       clat = c(15.088946,-11.664959,1.881687,-21.172421,1.97724))
    centroid <- rbind(temp,centroid)
    
    centroid$country <- countrycode(centroid$country, origin = "country.name",
                                    destination = "country.name", warn = TRUE)
    
    master <- merge(master,centroid, by = "country", all = TRUE)
    
    master <- filter(master, !is.na(amount))
    master <- filter(master, !(country == "India" & clat > 23))
    
    if(sum(duplicated(master$country)) > 0) print("duplicated country!")
    
    
    fao_world2 = fortify(fao_world)
    countryName = fao_world@data
    countryName$id = rownames(countryName)
    
    fao_world2 = merge(fao_world2, countryName, by = "id")
    
    #filter data
    master.t <- filter(master,!is.na(amount))
    fao_world2 <- filter(fao_world2, ADM0_NAME != "Antarctica")
    fao_world2 <- arrange(fao_world2,order)
    
    master.t$country  <- gsub("^ +","", master.t$country)
    
    ##plot
    map <- ggplot() +
                geom_polygon(data=fao_world2, aes(x=long, y=lat, group = group),
                             colour="darkgrey", fill="white" ) +
                theme_classic() +
                theme(axis.line = element_blank(), 
                      axis.text.x = element_blank(), 
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(), 
                      axis.title.x = element_blank(), 
                      axis.title.y = element_blank()) +
                geom_point(data=master.t, aes(x=clong, y=clat, size = amount)) +
                scale_size(#max_size = 10,
                           range = c(3,11),
                           breaks = breaks,
                           name = "Total Funding \n(million USD)",
                           labels = c("1", "5",
                            "10","15")) + 
                          geom_text(size=4)
  }
  
  
  
map  
}