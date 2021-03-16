  # rename some of the columns to remove special characters or encoding
  names(datainterpol)[1:2] <- c("Authors", "AuthorID")
  names(datainterpol)[5]<-c("Source.title")
  names(datainterpol)[17:18]<-c("Author.Keywords", "Index.Keywords")
  
  
  CountryDataSet <- datainterpol %>%
    select(Year,Title,Source.title,Authors,AuthorID,Affiliations) %>%
    distinct()
  
  #############################################################
  #####                    Countries                      #####
  #############################################################
  
  # get city/country data
  data(world.cities)
  
  # replace "United States" with USA & "United Kingdom" with UK.
  aff.lst <- gsub("United States", "USA", CountryDataSet$Affiliations, perl = TRUE)
  aff.lst <- gsub("United Kingdom", "UK", aff.lst, perl = TRUE)
  # replace ';' with ',' as multiple affiliations are separated with ';'
  # but that doesn't fit with the strsplit()
  aff.lst <- gsub(";", ",", aff.lst)
  # split fields by ", "
  splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
  # extract fields which match a known city making sure that diacritics aren't a problem...
  city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])
  
  # this version only returns unique instances of countries per publication
  cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in% world.cities$country.etc)]))
  
  cntry.dat <- data.frame(Country = removeDiacritics(unlist(cntry.lst)), stringsAsFactors = FALSE)
  
  # define continent for each country
  cntry.dat$Continent <- countrycode(sourcevar = cntry.dat[, "Country"],
                                     origin = "country.name",
                                     destination = "continent")
  # get number of record per countries
  Total <- cntry.dat %>% 
    group_by(Country, Continent) %>% 
    dplyr::summarise(Count = n())
  
  threshold <-sort(Total$Count,T)[20]
  threshold <- as.numeric(threshold)
  
  # get countries under threshold
  other.dat <- cntry.dat %>% 
    group_by(Country, Continent) %>% 
    dplyr::summarise(Count = n()) %>% 
    filter(Count <= threshold)
  # aggregate counts as 'Others'
  other.dat <- data.frame(Country = "Others", Continent = "Other", Count = sum(other.dat$Count))
  
  # Collate counts for countries over threshold
  cntry.dat <- cntry.dat %>% 
    group_by(Country, Continent) %>% 
    dplyr::summarise(Count = n()) %>% 
    filter(Count >= threshold)
  # order by count
  cntry.dat$Country <- reorder(cntry.dat$Country, +cntry.dat$Count)
  # add in 'Others'
  cntry.dat <- rbind(other.dat, data.frame(cntry.dat))
  # plot
  # view(cntry.dat)