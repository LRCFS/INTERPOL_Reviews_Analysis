source('Code/Functions/Diacritics.R')

# function to extract countries from affiliations
# returns the same number of rows as input
extractCountries <- function(aff.df , institutions = FALSE) {

  # get city/country data
  data(world.cities)
  
  # replace "United States" with USA & "United Kingdom" with UK.
  aff.df <- gsub("United States", "USA", aff.df, perl = TRUE)
  aff.df <- gsub("United Kingdom", "UK", aff.df, perl = TRUE)
  # replace ';' with ',' as multiple affiliations are separated with ';'
  # but that doesn't fit with the strsplit()
  aff.df <- gsub(";", ",", aff.df)
  # split fields by ", "
  splt.lst <- sapply(aff.df, strsplit, split = ", ", USE.NAMES = FALSE)
  cntry.lst <- list()
  if (institutions == TRUE) {
    # extract fields which match a known country and keep all institutional instances per row...
    cntry.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])
  } else {
    # extract fields which match a known country and keep unique instances per row...
    cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in% world.cities$country.etc)]))
  }
  cntry.vec <- unlist(lapply(cntry.lst, paste, collapse = ','))

  return(cntry.vec)
}

# for an array of comma-delimited list of countries, split,
# add continent info and count up.
countryCounts <- function(countries) {
  cntry.lst = strsplit(countries, ',')
  cntry.dat = data.frame(Country = removeDiacritics(unlist(cntry.lst)))
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
  # factrorise Country to avoid rbind() breaking the ordering below
  other.dat$Country <- as.factor(other.dat$Country)
  
  # Collate counts for countries over threshold
  cntry.dat <- cntry.dat %>% 
    group_by(Country, Continent) %>% 
    dplyr::summarise(Count = n()) %>% 
    filter(Count >= threshold)
  # order by count
  cntry.dat$Country <- reorder(cntry.dat$Country, +cntry.dat$Count)
  # add in 'Others'
  cntry.dat <- rbind(other.dat, data.frame(cntry.dat))
  return(cntry.dat)
  
}
