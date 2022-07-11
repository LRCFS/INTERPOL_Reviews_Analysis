rm(list=ls())

library(tidyverse)
library(expss)
library(dataCompareR)
library(dplyr)
library(plyr)


#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported is generated from Scopus.
# The columns will need to contain:
#   Year; Title; Source.title; Authors; AuthorID; DE; DES; EID


#library(dplyr)
#library(stringr)
#library(tidyr)
#library(hablar)
#library(ggplot2)
#library(maps)
#library(countrycode)
#library(RColorBrewer)
#library(bibliometrix)

#############################################################
#####                      Function  1/4                #####
#############################################################

#### Function to search and replace ####

# Include function to duplicate {Marc Schwartz (via MN) on http://r.789695.n4.nabble.com/replace-values-in-data-frame-td803416.html}
gsr <- function(Source, Search, Replace) 
{ 
  if (length(Search) != length(Replace)) 
    stop("Search and Replace Must Have Equal Number of Items\n") 
  
  Changed <- as.character(Source) 
  
  for (i in 1:length(Search)) 
  { 
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n") 
    Changed <- replace(Changed, Changed == Search[i], Replace[i]) 
  } 
  
  cat("\n") 
  
  Changed 
}

#############################################################
#####                      Function  2/4                #####
#############################################################

# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}

#############################################################
#####                      Function  3/4                #####
#############################################################

# Function, adapted from https://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/

# Function to check for duplicate and partial match between database imports. The generated list can then be used to make the appropriate changes to the lists

#Data loaded in from downloaded files as:
##PercentageUsingTheNet
##ccode

##Here's where the algorithm starts...
##I'm going to generate a signature from country names to reduce some of the minor differences between strings
##In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
##So for example, United Kingdom would become kingdomunited
##We might also remove stopwords such as 'the' and 'of'.

# Separator <- list(c("\\. |\\.| | \\& |\\: | \\ - |\\-|\\ -|\\- |\\-"))

signature=function(x){
  #  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
  sig=paste(sort(unlist(strsplit(x," "))),collapse = '')
  #  sig=paste(x)
  #  sig=paste(x, sep =" ", collapse = " ")
  sig=x
  return(sig)
}

number = 8.0

partialMatch=function(x,y,levDist = number){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))
  
  return(matched)
}

#############################################################
#####                      Function  4/4                #####
#############################################################


rem_dup_word <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split="\\, ",fixed=F,perl=T)))),collapse = 
          " ")
}

#########################################################
#####     INTERPOL ORIGINAL LIST OF REFERENCES      #####
#########################################################

##### Data loading 1: INTERPOL Reference List
INTERPOLOriginal <- read.csv("INTERPOL/All_data_shiny.csv", sep=",", header=TRUE)

# Fill every empty cell with NA
INTERPOLOriginal[INTERPOLOriginal==""]<- NA

# rename some of the columns to remove special characters or encoding
names(INTERPOLOriginal)[2] <- c("Title")

# Remove duplicates and select the Year and EID of each record
INTERPOLOriginalReduced <- INTERPOLOriginal %>%
  select(Year,Authors,AuthorID,Title,EID) %>%
  distinct()
INTERPOLOriginalReduced$Title <- as.character(INTERPOLOriginalReduced$Title)

# Remove NA cells
INTERPOLOriginalReducedNarrow <- INTERPOLOriginalReduced[complete.cases(INTERPOLOriginalReduced$Title),]

INTERPOLOriginalReducedNarrow$Title <- toupper(INTERPOLOriginalReducedNarrow$Title)

# # replace specific non-alphanumeric characters 
# INTERPOLOriginalReducedNarrow$Title <- str_replace_all(INTERPOLOriginalReducedNarrow$Title, "Ó", " ")
# INTERPOLOriginalReducedNarrow$Title <- str_replace_all(INTERPOLOriginalReducedNarrow$Title, "Ñ", " ")
# 
# # replace all non-english characters 
# INTERPOLOriginalReducedNarrow$Title <- removeDiacritics(INTERPOLOriginalReducedNarrow$Title)
# 
# # swap out all non-alphanumeric characters 
# INTERPOLOriginalReducedNarrow$Title <- str_replace_all(INTERPOLOriginalReducedNarrow$Title, "[^[:alnum:]]", " ")
# # remove the double space the previous line create
# INTERPOLOriginalReducedNarrow$Title <- str_replace_all(INTERPOLOriginalReducedNarrow$Title, "  ", " ")
# 
# # remove leadind and trailing white space
# INTERPOLOriginalReducedNarrow$Title <- trimws(INTERPOLOriginalReducedNarrow$Title)

INTERPOLOriginalReducedNarrow <- INTERPOLOriginalReducedNarrow %>%
  distinct()

##### Data loading 2: Scopus Reference List
#filenames <- list.files("Scopus/BE/", pattern="*.csv", full.names=TRUE)
#ScopusOriginal <- rbindlist(lapply(filenames,fread, encoding='UTF-8'))


#########################################################
#####    INTERPOL LIST OF REFERENCE TO BE ADDED     #####
#########################################################

##### Data loading: New INTERPOL Reference List
# INTERPOLNew <- read.csv("INTERPOL/Temp/IFSMS_FS.csv", sep=",", header=TRUE)

#### If more than one list to load ####
# load INTERPOL data
filenames <- list.files("INTERPOL/Temp/", pattern=".csv", full.names=TRUE)

# merge together
INTERPOLNew <- ldply(filenames, read_csv)
INTERPOLNew <- INTERPOLNew %>%
  distinct()
#######################################

# Fill every empty cell with NA
INTERPOLNew[INTERPOLNew==""]<- NA

# rename some of the columns to remove special characters or encoding
names(INTERPOLNew)[1:2] <- c("Authors", "AuthorID")

# Remove duplicates and select the Year and EID of each record
INTERPOLNewReduced <- INTERPOLNew %>%
  select(Year,Authors,AuthorID,Title,EID) %>%
  distinct()

INTERPOLNewReduced$Title <- as.character(INTERPOLNewReduced$Title)

# Remove empty EID rows in INTERPOLNewReduced
INTERPOLNewReducedNarrow <- INTERPOLNewReduced[complete.cases(INTERPOLNewReduced$Title),]

# swap out all non-alphanumeric characters 
INTERPOLNewReducedNarrow$Title <- str_replace_all(INTERPOLNewReducedNarrow$Title, "[^[:alnum:]]", " ")

INTERPOLNewReducedNarrow$Title <- toupper(INTERPOLNewReducedNarrow$Title)

# replace specific non-alphanumeric characters 
INTERPOLNewReducedNarrow$Title <- str_replace_all(INTERPOLNewReducedNarrow$Title, "Ó", " ")
INTERPOLNewReducedNarrow$Title <- str_replace_all(INTERPOLNewReducedNarrow$Title, "Ñ", " ")

# replace all non-english characters 
INTERPOLNewReducedNarrow$Title <- removeDiacritics(INTERPOLNewReducedNarrow$Title)

# remove the double space the previous line create
INTERPOLNewReducedNarrow$Title <- str_replace_all(INTERPOLNewReducedNarrow$Title, "  ", " ")

# remove leading and trailing white space
INTERPOLNewReducedNarrow$Title <- trimws(INTERPOLNewReducedNarrow$Title)

INTERPOLNewReducedNarrow <- INTERPOLNewReducedNarrow %>%
  distinct()

# Correction to the title can be applied at this stage. This can be done in Notepad++, Excel etc.
# The title for the first evidence type in alphabetical order will be used to correct the second one. 
# Missed duplicate or partial can be added as well. This is the case here with one extra correction added
# Applying the title correction does not necessary mean there will not be further partial match is the function is rerun on the new (corrected) Scopus dataframe. 
#TitleCorrection <- read.csv("TitleCorrection.txt", sep = "\t", header = TRUE)
#TitleCorrection <- as.data.frame(TitleCorrection)

#ScopusES$Title <- gsr(as.character(ScopusES$Title),as.character(TitleCorrection$raw.y),as.character(TitleCorrection$raw.x))


#############################################################
#####         Count INTERPOL vs. INTERPOL matches         #####
#############################################################

CountPublicationINTERPOL <-subset(INTERPOLNewReducedNarrow,Title %in% INTERPOLOriginalReducedNarrow$Title)
CountMatches <- CountPublicationINTERPOL %>%
  distinct()

########################################################
##### Search for match by title between the two datasets
##### to generate a list of titles with a partial match for external check

matches=partialMatch(INTERPOLNewReducedNarrow$Title,INTERPOLOriginalReducedNarrow$Title)

# x.raw refers to the New reference List.
# y.raw refers to the Original reference List.
# The Original list is used to correct any suitable partial duplicates 

write.csv(matches, file = "CorrectionLists/Duplicate_Partial_Match.csv", row.names = F)

##### This part is to create the correction list for the title
##### It is then loaded earlier in the code: currently commented 

aggregate(matches$pass, by=list(matches$pass), FUN=length)

# x.raw refers to the New reference List.
# y.raw refers to the Original reference List.
# The Original list is used to correct any suitable partial duplicates 

PartialExport <- matches %>% filter(pass == "Partial")

# The PartialExport can be written to a table and further processed manually using fo example Notepad++, Excel, etc.
# This correction need to be added to the list of title correction applied at the top.

write.csv(PartialExport, file = "CorrectionLists/Partial_Match.csv", row.names = F)

# Correction to the title can be applied at this stage. This can be done in Notepad++, Excel etc.
# The title for the first evidence type in alphabetical order will be used to correct the second one. 
# Missed duplicate or partial can be added as well. This is the case here with one extra correction added
# Applying the title correction does not necessary mean there will not be further partial match if the function is rerun on the new (corrected) Scopus dataframe. 
#TitleCorrection <- read.csv("TitleCorrection.txt", sep = "\t", header = TRUE)


