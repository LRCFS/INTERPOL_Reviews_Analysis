###########################################################################
#
# Research trends in forensic science: A scientometric approach to analyse 
# the content of the INTERPOL reviews - Copyright (C) 2021
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow

# Hervé Ménard, Oyewumi Akinpelu, Nana A. Fiakpui, Rong (Lily) He, Sarah Huxter,
# Caitlin Jordan, Lucy Judge, Aoife King, Brianna Miller, Sophie E. Moggs,
# Carmen-Teodora Patrascu, Teri Pearson, Eranthi M.E.J. Seneviatne,
# Lotte E. Timmerman, Penelope R. Haddrill, Joyce K. Klu, Christian Cole,
# Niamh Nic Daéid

# Website: https://github.com/LRCFS/INTERPOL_Reviews_Analysis
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
###########################################################################
#
# This code is correct titles in new reference list
# This code should be run after a checking for correction and/or editing
#                 Titlecorrections.csv
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list=ls())

library(tidyverse)
library(expss)
library(dataCompareR)
library(plyr)
library(dplyr)
library(maps)
library(countrycode)
library(stringr)
library(tidyr)

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
#####          INTERPOL LIST OF REFERENCE           #####
#########################################################


#########################################################
#####                   Title                       #####
#########################################################

# load data
filenames <- list.files("INTERPOL/", pattern=".csv", full.names=TRUE)

# merge together
INTERPOLNew <- ldply(filenames, read_csv)

# remove duplicate
INTERPOLNew <- INTERPOLNew %>%
  distinct()

#######################################

# rename some of the columns to remove special characters or encoding
names(INTERPOLNew)[1:2] <- c("Authors", "AuthorID")
names(INTERPOLNew)[5] <- c("Source.title")
names(INTERPOLNew)[9:10] <- c("Page.start", "Page.end")
names(INTERPOLNew)[17:18] <- c("AKeywords", "IKeywords")
names(INTERPOLNew)[27:28] <- c("Reference List", "Evidence Type")
INTERPOLNew$Title <- as.character(INTERPOLNew$Title)

# swap out all non-alphanumeric characters 
INTERPOLNew$Title <- str_replace_all(INTERPOLNew$Title, "[^[:alnum:]]", " ")

INTERPOLNew$Title <- toupper(INTERPOLNew$Title)

# replace specific non-alphanumeric characters 
INTERPOLNew$Title <- str_replace_all(INTERPOLNew$Title, "Ó", " ")
INTERPOLNew$Title <- str_replace_all(INTERPOLNew$Title, "Ñ", " ")

# replace all non-english characters 
INTERPOLNew$Title <- removeDiacritics(INTERPOLNew$Title)

# remove the double space the previous line create
INTERPOLNew$Title <- str_replace_all(INTERPOLNew$Title, "  ", " ")

# remove leading and trailing white space
INTERPOLNew$Title <- trimws(INTERPOLNew$Title)


# Correction to the title can be applied at this stage. This can be done in Notepad++, Excel etc.
# The title for the first evidence type in alphabetical order will be used to correct the second one. 
# Missed duplicate or partial can be added as well. This is the case here with one extra correction added
# Applying the title correction does not necessary mean there will not be further partial match is the function is rerun on the new (corrected) Scopus dataframe. 
TitleCorrection <- read.csv("CorrectionLists/TitleCorrections.csv", sep = ",", header = TRUE)
TitleCorrection <- as.data.frame(TitleCorrection)

INTERPOLNew$TitleCorrected <- gsr(as.character(INTERPOLNew$Title),as.character(TitleCorrection$Original),as.character(TitleCorrection$Corrected))


write.csv(INTERPOLNew, file = "INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", row.names = F)

#########################################################
#####                 Keywords                      #####
#########################################################

#read file
#INTERPOLNew <- read.csv("INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", header = TRUE)

#############################################################
#####                   Authors Keywords                #####
#############################################################
#############################################################

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
INTERPOLNewAKeywordList <- INTERPOLNew %>%
  mutate(AKeywords = strsplit(as.character(AKeywords), ";")) %>% 
  unnest(AKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "INTERPOLNewAKeywordList" and save in dataframe
# Extract list of "AIkeywords" and remove duplicate
INTERPOLNewAKeywordList$AKeywords <- toupper(INTERPOLNewAKeywordList$AKeywords)

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list
KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrectionFull.csv", header=TRUE)
# KeywordsCorrected <- as.data.frame(KeywordsCorrected)
# INTERPOLNewAKeywordList$KeywordsCorrected <- gsr(as.character(INTERPOLNewAKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))
INTERPOLNewAKeywordList$KeywordsCor <- gsr(as.character(INTERPOLNewAKeywordList$AKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))

# exclude the original List of keywords (Authors)
INTERPOLNewAKeywordList <- subset(INTERPOLNewAKeywordList, select = -AKeywords)

# rebuilt the list for KeywordsCor by grouping. Should be the same length as the original (after removing duplicate)
# DPLYR needs to be loaded after Plyr for this to work, or alternatively specify dplyr in the code??
INTERPOLNewAKeywordListTemp <- INTERPOLNewAKeywordList %>%
  select(Authors,AuthorID,TitleCorrected,Year,Source.title,Volume,Issue,Page.start,Page.end,`Page count`,`Cited by`,Link,`Authors with affiliations`,`Correspondence Address`,`Document Type`,`Publication Stage`,`Access Type`,Source,`Active Webpage`,DOI,Affiliations,EID,Report,`Reference List`,`Evidence Type`,IKeywords,KeywordsCor) %>%
 dplyr::group_by(Authors,AuthorID,TitleCorrected,Year,Source.title,Volume,Issue,Page.start,Page.end,`Page count`,`Cited by`,Link,`Authors with affiliations`,`Correspondence Address`,`Document Type`,`Publication Stage`,`Access Type`,Source,`Active Webpage`,DOI,Affiliations,EID,Report,`Reference List`,`Evidence Type`,IKeywords) %>%
  dplyr::summarise(Temp = paste(KeywordsCor,collapse=";"))

# Rename the corrected list to AKeywords
names(INTERPOLNewAKeywordListTemp)[27] <- "AKeywords"

# remove possible duplicatea
INTERPOLNew <- INTERPOLNewAKeywordListTemp %>%
  distinct()

write.csv(INTERPOLNew, file = "INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", row.names = F)

#############################################################
#####                  Database Keywords                #####
#############################################################
#############################################################

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
INTERPOLNewAKeywordList <- INTERPOLNew %>%
  mutate(IKeywords = strsplit(as.character(IKeywords), ";")) %>% 
  unnest(IKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "INTERPOLNewAKeywordList" and save in dataframe
# Extract list of "AIkeywords" and remove duplicate
INTERPOLNewAKeywordList$IKeywords <- toupper(INTERPOLNewAKeywordList$IKeywords)

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list
KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrectionFull.csv", header=TRUE)
# KeywordsCorrected <- as.data.frame(KeywordsCorrected)
# INTERPOLNewAKeywordList$KeywordsCorrected <- gsr(as.character(INTERPOLNewAKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))
INTERPOLNewAKeywordList$KeywordsCor <- gsr(as.character(INTERPOLNewAKeywordList$IKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))

# exclude the original List of keywords (Authors)
INTERPOLNewAKeywordList <- subset(INTERPOLNewAKeywordList, select = -IKeywords)

# rebuilt the list for KeywordsCor by grouping. Should be the same length as the original (after removing duplicate)
# DPLYR needs to be loaded after Plyr for this to work, or alternatively specify dplyr in the code??
INTERPOLNewAKeywordListTemp <- INTERPOLNewAKeywordList %>%
  select(Authors,AuthorID,TitleCorrected,Year,Source.title,Volume,Issue,Page.start,Page.end,`Page count`,`Cited by`,Link,`Authors with affiliations`,`Correspondence Address`,`Document Type`,`Publication Stage`,`Access Type`,Source,`Active Webpage`,DOI,Affiliations,EID,Report,`Reference List`,`Evidence Type`,AKeywords,KeywordsCor) %>%
  dplyr::group_by(Authors,AuthorID,TitleCorrected,Year,Source.title,Volume,Issue,Page.start,Page.end,`Page count`,`Cited by`,Link,`Authors with affiliations`,`Correspondence Address`,`Document Type`,`Publication Stage`,`Access Type`,Source,`Active Webpage`,DOI,Affiliations,EID,Report,`Reference List`,`Evidence Type`,AKeywords) %>%
  dplyr::summarise(Temp = paste(KeywordsCor,collapse=";"))

# Rename the corrected list to IKeywords
names(INTERPOLNewAKeywordListTemp)[27] <- "IKeywords"

# remove possible duplicatea
INTERPOLNew <- INTERPOLNewAKeywordListTemp %>%
  distinct()

write.csv(INTERPOLNew, file = "INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", row.names = F)

#########################################################
#####                 Country lists                 #####
#########################################################

#read file
#INTERPOLNew <- read.csv("INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", header = TRUE)

#####         Distinct Countries per record         ##### 
# get city/country data
data(world.cities)

# replace "United States" with USA & "United Kingdom" with UK.
aff.lst <- gsub("United States", "USA", INTERPOLNew$Affiliations, perl = TRUE)
aff.lst <- gsub("United Kingdom", "UK", aff.lst, perl = TRUE)
# replace ';' with ',' as multiple affiliations are separated with ';'
# but that doesn't fit with the strsplit()
aff.lst <- gsub(";", ",", aff.lst)
# split fields by ", "
splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
# extract fields which match a known city making sure that diacritics aren't a problem...
city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])
# ... or country
#cntry.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])
# this version only returns unique instances of countries per publication
cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in% world.cities$country.etc)]))



#extract the list of country per paper and place in dataframe
cntry.lst.dat <- as.data.table(matrix(cntry.lst),stringsAsFactors=FALSE)

# bind to the original data
INTERPOLNew_temp <- cbind(INTERPOLNew,cntry.lst.dat)

# cntry.lst.dat.map <- Map(as.data.table,cntry.lst)
# cntry.lst.dat <- rbindlist(cntry.lst.dat.map)

#convert to character - may not be necessary
INTERPOLNew_temp$V1 <- as.character(INTERPOLNew_temp$V1)

# remove unwanted characters  
INTERPOLNew_temp$V1 <-  gsub("c\\(","",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub("\\)","",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub("\"","",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub(", ",",",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub("character\\(0",NA,INTERPOLNew_temp$V1)

# rename column to country
names(INTERPOLNew_temp)[28] <- "Countries"

INTERPOLNew <- INTERPOLNew_temp
# write to file and read again to remove unwanted list !!! Should look  for a better solution
write.csv(INTERPOLNew,file = "INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", row.names = F)

#read file
INTERPOLNew <- read.csv("INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", header = TRUE)

#####         Distinct Institutions per record         ##### 

# not all need rerunning as previously done for countries,  but left commented to show the steps
# get city/country data
# data(world.cities)
# 
# # replace "United States" with USA & "United Kingdom" with UK.
# aff.lst <- gsub("United States", "USA", INTERPOLNew$Affiliations, perl = TRUE)
# aff.lst <- gsub("United Kingdom", "UK", aff.lst, perl = TRUE)
# # replace ';' with ',' as multiple affiliations are separated with ';'
# # but that doesn't fit with the strsplit()
# aff.lst <- gsub(";", ",", aff.lst)
# # split fields by ", "
# splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
# # extract fields which match a known city making sure that diacritics aren't a problem...
# city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])
# ... or country
Insti.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])
# this version only returns unique instances of countries per publication
#cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in% world.cities$country.etc)]))

#extract the list of country per paper and place in dataframe
Insti.list.dat <- as.data.table(matrix(Insti.lst),stringsAsFactors=FALSE)

# bind to the original data
INTERPOLNew_temp <- cbind(INTERPOLNew,Insti.list.dat)

# Insti.list.dat.map <- Map(as.data.table,cntry.lst)
# Insti.list.dat <- rbindlist(Insti.list.dat.map)

#convert to character - may not be necessary
INTERPOLNew_temp$V1 <- as.character(INTERPOLNew_temp$V1)

# remove unwanted characters  
INTERPOLNew_temp$V1 <-  gsub("c\\(","",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub("\\)","",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub("\"","",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub(", ",",",INTERPOLNew_temp$V1)
INTERPOLNew_temp$V1 <-  gsub("character\\(0",NA,INTERPOLNew_temp$V1)

# rename column to country
names(INTERPOLNew_temp)[29] <- "Institutions"

INTERPOLNew <- INTERPOLNew_temp

# write to file and read again to remove unwanted list !!! Should look  for a better solution
write.csv(INTERPOLNew,file = "INTERPOL/PartiallyCorrected/NewTitleCorrected.csv", row.names = F)

# This should be the finally corrected faile fro all references. Further filtering can be applied to remove unwanted columns, or kept as such



