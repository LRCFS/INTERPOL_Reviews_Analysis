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
# This code is for Country and Institution figures
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list=ls())

#############################################################
#####                      Library                      #####
#############################################################

library(dplyr)
library(expss)
library(stringr)
library(tidyr)
library(ggplot2)
library(maps)
library(countrycode)
library(RColorBrewer)

#############################################################
#####                      Function                     #####
#############################################################

# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    #"SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}

#############################################################
#####                 Folders and files                 #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "Standalone/INTERPOL/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Standalone/Results/"
dir.create(file.path(Results.dir),recursive = TRUE)
Figure.dir <- "Figures/"
# dir.create(file.path(Figure.dir),recursive = TRUE)
Keyword.dir <- "INTERPOL-Keyword/"
# dir.create(file.path(Figure.dir,Keyword.dir),recursive = TRUE)

Table.dir <- "Tables/"
# dir.create(file.path(Table.dir))
# dir.create(file.path(Table.dir,Keyword.dir),recursive = TRUE)
Title.dir <- "INTERPOL-INTERPOL-Title/"
# dir.create(file.path(Results.dir,Title.dir),recursive = TRUE)

# where the generated files and figures are saved 
fileDirAll <- "FullList/"
dir.create(file.path(Results.dir,Keyword.dir,fileDirAll),recursive = TRUE)
fileDirTop <- "TopList/"
dir.create(file.path(Results.dir,Keyword.dir,fileDirTop),recursive = TRUE)
Affiliation.dir <- "Affiliation/"
dir.create(file.path(Results.dir,Affiliation.dir),recursive = TRUE)


# Filenames for figures and tables
OutputName <- "INTERPOL_" # FigureName
OutputFigure <- "Figure_"
OutputTable <- "Table_"
Keyword.fil <- "Keyword_"
OuputTitle <- "Title_"

# INTERPOL top citation list
InterpolTopCitation <- "INTERPOL_Top_Citation"

# INTERPOL Jaccard export figure
JaccardExport <- "INTERPOL_Jaccard_"

# and filenames for figures
All <- "All_"
Top <- "Top_"


#############################################################
##### Country and Institution figures
# Number of individual country appearing on the figures
NumberCountry <- 20


#############################################################
#####                    Data loading                   #####
#############################################################


#####                    Data INTERPOL                  #####
# load INTERPOL data
filenames <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)
FigureName <- "Fig2_INTERPOL_"

#####                    Data Scopus                    #####
# # or load Scopus data
# filenames <- list.files("Scopus/", pattern="*.csv", full.names=TRUE)
# Dataset <- rbindlist(lapply(filenames,fread, encoding='UTF-8'))
# # filename for figure export
# FigureName <- "Fig2_Scopus_"


#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

for (file in filenames){
  # remove the extension and path of the file in column reference  
  name <- gsub(extension, "", file)
  name <- gsub(".*_", "", name)

  Dataset <- read.csv(file, header = TRUE, encoding = "UTF-8")

  # rename some of the columns to remove special characters or encoding
  names(Dataset)[1:2] <- c("Authors", "AuthorID")
  names(Dataset)[5]<-c("Source.title")
  names(Dataset)[17:18]<-c("Author.Keywords", "Index.Keywords")

  
  Dataset <- Dataset %>%
    select(Year,Title,Source.title,Authors,AuthorID,Affiliations) %>%
    distinct()
  
#############################################################
#####                    Countries                      #####
#############################################################

# get city/country data
  # get city/country data
  data(world.cities)
  
  # replace "United States" with USA & "United Kingdom" with UK.
  aff.lst <- gsub("United States", "USA", Dataset$Affiliations, perl = TRUE)
  aff.lst <- gsub("United Kingdom", "UK", aff.lst, perl = TRUE)
  # replace ';' with ',' as multiple affiliations are separated with ';'
  # but that doesn't fit with the strsplit()
  aff.lst <- gsub(";", ",", aff.lst)
  # split fields by ", "
  splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
  # extract fields which match a known city making sure that diacritics aren't a problem...
  city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])
  
  # this version returns all countries per publication
  cntry.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])
  
  cntry.dat <- data.frame(Country = removeDiacritics(unlist(cntry.lst)), stringsAsFactors = FALSE)
  
  # define continent for each country
  cntry.dat$Continent <- countrycode(sourcevar = cntry.dat[, "Country"],
                                     origin = "country.name",
                                     destination = "continent")
  # get number of record per countries
  Total <- cntry.dat %>% 
    group_by(Country, Continent) %>% 
    dplyr::summarise(Count = n())
  
  threshold <-sort(Total$Count,T)[NumberCountry]
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
   
   cntry.dat <- cntry.dat[order(cntry.dat$Count, decreasing = FALSE), ]
   
  # add in 'Others'
  cntry.dat <- rbind(other.dat, data.frame(cntry.dat))
  
  cntry.dat$Country <- factor(cntry.dat$Country, levels = cntry.dat$Country)

  #cntry.dat = countryCounts(datainterpol$Institutions)
  #cntry.dat = cntry.dat %>% filter(!is.na(Country))
  
  # Plot figure country distribution
p <- ggplot(cntry.dat, aes(x=Country, y=Count, fill=Continent)) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
  xlab('Country Affiliation') +
  ylab('Total Papers') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Arial"))

# filename for figure export
var <- paste0(OutputName,"Institution_",name)

#save figure
ggsave(file=paste0(Results.dir,Affiliation.dir,sprintf("%s.png",var)), p, width = 6, height = 8, units = "in", dpi=150)



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

threshold <-sort(Total$Count,T)[NumberCountry]
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

cntry.dat <- cntry.dat[order(cntry.dat$Count, decreasing = FALSE), ]

# add in 'Others'
cntry.dat <- rbind(other.dat, data.frame(cntry.dat))

cntry.dat$Country <- factor(cntry.dat$Country, levels = cntry.dat$Country)

# cntry.dat$Country <- reorder(cntry.dat$Country, +cntry.dat$Count)
# # add in 'Others'
# cntry.dat <- rbind(other.dat, data.frame(cntry.dat))
# plot
p <- ggplot(cntry.dat, aes(x=Country, y=Count, fill=Continent)) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
  xlab('Country Affiliation') +
  ylab('Total Papers') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Arial"))

# filename for figure export
var <- paste0(OutputName,"Country_",name)

#save figure
ggsave(file=paste0(Results.dir,Affiliation.dir,sprintf("%s.png",var)), p, width = 6, height = 8, units = "in", dpi=150)

}

