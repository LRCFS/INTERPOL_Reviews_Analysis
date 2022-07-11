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
# This code is for Figure 4
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
library(tidyverse)
library(reshape2)
library(plyr)
library(readr)

#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")

#############################################################
#####                      Function                     #####
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
#####                 Folders and files                 #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "INTERPOL/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
dir.create(file.path(Results.dir),recursive = TRUE)
Figure.dir <- "Figures/"
# dir.create(file.path(Figure.dir),recursive = TRUE)
Keyword.dir <- "INTERPOL-Keyword/"
# dir.create(file.path(Figure.dir,Keyword.dir),recursive = TRUE)

Table.dir <- "Tables/"
# dir.create(file.path(Table.dir))
# dir.create(file.path(Table.dir,Keyword.dir),recursive = TRUE)
Title.dir <- "INTERPOL-INTERPOL-Title/"
# dir.create(file.path(Table.dir,Title.dir),recursive = TRUE)

# where the generated files and figures are saved 
fileDirAll <- "FullList/"
dir.create(file.path(Results.dir,Keyword.dir,fileDirAll),recursive = TRUE)
fileDirTop <- "TopList/"
dir.create(file.path(Results.dir,Keyword.dir,fileDirTop),recursive = TRUE)

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
##### Keyword Figures
# the number of keywords (top most frequent) appearing in the figure
number <- 100   # target number of keywords apearing in the keyword figure
maximum <- 105  # maximum number of keywords appearing in the keyword figure

# colour palette for the Keyword figure
pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#FF00FF","#330033")

#     Select one of the following three options
KeywordEntries <- "Author.Keywords"
# KeywordEntries <- "Database.Keywords"
# KeywordEntries <- "All.Keywords"

# # # filename for figure export
# FigureName <- "Fig_INTERPOL_Keyword_"
# TableName <- "Table1_INTERPOL_Keyword_"


#############################################################
#####                    Data loading                   #####
#############################################################

# # the number of keywords (top most frequent) appearing in the figure
# number <- 100   # target number of keywords apearing in the keyword figure
# maximum <- 105  # maximum number of keywords appearing in the keyword figure  
# 
# # colour palette for the Keyword figure
# pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#FF00FF","#330033")
# 
# ####### Select ######
# ####                    Data INTERPOL                  #####
# 
cit.path.INTERPOL <- "INTERPOL/"
filenames <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)
# # # filename for figure export
# TableName <- "Table_Keyword_INTERPOL_"
# CitationSource <- "_INTERPOL"


#######  or   #######
#####                    Data Scopus                    #####

# cit.path.Scopus <- "Scopus/"
# filenames <- list.files(cit.path.Scopus, pattern=extension, full.names=TRUE)
# # filename for figure export
# TableName <- "Table_Keyword_Scopus_"
# CitationSource <- "_Scopus"

#############################################################
#####     Select one of the following three options     #####
#############################################################
# this is select the keyword list(s) to analyse
KeywordEntries <- "Author"   #Keywords
# KeywordEntries <- "Database" #Keywords
# KeywordEntries <- "All"        #Keywords

#############################################################
#####     Select one of the following two options     #####
#############################################################
# this is select the groups or subgroup of evidence type. If none are selected, it will be combined (grouped) "Evidence" only.
# EvidenceEntries <- "SubEvidence"   #individual and splitted evidence type if applied; based on colum "SubEvidence"
EvidenceEntries <- "Evidence" #individual and grouped evidence type; based on colum "Evidence"

#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

# # for (file in filenames){
#   # remove the extension and path of the file in column reference  
# name <- gsub(extension, "", file)
# name <- gsub(".*_", "", name)

  
# Dataset <- read.csv(file, header = TRUE, encoding = "UTF-8")
Dataset <- rbindlist(lapply(filenames,fread, encoding='UTF-8'))

# rename some of the columns to remove special characters or encoding
names(Dataset)[1:2] <- c("Authors", "AuthorID")
names(Dataset)[5]<-c("Source.title")
names(Dataset)[17:18]<-c("Author.Keywords", "Index.Keywords")


Dataset$Title <- toupper(Dataset$Title)

# replace specific non-alphanumeric characters 
Dataset$Title <- str_replace_all(Dataset$Title, "Ó", " ")
Dataset$Title <- str_replace_all(Dataset$Title, "Ñ", " ")

# replace all non-english characters 
Dataset$Title <- removeDiacritics(Dataset$Title)

# swap out all non-alphanumeric characters 
Dataset$Title <- str_replace_all(Dataset$Title, "[^[:alnum:]]", " ")
# remove the double space the previous line create
Dataset$Title <- str_replace_all(Dataset$Title, "  ", " ")

# remove leadind and trailing white space
Dataset$Title <- trimws(Dataset$Title)


#############################################################
#####                  Title cleansing                  #####
#############################################################

#read the corrected list for Title
TitleCorrected <- read.csv("CorrectionLists/TitleCorrections.csv", header=TRUE, encoding = 'UTF-8')

Dataset$TitleCorrected <- gsr(as.character(Dataset$Title),as.character(TitleCorrected$Original),as.character(TitleCorrected$Corrected))

Dataset$TitleCorrected <- na_if(Dataset$TitleCorrected, "NO TITLE AVAILABLE")

# remove duplicates 

Dataset <- Dataset %>%
  distinct()

  if (EvidenceEntries == "SubEvidence"){
    #   Index Keywords only
    DatasetNarrow <- Dataset %>%
      select(Year,TitleCorrected,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords,SubEvidence)%>%
      distinct()
    names(DatasetNarrow)[8]<-c("EvidenceType")
  }    else {
    DatasetNarrow <- Dataset %>%
      select(Year,TitleCorrected,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords,Evidence)%>%
      distinct()
    names(DatasetNarrow)[8]<-c("EvidenceType")
  }
# note: when using the sub-evidence, the number of obs in "DatasetNarrow" is likely to be less than the one in "Dataset".  

  #############################################################
  #####                     Keywords                      #####
  #############################################################
  
  # This section is looks at Keywords
  
  ReducedDataSetList <- split(DatasetNarrow, interaction(DatasetNarrow$EvidenceType), drop = TRUE)

  for (i in 1:length(ReducedDataSetList)){
    ReducedDataSet <- ReducedDataSetList[[i]]
    name <- names(ReducedDataSetList)[i]
    Count <- number  
    
  if (KeywordEntries == "Author"){
    #   Author Keywords only
    names(ReducedDataSet) <- sub("Author.Keywords","AIKeywords", names(ReducedDataSet))
    Keyname <- "A_Keywords"
  } else {
  if (KeywordEntries == "Database"){
    #   Index Keywords only
    names(ReducedDataSet) <- sub("Index.Keywords","AIKeywords", names(ReducedDataSet))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    ReducedDataSet <- ReducedDataSet %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}
  
  #############################################################
  
  #Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
  DatasetKeywordList <- ReducedDataSet %>% 
    mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
    unnest(AIKeywords) %>%
    mutate_if(is.character, str_trim)
  
  # Upper case "AIKeywords" in "DatasetKeywordList" and save in dataframe
  # Extract list of "AIkeywords" and remove duplicate
  DatasetKeywordList$AIKeywords <- toupper(DatasetKeywordList$AIKeywords)
  KeywordList <- DatasetKeywordList %>%
    select(AIKeywords)
  Keyword <- KeywordList %>%
    distinct()
  
  #############################################################
  #####                  Data cleansing                   #####
  #############################################################
  
  #Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
  #read the corrected list of keywords and combine it to the original list
  KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrectionFull.csv", header=TRUE)
  # KeywordsCorrected <- as.data.frame(KeywordsCorrected)
  # DatasetKeywordList$KeywordsCorrected <- gsr(as.character(DatasetKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))
  DatasetKeywordList$KeywordsCorrected <- gsr(as.character(DatasetKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorAIKeywordsAcronym))
  
  #############################################################
  #####               Data analysis - Keywords            #####
  #############################################################
  
  #Count to number of time the same year is repeated in the "DatasetKeywordList$Year" and save in a data.frame "Year" 
  PublicationYear<- data.frame(table(ReducedDataSet$Year));PublicationYear
  names(PublicationYear) <- c("Year","Publications")
  PublicationYear$Year <- as.numeric(as.character(PublicationYear$Year))
  
  #count the number of keywords per title paper 
  DatasetKeywordListTemp1 <- DatasetKeywordList  %>%
    select(Year,TitleCorrected,Source.title,KeywordsCorrected) %>%
    distinct()
  DatasetKeywordListTemp2 <-DatasetKeywordListTemp1[complete.cases(DatasetKeywordListTemp1), ]
  sum(is.na(DatasetKeywordListTemp2$KeywordsCorrected))
  
  DatasetKeywordYearCount <- aggregate(DatasetKeywordListTemp2$Year, by=list(Year=DatasetKeywordListTemp2$Year, Keyword=DatasetKeywordListTemp2$KeywordsCorrected), FUN=length)
  DatasetKeywordTotalCount <- aggregate(DatasetKeywordListTemp2$Year, by=list(Keyword=DatasetKeywordListTemp2$KeywordsCorrected), FUN=length)
  
    # narrowing range for plot
  DatasetKeywordNarrowRangeGraph <- top_n(DatasetKeywordTotalCount, Count)
  
  # count the number of rows, hense the number of keywords in figure
  a <- nrow(DatasetKeywordNarrowRangeGraph)
  
  while (a>maximum) {
    Count <- Count-1
    DatasetKeywordNarrowRangeGraph <- top_n(DatasetKeywordTotalCount, Count)
    a <- nrow(DatasetKeywordNarrowRangeGraph)
  }
  names(DatasetKeywordNarrowRangeGraph)[2] <- c(name)

  var1 <- paste0(OutputTable,OutputName,All,Keyname,name)
  var2 <- paste0(OutputTable,OutputName,Top,Keyname,name)
  
 #   var <- paste0(TableName,name)
  #Export to text file 
  write.table(DatasetKeywordNarrowRangeGraph, file=paste0(Results.dir,Keyword.dir,fileDirTop,sprintf("%s.txt",var2)), sep = "\t", row.names = F)

    # for the full list of keywords
  #VarFull <- paste0(TableName,"Total_",name)
  #Export to text file
  names(DatasetKeywordTotalCount)[2] <- c(name)
  write.table(DatasetKeywordTotalCount, file=paste0(Results.dir,Keyword.dir,fileDirAll,sprintf("%s.txt",var1)), sep = "\t", row.names = F)

  
  }


#############################################################
#####   This is the code to merge the exported files    #####
#####         For the top list of keywords             #####
#############################################################
KeywordFullTop <- list.files(paste0(Results.dir,Keyword.dir,fileDirTop), pattern=".txt", full.names=TRUE)

# adapted from https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# and comments

rm(datasetEvidence)

for (file in KeywordFullTop){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("datasetEvidence")){
    datasetEvidence <- read.table(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("datasetEvidence")){
    temp_dataset <-read.table(file, header=TRUE, sep="\t")
    datasetEvidence<-full_join(datasetEvidence, temp_dataset)
    rm(temp_dataset)
  }
  
}

# Sum the rows (except first column) to give frequency count
datasetEvidence$sum <- rowSums(!is.na(datasetEvidence[,-1]))

# narrowing range for plot # will select by "sum"
datasetEvidenceNarrowRangeGraph <- top_n(datasetEvidence, Count)

# count the number of rows, hense the number of keywords in figure
a <- nrow(datasetEvidenceNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  datasetEvidenceNarrowRangeGraph <- top_n(datasetEvidence, Count)
  a <- nrow(datasetEvidenceNarrowRangeGraph)
}
# exclude last column
n <- as.numeric(ncol(datasetEvidenceNarrowRangeGraph))
# replace numbers to 1
datasetEvidenceNarrowRangeGraph[,2:(n-1)][!is.na(datasetEvidenceNarrowRangeGraph[,2:(n-1)])] <- 1
# and NA to 0
datasetEvidenceNarrowRangeGraph[,2:(n-1)][is.na(datasetEvidenceNarrowRangeGraph[,2:(n-1)])] <- 0

datasetEvidenceNarrowRangeGraphTemp <- datasetEvidenceNarrowRangeGraph[,-n]

test2 <- as.data.frame(colSums(Filter(is.numeric, datasetEvidenceNarrowRangeGraphTemp)))
test2$EvidenceType <- rownames(test2)

nba.m <- melt(datasetEvidenceNarrowRangeGraphTemp)

# nba.m <- ddply(nba.m, .(variable), transform)
nba.m$grapheorder <-  gsr(as.character(nba.m$Keyword),as.character(datasetEvidenceNarrowRangeGraph$Keyword),datasetEvidenceNarrowRangeGraph$sum)

nba.m$grapheorder2 <-  gsr(as.character(nba.m$variable),as.character(test2$EvidenceType),test2$`colSums(Filter(is.numeric, datasetEvidenceNarrowRangeGraphTemp))`)
# 
nba.m$grapheorder <- as.numeric(nba.m$grapheorder)
nba.m$grapheorder2 <- as.numeric(nba.m$grapheorder2)
#p <- ggplot(nba.m,aes(x=variable,y=Keyword,fill=value))+

nba.m$variable <-gsub("\\.", "  \n ",nba.m$variable)

# assign text colour
textcol <- "black"


p <- ggplot(nba.m,aes(x=reorder(variable,desc(grapheorder2)),y=reorder(Keyword, grapheorder),fill=value))+
  geom_tile(colour="white",size=0.2)+
  scale_fill_gradientn(colours = c("light grey", "blue"), values = c(0,1))+
  # guides(fill=guide_legend(title="Count"))+
  labs(x="",y="",title="")+
  theme_grey(base_size=5)+
  theme(text = element_text(family = "Arial"),
        legend.position="none",legend.direction="vertical",
        legend.title=element_blank(),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=6),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(vjust=0.5,angle= 90,size=5,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=10))

#save figure
ggsave(file=paste0(Results.dir,sprintf("%s.tiff",paste0(Top,Keyname,EvidenceEntries))), p, width = 4.87, height = 6, units = "in", dpi=300)


show(p)

keywordToplist <- datasetEvidence %>%
  select(Keyword)
keywordToplist$colourcode <-1

#############################################################
#####   This is the code to merge the exported files    #####
#####         For the full list of keywords             #####
#############################################################
KeywordFullList <- list.files(paste0(Results.dir,Keyword.dir,fileDirAll), pattern=".txt", full.names=TRUE)

# adapted from https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# and comments
rm(datasetEvidence)
for (file in KeywordFullList){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("datasetEvidence")){
    datasetEvidence <- read.table(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("datasetEvidence")){
    temp_dataset <-read.table(file, header=TRUE, sep="\t")
    datasetEvidence<-full_join(datasetEvidence, temp_dataset)
    rm(temp_dataset)
  }
  
}

# Sum the rows (except first column) to give frequency count
datasetEvidence$sum <- rowSums(!is.na(datasetEvidence[,-1]))

# narrowing range for plot # will select by "sum"
datasetEvidenceNarrowRangeGraph <- top_n(datasetEvidence, Count)

# count the number of rows, hense the number of keywords in figure
a <- nrow(datasetEvidenceNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  datasetEvidenceNarrowRangeGraph <- top_n(datasetEvidence, Count)
  a <- nrow(datasetEvidenceNarrowRangeGraph)
}
# exclude last column
n <- as.numeric(ncol(datasetEvidenceNarrowRangeGraph))
# replace numbers to 1
datasetEvidenceNarrowRangeGraph[,2:(n-1)][!is.na(datasetEvidenceNarrowRangeGraph[,2:(n-1)])] <- 1
# and NA to 0
datasetEvidenceNarrowRangeGraph[,2:(n-1)][is.na(datasetEvidenceNarrowRangeGraph[,2:(n-1)])] <- 0

datasetEvidenceNarrowRangeGraphTemp <- datasetEvidenceNarrowRangeGraph[,-n]

test2 <- as.data.frame(colSums(Filter(is.numeric, datasetEvidenceNarrowRangeGraphTemp)))
test2$EvidenceType <- rownames(test2)

nba.m <- melt(datasetEvidenceNarrowRangeGraphTemp)

# nba.m <- ddply(nba.m, .(variable), transform)
nba.m$grapheorder <-  gsr(as.character(nba.m$Keyword),as.character(datasetEvidenceNarrowRangeGraph$Keyword),datasetEvidenceNarrowRangeGraph$sum)

nba.m$grapheorder2 <-  gsr(as.character(nba.m$variable),as.character(test2$EvidenceType),test2$`colSums(Filter(is.numeric, datasetEvidenceNarrowRangeGraphTemp))`)


# look for top keywords in the list considering all the keyword; this is to colour coded the figure.
nba.m$colourcodetemp <- gsr(as.character(nba.m$Keyword),as.character(keywordToplist$Keyword),as.character(keywordToplist$colourcode))

# assigned a colour code, 1 is present in both list, 0 is not
nba.m$colourcode <- ifelse(nba.m$colourcode=="1","black","red")

# remove the extra temp colour column
# nba.m <- nba.m[,-6]
nba.m$variable <-gsub("\\.", "  \n ",nba.m$variable)
nba.m$grapheorder <- as.numeric(nba.m$grapheorder)
nba.m$grapheorder2 <- as.numeric(nba.m$grapheorder2)
#p <- ggplot(nba.m,aes(x=variable,y=Keyword,fill=value))+
# temp <- nba.m[]

colors <- nba.m %>%
  arrange(desc(grapheorder2), desc(grapheorder),desc(Keyword))
colors2 <- nba.m %>%
  arrange(grapheorder2, grapheorder,Keyword)

TempColour <- colors2$colourcode 


# assign text colour
textcol <- "black"


p <- ggplot(nba.m,aes(x=reorder(variable,desc(grapheorder2)),y=reorder(Keyword, grapheorder),fill=value))+
  geom_tile(colour="white",size=0.2)+
  scale_fill_gradientn(colours = c("light grey", "blue"), values = c(0,1))+
  # guides(fill=guide_legend(title="Count"))+
  labs(x="",y="",title="")+
theme_grey(base_size=5)+
  theme(text = element_text(family = "Arial"),
        legend.position="none",legend.direction="vertical",
        legend.title=element_blank(),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=6),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(vjust=0.5,angle= 90,size=5,colour=textcol),
        axis.text.y=element_text(vjust=0.2,size=4,colour=TempColour),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=10))

#save figure
ggsave(file=paste0(Results.dir,sprintf("%s.tiff",paste0(All,Keyname,EvidenceEntries))), p, width = 4.87, height = 6, units = "in", dpi=300)


show(p)

print("Processing complete. Please check 'Results' folder for output")

