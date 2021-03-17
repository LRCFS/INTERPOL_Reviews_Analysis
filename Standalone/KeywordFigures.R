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
# This code is for Keywords figures
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list = ls())

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

#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")

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
# dir.create(file.path(Results.dir,Title.dir),recursive = TRUE)

# where the generated files and figures are saved 
fileDirAll <- "FullList/"
# dir.create(file.path(Results.dir,Keyword.dir,fileDirAll),recursive = TRUE)
fileDirTop <- "TopList/"
# dir.create(file.path(Results.dir,Keyword.dir,fileDirTop),recursive = TRUE)
Affiliation.dir <- "Affiliation/"
dir.create(file.path(Results.dir,Affiliation.dir),recursive = TRUE)
Citation.dir <- "CrossCitation/"
dir.create(file.path(Results.dir,Citation.dir),recursive = TRUE)

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

# Filenames for figures and tables
OutputName <- "INTERPOL_" # FigureName
OutputFigure <- "Figure_"
OutputTable <- "Table_"
Keyword <- "Keyword_"
OuputTitle <- "Title_"

# INTERPOL Jaccard export figure
JaccardExport <- "INTERPOL_Jaccard_"


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

# # set extension and Citation
# extension <- ".csv"
# 
# # where the generated figures are saved 
# Figure.dir <- "Figures/"
# Table.dir <- "Tables/"
# # the number of keywords (top most frequent) appearing in the figure
# number <- 100   # target number of keywords apearing in the keyword figure
# maximum <- 105  # maximum number of keywords appearing in the keyword figure  
# 
# # colour palette for the Keyword figure
# pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#FF00FF","#330033")

####### Select ######
####                    Data INTERPOL                  #####

# filenames <- list.files(cit.path.Drugs, pattern=extension, full.names=TRUE)


filenames <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)
# # filename for figure export
FigureName <- "Fig1_INTERPOL_Keyword_"
TableName <- "Table1_INTERPOL_Keyword_"

#######  or   #######
#####                    Data Scopus                    #####

# cit.path.Scopus <- "Scopus/"
# filenames <- list.files(cit.path.Scopus, pattern=extension, full.names=TRUE)
# # filename for figure export
# FigureName <- "Fig1_Scopus_Keyword_"


#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

for (file in filenames){
# remove the extension and path of the file in column reference  
  name <- gsub(extension, "", file)
  name <- gsub(".*_", "", name)
  Count <- number

Dataset <- read.csv(file, header = TRUE, encoding = "UTF-8")
# Dataset <- rbindlist(lapply(file,fread, encoding='UTF-8'))
# filename for figure export
var <- paste0(FigureName,name)
var2 <- paste0(TableName,name)

# rename some of the columns to remove special characters or encoding
names(Dataset)[1:2] <- c("Authors", "AuthorID")
names(Dataset)[5]<-c("Source.title")
names(Dataset)[17:18]<-c("Author.Keywords", "Index.Keywords")

#############################################################
#####                     Keywords                      #####
#############################################################

# This section is looks at Keywords

# Select column label $Year, $Title,  $Source.title, $Author.Keywords, $Index.Keyword
ReducedDataSet <- Dataset %>%
  select(Year,Title,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords)%>%
  distinct()

if (KeywordEntries == "Author.Keywords"){
  #   Author Keywords only
  names(ReducedDataSet) <- sub("Author.Keywords","AIKeywords", names(ReducedDataSet))
  Keyname <- "A_Keywords"
} else{
if (KeywordEntries == "Database.Keywords"){
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
KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
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
  select(Year,Title,Source.title,KeywordsCorrected) %>%
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

# DatasetKeywordNarrowRangeGraph <- subset(DatasetKeywordTotalCount,x>Count)
SubsetKeywordNarrowRangeGraph <-subset(DatasetKeywordYearCount,Keyword %in% DatasetKeywordNarrowRangeGraph$Keyword)

#############################################################
#####                      Graph Range                  #####
#############################################################

source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Scopus data)
  #Breaks and labels for Interpol
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(BreakRange,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=DatasetRange)

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Keyword,levels=rev(sort(unique(Keyword))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
                         labels=DatasetRange))  %>%

  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
GraphTemp2 <- aggregate(GraphTemp1[, 1], list(GraphTemp1$KeywordsCorrected), min)

GraphTemp1$graphorder <- as.numeric(gsr(GraphTemp1$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p <- ggplot(GraphTemp1,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1925,1935,1945,1955,1965,1975,1985,1995,2005,2015))+
  scale_fill_manual(values=c(pal),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=8)+
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

var1 <- paste0(OutputName,Keyname,name)
var2 <- paste0(OutputName,Keyname,name)

#save figure
ggsave(file=paste0(Results.dir,Keyword.dir,sprintf("%s.png",var1)), p, width = 6, height = 8, units = "in", dpi=150)

names(DatasetKeywordNarrowRangeGraph)[2]<-c(name)

#Export to top keywords list per evidence type 
write.csv(DatasetKeywordNarrowRangeGraph, file=paste0(Results.dir,Keyword.dir,sprintf("%s.csv",var2)), row.names = F)

#Export to all keywords list per evidence type 
# write.csv(DatasetKeywordTotalCount, file=paste0(Results.dir,Keyword.dir,sprintf("%s.csv",var2)), row.names = F)

}

#### To merge all the evidence lists


# KeywordFullTop <- list.files(paste0(Results.dir,Keyword.dir), pattern=".csv", full.names=TRUE)
# 
# # adapted from https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# # and comments
# 
# rm(datasetEvidence)
# 
# for (file in KeywordFullTop){
# 
#   # if the merged dataset doesn't exist, create it
#   if (!exists("datasetEvidence")){
#     datasetEvidence <- read.csv(file, header=TRUE)
#   }
# 
#   # if the merged dataset does exist, append to it
#   if (exists("datasetEvidence")){
#     temp_dataset <-read.csv(file, header=TRUE)
#     datasetEvidence<-full_join(datasetEvidence, temp_dataset)
#     rm(temp_dataset)
#   }
# 
# }





