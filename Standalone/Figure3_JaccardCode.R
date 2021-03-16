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
# This code is for Figure 3
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list=ls())

#############################################################
#####                      Library                      #####
#############################################################
library(plyr)
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
library(corrplot)
library(readr)
library(jaccard)

## possible error messages : "Error: package or namespace load failed for ‘jaccard’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
##                           there is no package called ‘qvalue’"
## qvalue is part of "BioManager"
## Can be installed follwing these steps in the Console 
# install.packages("BiocManager")
# BiocManager::install("qvalue")

# similar error for install on Ubuntu 18.04. run same lines as above

#############################################################
#####                      Function                     #####
#############################################################

source("Standalone/Functions/SearchAndReplace.R")

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
#####     Select one of the following two options     #####
#############################################################
# this is select the groups or subgroup of evidence type. If noe are selectes, it will be combined (grouped) "Evidence" only.
 EvidenceEntries <- "SubEvidence"   #individual and splitted evidence type if applied; based on colum "SubEvidence"
# EvidenceEntries <- "Evidence" #individual and grouped evidence type; based on colum "Evidence"

#############################################################
#####                 Folders and files                 #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "Standalone/INTERPOL/"
 
# where the generated figures are saved, create folder if not existing
Results.dir <- "Standalone/Results/"
dir.create(file.path(Results.dir))
Figure.dir <- "Figures/"
# dir.create(file.path(Figure.dir))
Keyword.dir <- "INTERPOL-Keyword/"
# dir.create(file.path(Figure.dir,Keyword.dir))
 
Table.dir <- "Tables/"
# dir.create(file.path(Table.dir))
# dir.create(file.path(Table.dir,Keyword.dir))
Title.dir <- "INTERPOL-INTERPOL-Title/"
# dir.create(file.path(Table.dir,Title.dir))
 
# Filenames for figures and tables
OutputName <- "INTERPOL_" # FigureName
OutputFigure <- "Figure_"
OutputTable <- "Table_"
Keyword <- "Keyword_"
OuputTitle <- "Title_"
 
# INTERPOL top ciation list
InterpolTopCitation <- "INTERPOL_Top_Citation"
 
# INTERPOL Jaccard export figure
JaccardExport <- "INTERPOL_Jaccard_"

#############################################################
#####                    Data loading                   #####
#############################################################

#####                    Data INTERPOL                  #####
# load INTERPOL data
filenames <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)

dat_csv = ldply(filenames, read_csv)

if (EvidenceEntries == "SubEvidence"){
  #   Index Keywords only
  dat_csvReduced <- dat_csv %>%
    distinct() %>%
    select(SubEvidence,Title,Year)
  names(dat_csvReduced)[1]<-c("EvidenceType")
}    else {
  dat_csvReduced <- dat_csv %>%
    distinct() %>%
    select(Evidence,Title,Year)
  names(dat_csvReduced)[1]<-c("EvidenceType")
}

# Convert to upper character titles
dat_csvReduced$Title <- toupper(dat_csvReduced$Title)

# replace specific non-alphanumeric characters
dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "Ó", " ")
dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "Ñ", " ")

# replace all non-english characters
dat_csvReduced$Title <- removeDiacritics(dat_csvReduced$Title)

# swap out all non-alphanumeric characters
dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "[^[:alnum:]]", " ")
# remove the double space the previous line create
dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "  ", " ")

# remove leadind and trailing white space
dat_csvReduced$Title <- trimws(dat_csvReduced$Title)

dat_csvReduced <- dat_csvReduced %>%
  distinct()

#############################################################
#####                  Data cleansing                   #####
#############################################################

#read the corrected list for Title
TitleCorrected <- read.csv("Standalone/CorrectionLists/TitleCorrections.csv", header=TRUE, encoding = 'UTF-8')

dat_csvReduced$TitleCorrected <- gsr(as.character(dat_csvReduced$Title),as.character(TitleCorrected$Original),as.character(TitleCorrected$Corrected))

# replace "NO TITLE AVAILABLE" with NA
dat_csvReduced$TitleCorrected <- na_if(dat_csvReduced$TitleCorrected, "NO TITLE AVAILABLE")

# remove entries with missing (i.e. NA) Titles 
dat_csvReducedCorrectedNarrow <- dat_csvReduced[complete.cases(dat_csvReduced[ ,4]),]

# replace missing years (i.e. NA) with "undefined"
dat_csvReducedCorrectedNarrow$Year[is.na(dat_csvReducedCorrectedNarrow$Year)] <- c("undefined")

# keep wanted columns
dat_csvFinal <- dat_csvReducedCorrectedNarrow %>%
  select(EvidenceType,Year,TitleCorrected) %>%
  # select(EvidenceType,TitleCorrected) %>%
  distinct()

dat_csvFinal <-as.data.frame(dat_csvFinal)

dat_csvFinal <- dat_csvFinal %>%
  dplyr::group_by(EvidenceType, TitleCorrected) %>%
  dplyr::summarise(Year=paste(Year, collapse = ";"))

# Pivot to group by Title
# Warning, some records may have more that one year attached to them, different version. To remove the warning, add values_fn = list(Year = length)
TableTitleList <- dat_csvFinal %>%
   # pivot_wider(names_from = EvidenceType, values_from = Year)
  pivot_wider(names_from = EvidenceType, values_from = Year, values_fn = list(Year = length))
  # pivot_wider(names_from = EvidenceType, values_from = TitleCorrected, values_fn = list(TitleCorrected = length))
# Sum the rows (except first column) to give frequency count

TableTitleList$sum <- rowSums(!is.na(TableTitleList[,-1]))

# select top frequency count
topList <- TableTitleList[(TableTitleList$sum>1),]

# narrowing range for plot
TableTitleListReduced <- top_n(TableTitleList, 10)

#Export to text file 
write.table(TableTitleListReduced, file=paste0(Results.dir,sprintf("%s.txt",InterpolTopCitation)), sep = "\t", row.names = F)

#############################################################
# Correlation matrix
#############################################################

# pivot data to wider format
Correlation <- dat_csvFinal %>%
  pivot_wider(names_from = EvidenceType, values_from = Year, values_fn = list(Year = length))

df = Correlation[,-1]
df[!(is.na(df))] <- 1 # necessary when different dates are given for a document (Title) meaning output greater than 1
                      # if left to orginal number (not 1), then  value <- jaccard(df[,n], df[,n]) # not equal to 1
df[is.na(df)] <- 0

##############################################
######  Jaccard coefficient calculation ######
##############################################

# For all the evidence against each other, including the p values

for (i in 1:ncol(df)) {
  for (j in 1:ncol(df)) {
    # if the merged dataset doesn't exist, create it
    if (!exists("datasetEvidence")){
      datasetEvidence_i <- colnames(df)[i]
      datasetEvidence_j <- colnames(df)[j]
      datasetEvidence_Jaccard <-jaccard(df[,i], df[,j])
      # jt = jaccard.test(df[,i], df[,j], method = 'bootstrap')
      datasetEvidence <- data.frame(datasetEvidence_i,datasetEvidence_j,datasetEvidence_Jaccard)
    }
    
    # if the merged dataset does exist, append to it
    if (exists("datasetEvidence")){
      datasetEvidence_i <- colnames(df)[i]
      datasetEvidence_j <- colnames(df)[j]
      datasetEvidence_Jaccard <-jaccard(df[,i], df[,j])
      # jt = jaccard.test(df[,i], df[,j], method = 'bootstrap')
      temp_dataset <- data.frame(datasetEvidence_i,datasetEvidence_j,datasetEvidence_Jaccard)
      datasetEvidence<-full_join(datasetEvidence, temp_dataset)
      rm(temp_dataset)
    
    }
  
}}

m <- dcast(datasetEvidence, datasetEvidence_i ~ datasetEvidence_j, value.var = "datasetEvidence_Jaccard")

result <- m[-1]
row.names(result) <- m$datasetEvidence_i

result <- as.matrix(result)

# for figure export
CorVar <- paste0(JaccardExport,EvidenceEntries)

# corrplot does not have an save funtionality as for example ggplot. Here are it is done instead:
png(file.path(Results.dir,paste0(sprintf("%s.png",CorVar))), res=300, width = 6000, height = 4800, pointsize=27,
    units = "px", type = "cairo")
par(mar=c(1,1,1,520)+0.1)
corrplot(result, method="color",
         #change font size of names
         tl.cex = 0.9,
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         number.digits = 3,
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         #Change font size of coefficient 
         number.cex=0.75,
         cl.lim = c(0,1)
)
dev.off()
