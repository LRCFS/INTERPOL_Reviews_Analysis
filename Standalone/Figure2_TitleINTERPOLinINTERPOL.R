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
# This code is for Figure 2
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
# Figure.dir <- "Figures/"
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
#####                    Data loading                   #####
#############################################################

#####                    Data INTERPOL                  #####
# load INTERPOL data
filenames <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)
TableName <- "Table7_INTERPOL_"

dat_csv = ldply(filenames, read_csv)
dat_csv <- dat_csv %>%
  distinct()

dat_csvReduced <- dat_csv %>%
   select(SubEvidence,Title)%>%
  distinct()

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
TitleCorrected <- read.csv("CorrectionLists/TitleCorrections.csv", header=TRUE, encoding = 'UTF-8')

dat_csvReduced$TitleCorrected <- gsr(as.character(dat_csvReduced$Title),as.character(TitleCorrected$Original),as.character(TitleCorrected$Corrected))

dat_csvReduced$TitleCorrected <- na_if(dat_csvReduced$TitleCorrected, "NO TITLE AVAILABLE")

dat_csvReduced <- dat_csvReduced%>%
  select(SubEvidence,TitleCorrected)%>%
  distinct()

dat_csvReduced <- na.omit(dat_csvReduced)

#############################################################
#####This is the code for generating INTERPOL Cross Ref #####
#############################################################


for (file in filenames){
  # remove the extension and path of the file in column reference  
  name <- gsub(extension, "", file)
  name <- gsub(".*_", "", name)
  
  Dataset <- read.csv(file, header = TRUE, encoding = "UTF-8",na.strings = c("","NA"))

  # to remove possible duplicate present in the same report
  DatasetNarrow <- Dataset %>%
    select(SubEvidence,Title) %>%
    distinct()
  
  DatasetNarrow$Title <- toupper(DatasetNarrow$Title)
  
  # replace specific non-alphanumeric characters 
  DatasetNarrow$Title <- str_replace_all(DatasetNarrow$Title, "Ó", " ")
  DatasetNarrow$Title <- str_replace_all(DatasetNarrow$Title, "Ñ", " ")
  
  # replace all non-english characters 
  DatasetNarrow$Title <- removeDiacritics(DatasetNarrow$Title)
  
  # swap out all non-alphanumeric characters 
  DatasetNarrow$Title <- str_replace_all(DatasetNarrow$Title, "[^[:alnum:]]", " ")
  # remove the double space the previous line create
  DatasetNarrow$Title <- str_replace_all(DatasetNarrow$Title, "  ", " ")
  
  # remove leadind and trailing white space
  DatasetNarrow$Title <- trimws(DatasetNarrow$Title)
  
  DatasetNarrow$TitleCorrected <- gsr(as.character(DatasetNarrow$Title),as.character(TitleCorrected$Original),as.character(TitleCorrected$Corrected))
  
  DatasetNarrow$TitleCorrected <- na_if(DatasetNarrow$TitleCorrected, "NO TITLE AVAILABLE")
  
  DatasetNarrow <- DatasetNarrow %>%
    select(SubEvidence,TitleCorrected)%>%
    distinct()
  
  DatasetNarrow <- na.omit(DatasetNarrow)
  
  # Dataset <- rbindlist(lapply(file,fread, encoding='UTF-8'))
  # filename for figure export
  var3 <- paste0(OutputTable,OutputName,OuputTitle,name)
  
  ## Count Evidence in INTERPOL matches
  CountAcrossEvidence <-subset(dat_csvReduced,TitleCorrected %in% DatasetNarrow$TitleCorrected)
  CountTotalMatches <- CountAcrossEvidence %>%
    distinct()
  
  SummariseEvidence <- data.frame(table(CountTotalMatches$SubEvidence))
  DatasetNarrow$SubEvidence <- as.character(DatasetNarrow$SubEvidence)
  Colname <- DatasetNarrow[1,1]
  names(SummariseEvidence)[2] <-  Colname
 
  #Export to text file 
  write.csv(SummariseEvidence, file=paste0(Results.dir,Title.dir,sprintf("%s.csv",var3)), row.names = F)
}

#############################################################
#####   This is the code to merge the exported files    #####
#############################################################

# adapted from https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# and comments

INTERPOL_INTERPOL <- list.files(paste0(Results.dir,Title.dir), pattern=".csv", full.names=TRUE)

# remove dataframe is exists
rm(datasetINTERPOL)

for (file in INTERPOL_INTERPOL){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("datasetINTERPOL")){
    datasetINTERPOL <- read.csv(file, check.names=FALSE)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("datasetINTERPOL")){
    temp_dataset <-read.csv(file, check.names=FALSE)
    datasetINTERPOL<-full_join(datasetINTERPOL, temp_dataset)
    rm(temp_dataset)
  }
  
}

# convert NA to 0
datasetINTERPOL[is.na(datasetINTERPOL)] <- 0

# order column alphabetically
col_order1 <- colnames(datasetINTERPOL[1])
col_order2 <- sort(colnames(datasetINTERPOL[,-1]))
col_order <- c(col_order1,col_order2)
OverallTable<- datasetINTERPOL[,col_order]
OverallTable$Var1 <- as.character(OverallTable$Var1)

# order rows alphabetically
OverallTable <- OverallTable[order(OverallTable$Var1),]

# remove first row and assign to row name instead
result <- OverallTable[-1]
row.names(result) <- OverallTable$Var1

results <- as.data.frame(result)

var4 <- paste0(OutputFigure,OutputName,OuputTitle)

# define upper triangle and set it to NA 
# - this opposite to what we want, but I think 
#   that's due to ggplot's default ordering
results[upper.tri(results)] <- NA
# add row names as col1
res = data.frame(col1 = row.names(results), results)
# make long format for ggplot
res.long = res %>% 
  gather(col2, counts, Biological.Evidence.DNA:Toxicology.Surveillance)
# replace dots added to names in col2 by data.frame() above
res.long$col2 = gsub( '.', ' ', res.long$col2, fixed = TRUE)
# fix BE and PG as '-' were also replaced with '.'
res.long[res.long$col2 == 'Biological Evidence DNA','col2'] <- 'Biological Evidence-DNA'
res.long[res.long$col2 == 'Paint Glass','col2'] <- 'Paint-Glass'
res.long[res.long$col2 == 'Toxicology Challenge Advances','col2'] <- 'Toxicology Challenge-Advances'

# get vector of types for reordering the y-axis
types.vec = row.names(results)
p = ggplot(res.long, aes(col1, col2, fill = counts)) + 
  geom_tile() + 
  # the darkest blue colour is too dark to see the black text
  # so make high value labels grey to be able to read them
  geom_text(aes(label = counts), size = 2.9, fontface = 'bold', colour = ifelse(res.long$counts > 1900, 'grey', 'black')) + 
  scale_fill_gradient(low = 'white', high = 'darkblue', na.value = 'white') +
  # reorder y-axis to make the desired diagonal
  scale_y_discrete(limits = types.vec[order(types.vec, decreasing  = TRUE)]) +
  scale_x_discrete(position = 'top') +
  labs(x = '', y = '') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=60, hjust =0),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())

show(p)

ggsave(
  sprintf("%s.tiff",var4),
  plot = p,
  device = NULL,
  path = file.path(Results.dir),
  scale = 1,
  width = 7,
  height = 5.0,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE
)

print("Processing complete. Please check 'Results' folder for output")
