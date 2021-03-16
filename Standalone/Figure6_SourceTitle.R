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
# This code is for Figure 6
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
library(reshape2)
library(plyr)
library(readr)
library(ggthemes)

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
#####                 Folders and files                 #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "Standalone/INTERPOL/"
cit.path.Scopus <- "Standalone/Scopus/"

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
# dir.create(file.path(Results.dir,Keyword.dir,fileDirAll),recursive = TRUE)
fileDirTop <- "TopList/"
# dir.create(file.path(Results.dir,Keyword.dir,fileDirTop),recursive = TRUE)

# Filenames for figures and tables
OutputName <- "INTERPOL_" # FigureName
OutputFigure <- "Figure_"
OutputTable <- "Table_"
Keyword.fil <- "Keyword_"
OuputTitle <- "Title_"

SourceTitle <-"SourceTitle_"
Scopus <- "Scopus"

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
InterpolFiles <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)

dat_csv_Interpol = ldply(InterpolFiles, read_csv)

Dataset <- dat_csv_Interpol

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
#####                  Data cleansing                   #####
#############################################################

#read the corrected list for Title
TitleCorrected <- read.csv("Standalone/CorrectionLists/TitleCorrections.csv", header=TRUE, encoding = 'UTF-8')

Dataset$TitleCorrected <- gsr(as.character(Dataset$Title),as.character(TitleCorrected$Original),as.character(TitleCorrected$Corrected))

Dataset$TitleCorrected <- na_if(Dataset$TitleCorrected, "NO TITLE AVAILABLE")

# the number of record should identical to the total 
dat_csv_Interpol2 <- subset(Dataset, select=-c(Report)) %>%
  distinct()

# dat_csv_Interpol <- dat_csv_Interpol%>%
#   select()
#   distinct()
#
# select(TitleCorrected,'Source title',Year,`Access Type`,EID) %>%
#   distinct()
# 
# OA <- sum(!is.na(dat_csv_Interpol_open$`Access Type`))

dat_csv_InterpolReduced <- dat_csv_Interpol2 %>%
  # select(`Source title`,Evidence) %>%
  select(`Source title`) %>%
  mutate_if(is.character, str_trim)

dat_csv_InterpolReduced$`Source title` <- toupper(dat_csv_InterpolReduced$`Source title`)

#read the abbreviated list of title and combine it to the original list
JournalCorrected <- read.csv("Standalone/CorrectionLists/TitleAbbreviation.csv", header=TRUE)

dat_csv_InterpolReduced$AbbreviatedTitle <- gsr(as.character(dat_csv_InterpolReduced$`Source title`),as.character(JournalCorrected$JounalTitle),as.character(JournalCorrected$JounalAbbreviation))

dat_csv_InterpolReduced <-dat_csv_InterpolReduced %>%
  # select(AbbreviatedTitle, Evidence)
  select(AbbreviatedTitle)

dat_csv_InterpolReducedTemp <- aggregate(dat_csv_InterpolReduced$AbbreviatedTitle, by=list(AbbreviatedTitle=dat_csv_InterpolReduced$AbbreviatedTitle), FUN=length)


# dat_csv_InterpolReducedTemp <- aggregate(dat_csv_InterpolReduced$AbbreviatedTitle, by=list(AbbreviatedTitle=dat_csv_InterpolReduced$AbbreviatedTitle, Evidence=dat_csv_InterpolReduced$Evidence), FUN=length)

# TableTitleList <- dat_csv_InterpolReducedTemp %>%
#    pivot_wider(names_from = Evidence, values_from = x)
# 
# TableTitleList$sum <- rowSums(TableTitleList[,-1], na.rm = TRUE)

# Select top n
dat_csv_InterpolNarrow <- top_n(dat_csv_InterpolReducedTemp, 20)


#####                    Data Scopus                  #####
# load Scopus data
ScopusFiles <- list.files(cit.path.Scopus, pattern=extension, full.names=TRUE)

dat_csv_Scopus = ldply(ScopusFiles, read_csv)

dat_csv_Scopus <- dat_csv_Scopus %>%
  distinct()

dat_csv_ScopusReduced <- dat_csv_Scopus %>%
  select(`Source title`) %>%
  mutate_if(is.character, str_trim)

dat_csv_ScopusReduced$`Source title` <- toupper(dat_csv_ScopusReduced$`Source title`)

#read the abbreviated list of title and combine it to the original list
dat_csv_ScopusReduced$AbbreviatedTitle <- gsr(as.character(dat_csv_ScopusReduced$`Source title`),as.character(JournalCorrected$JounalTitle),as.character(JournalCorrected$JounalAbbreviation))

dat_csv_ScopusReduced <-dat_csv_ScopusReduced %>%
  select(AbbreviatedTitle)

dat_csv_ScopusReducedTemp <- aggregate(dat_csv_ScopusReduced$AbbreviatedTitle, by=list(AbbreviatedTitle=dat_csv_ScopusReduced$AbbreviatedTitle), FUN=length)

dat_csv_ScopusNarrow <- top_n(dat_csv_ScopusReducedTemp, 20)

# read the abbreviated list of title and combine it to the original list
# dat_csv_ScopusNarrow$SourceTitle <- gsr(as.character(dat_csv_ScopusNarrow$SourceTitle),as.character(JournalCorrected$JounalTitle),as.character(JournalCorrected$JounalAbbreviation))

##### Combined the two lists to form a full list

# combined the two lists of journals to generate overall list
JournalListTemp <- rbind(dat_csv_InterpolNarrow,dat_csv_ScopusNarrow)
JournalListTemp <- JournalListTemp %>%
  select(AbbreviatedTitle) %>%
  distinct()

# extract the number of records per journal, top n of each lists and also if present in the other list
# i.e. include journal counts even if not included in top 20 for that list but present in the other
# For Interpol
JournalListInterpolTemp <-subset(dat_csv_InterpolReducedTemp,AbbreviatedTitle %in% JournalListTemp$AbbreviatedTitle)
names(JournalListInterpolTemp)[2] <- c("number")
JournalListInterpolTemp$List <- c("INTERPOL")

# for Figure
JournalListInterpolTemp$number <- JournalListInterpolTemp$number * -1

# For Scopus
JournalListScopusTemp <-subset(dat_csv_ScopusReducedTemp,AbbreviatedTitle %in% JournalListTemp$AbbreviatedTitle)
names(JournalListScopusTemp)[2] <- c("number")
JournalListScopusTemp$List <- c("Scopus")

# combined the two lists of journals
JournalList <- rbind(JournalListInterpolTemp,JournalListScopusTemp)

#####
##### to order journals from the most frequently listed to the least accross both lists, keeping the largest numbers of both
# Interpol
dat_csv_InterpolNarrowTemp <- JournalListInterpolTemp %>%
  select(AbbreviatedTitle,number)
dat_csv_InterpolNarrowTemp$number <- abs(dat_csv_InterpolNarrowTemp$number)
names(dat_csv_InterpolNarrowTemp)[2] <- c("numberInterpol")

# Scopus
dat_csv_ScopusNarrowTemp <- JournalListScopusTemp %>%
  select(AbbreviatedTitle,number)
dat_csv_ScopusNarrowTemp$number <- as.numeric(dat_csv_ScopusNarrowTemp$number)
names(dat_csv_ScopusNarrowTemp)[2] <- c("numberScopus")

# combine the two lists
OrderTitleTemp <- full_join(dat_csv_InterpolNarrowTemp,dat_csv_ScopusNarrowTemp)
# determine the maximum betwen the two lists
# OrderTitle <- dplyr::mutate(OrderTitleTemp, MaxCount = pmax(numberInterpol, numberScopus, na.rm=TRUE))

# determine the maximum using INTERPOL List
OrderTitle <- dplyr::mutate(OrderTitleTemp, MaxCount = pmax(numberInterpol, na.rm=TRUE))


# to reattach to the order the Journal list
JournalList$order <- gsr(as.character(JournalList$AbbreviatedTitle),as.character(OrderTitle$AbbreviatedTitle),as.character(OrderTitle$MaxCount))
JournalList$order <- as.numeric(JournalList$order)

# X Axis Breaks and Labels 
brks <- seq(-3000, 3000, 500)
lbls = paste0(as.character(c(seq(3000, 0, -500), seq(500, 3000, 500))), "")

# Plot
p <- ggplot(JournalList, aes(x = reorder(AbbreviatedTitle,desc(order)), y = number, fill = List)) +   # Fill column
  geom_bar(stat = "identity", width = .6, show.legend = FALSE) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  # theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank()) +  # Centre plot title
  scale_fill_brewer(palette = "Dark2") + # Color palette
   ylab("Number of documents")

var1 <- paste0(SourceTitle,OutputName,Scopus)

#save figure
ggsave(file=paste0(Results.dir,sprintf("%s.png",var1)), p, width = 6.5, height = 8, units = "in", dpi=150)



show(p)
