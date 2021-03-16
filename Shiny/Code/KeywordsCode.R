# column names corrections
# names(datainterpol)[1:2] <- c("Authors", "AuthorID")
# names(datainterpol)[5]<-c("Source.title")
# names(datainterpol)[17:18]<-c("Author.Keywords", "Index.Keywords")

ReducedDataSet <- datainterpol %>%
  select(Year,TitleCorrected,Source.title,Authors,AuthorID,AKeywords,IKeywords)%>%
  distinct()

if (keywordType == "Author Keywords"){
  #   Author Keywords only
  names(ReducedDataSet) <- sub("AKeywords","AIKeywords", names(ReducedDataSet))
  Keyname <- "A_Keywords"
} else{
  if (keywordType == "Database Keywords"){
    #   Index Keywords only
    names(ReducedDataSet) <- sub("IKeywords","AIKeywords", names(ReducedDataSet))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    ReducedDataSet <- ReducedDataSet %>%
      unite("AIKeywords", AKeywords, IKeywords,sep = ";", remove = TRUE)
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
# DatasetKeywordList$AIKeywords <- toupper(DatasetKeywordList$AIKeywords)
KeywordList <- DatasetKeywordList %>%
  select(AIKeywords)
Keyword <- KeywordList %>%
  distinct()

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list
# KeywordsCorrected <- read.csv("Code/CorrectionLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
# KeywordsCorrected <- as.data.frame(KeywordsCorrected)
# DatasetKeywordList$KeywordsCorrected <- gsr(as.character(DatasetKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))
# DatasetKeywordList$KeywordsCorrected <- gsr(as.character(DatasetKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorAIKeywordsAcronym))
DatasetKeywordList$KeywordsCorrected <- DatasetKeywordList$AIKeywords
#############################################################
#####               Data analysis - Keywords            #####
#############################################################

#Count to number of time the same year is repeated in the "DatasetKeywordList$Year" and save in a data.frame "Year" 
# PublicationYear<- data.frame(table(ReducedDataSet$Year));PublicationYear
# names(PublicationYear) <- c("Year","Publications")
# 
# PublicationYear$Year <- as.numeric(as.character(PublicationYear$Year))

#count the number of keywords per title paper 
DatasetKeywordListTemp1 <- DatasetKeywordList  %>%
  select(Year,TitleCorrected,Source.title,KeywordsCorrected) %>%
  distinct()
DatasetKeywordListTemp2 <-DatasetKeywordListTemp1[complete.cases(DatasetKeywordListTemp1), ]
sum(is.na(DatasetKeywordListTemp2$KeywordsCorrected))

DatasetKeywordYearCount <- aggregate(DatasetKeywordListTemp2$Year, by=list(Year=DatasetKeywordListTemp2$Year, Keyword=DatasetKeywordListTemp2$KeywordsCorrected), FUN=length)
DatasetKeywordTotalCount <- aggregate(DatasetKeywordListTemp2$Year, by=list(Keyword=DatasetKeywordListTemp2$KeywordsCorrected), FUN=length)

# narrowing range for plot
Count <- number
DatasetKeywordNarrowRangeGraph <- top_n(DatasetKeywordTotalCount, Count)

a <- nrow(DatasetKeywordNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  DatasetKeywordNarrowRangeGraph <- top_n(DatasetKeywordTotalCount, Count)
  a <- nrow(DatasetKeywordNarrowRangeGraph)
}

# DatasetKeywordNarrowRangeGraph <- subset(DatasetKeywordTotalCount,x>Count)
SubsetKeywordNarrowRangeGraph <-subset(DatasetKeywordYearCount,Keyword %in% DatasetKeywordNarrowRangeGraph$Keyword)

SubsetKeywordNarrowRangeGraph[SubsetKeywordNarrowRangeGraph=="NA"] <-NA
SubsetKeywordNarrowRangeGraph <- na.omit(SubsetKeywordNarrowRangeGraph)


# range <- as.numeric(max(SubsetKeywordNarrowRangeGraph$x, na.rm = TRUE))
# 
# #############################################################
# #####                      Graph Range                  #####
# #############################################################
# 
# source("Code/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Scopus data)
#Breaks and labels for Interpol
# SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
#                                                      breaks = c(BreakRange,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
#                                                      labels=DatasetRange)

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Keyword,levels=rev(sort(unique(Keyword))))) # %>%
  # create a new variable from count
  # mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
  #                        labels=DatasetRange))  %>%
  #
  # change level order
  # mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
GraphTemp2 <- aggregate(GraphTemp1[, 1], list(GraphTemp1$KeywordsCorrected), min)

GraphTemp1$graphorder <- as.numeric(gsr(GraphTemp1$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"





