
#############################################################
#####     Select one of the following two options     #####
#############################################################
# this is select the groups or subgroup of evidence type. If noe are selectes, it will be combined (grouped) "Evidence" only.
# EvidenceEntries <- "SubEvidence"   #individual and splitted evidence type if applied; based on colum "SubEvidence"
# EvidenceEntries <- "Evidence" #individual and grouped evidence type; based on colum "Evidence"

#############################################################
#####                 Folders and files                 #####
#############################################################
# 
# 
# if (UserSelectionJaccard == "SubEvidence"){
#   #   Index Keywords only
#   dat_csvReduced <- ALL_DATA %>%
#     distinct() %>%
#     select(SubEvidence,Title,Year)
#   names(dat_csvReduced)[1]<-c("EvidenceType")
# }    else {
#   dat_csvReduced <- ALL_DATA %>%
#     distinct() %>%
#     select(Evidence,Title,Year)
#   names(dat_csvReduced)[1]<-c("EvidenceType")
# }

# Convert to upper character titles
# dat_csvReduced$Title <- toupper(dat_csvReduced$Title)
# 
# # replace specific non-alphanumeric characters
# dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "Ó", " ")
# dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "Ñ", " ")
# 
# # replace all non-english characters
# dat_csvReduced$Title <- removeDiacritics(dat_csvReduced$Title)
# 
# # swap out all non-alphanumeric characters
# dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "[^[:alnum:]]", " ")
# # remove the double space the previous line create
# dat_csvReduced$Title <- str_replace_all(dat_csvReduced$Title, "  ", " ")
# 
# # remove leadind and trailing white space
# dat_csvReduced$Title <- trimws(dat_csvReduced$Title)
# 
# dat_csvReduced <- dat_csvReduced %>%
#   distinct()
# 
# #############################################################
# #####                  Data cleansing                   #####
# #############################################################
# 
# #read the corrected list for Title
# # TitleCorrected <- read.csv("Code/CorrectionLists/TitleCorrections.csv", header=TRUE, encoding = 'UTF-8')
# 
# # dat_csvReduced$TitleCorrected <- gsr(as.character(dat_csvReduced$Title),as.character(TitleCorrected$Original),as.character(TitleCorrected$Corrected))
# 
# # replace "NO TITLE AVAILABLE" with NA
# dat_csvReduced$TitleCorrected <- na_if(dat_csvReduced$TitleCorrected, "NO TITLE AVAILABLE")
# 
# # remove entries with missing (i.e. NA) Titles 
# dat_csvReducedCorrectedNarrow <- dat_csvReduced[complete.cases(dat_csvReduced[ ,2]),]
# 
# # replace missing years (i.e. NA) with "undefined"
# dat_csvReducedCorrectedNarrow$Year[is.na(dat_csvReducedCorrectedNarrow$Year)] <- c("undefined")
# 
# # keep wanted columns
# dat_csvFinal <- dat_csvReducedCorrectedNarrow %>%
#   select(EvidenceType,Year,TitleCorrected) %>%
#   # select(EvidenceType,TitleCorrected) %>%
#   distinct()
# 
# dat_csvFinal <-as.data.frame(dat_csvFinal)
# 
# dat_csvFinal <- dat_csvFinal %>%
#   dplyr::group_by(EvidenceType, TitleCorrected) %>%
#   dplyr::summarise(Year=paste(Year, collapse = ";"))
# 
# # Pivot to group by Title
# # Warning, some records may have more that one year attached to them, different version. To remove the warning, add values_fn = list(Year = length)
# TableTitleList <- dat_csvFinal %>%
#   # pivot_wider(names_from = EvidenceType, values_from = Year)
#   pivot_wider(names_from = EvidenceType, values_from = Year, values_fn = list(Year = length))
# # pivot_wider(names_from = EvidenceType, values_from = TitleCorrected, values_fn = list(TitleCorrected = length))
# # Sum the rows (except first column) to give frequency count
# 
# TableTitleList$sum <- rowSums(!is.na(TableTitleList[,-1]))
# 
# # select top frequency count
# topList <- TableTitleList[(TableTitleList$sum>1),]
# 
# # narrowing range for plot
# TableTitleListReduced <- top_n(TableTitleList, 10)
# 
# #Export to text file 
# #write.table(TableTitleListReduced, file=paste0(Results.dir,sprintf("%s.txt",InterpolTopCitation)), sep = "\t", row.names = F)
# 
# #############################################################
# # Correlation matrix
# #############################################################
# 
# # pivot data to wider format
# Correlation <- dat_csvFinal %>%
#   pivot_wider(names_from = EvidenceType, values_from = Year, values_fn = list(Year = length))
# 
# df = Correlation[,-1]
# df[!(is.na(df))] <- 1 # necessary when different dates are given for a document (Title) meaning output greater than 1
# # if left to orginal number (not 1), then  value <- jaccard(df[,n], df[,n]) # not equal to 1
# df[is.na(df)] <- 0
# 
# ##############################################
# ######  Jaccard coefficient calculation ######
# ##############################################
# 
# # For all the evidence against each other, including the p values
# rm(datasetEvidence)
# for (i in 1:ncol(df)) {
#   for (j in 1:ncol(df)) {
#     # if the merged dataset doesn't exist, create it
#     if (!exists("datasetEvidence")){
#       datasetEvidence_i <- colnames(df)[i]
#       datasetEvidence_j <- colnames(df)[j]
#       datasetEvidence_Jaccard <-jaccard(df[,i], df[,j])
#       # jt = jaccard.test(df[,i], df[,j], method = 'bootstrap')
#       datasetEvidence <- data.frame(datasetEvidence_i,datasetEvidence_j,datasetEvidence_Jaccard)
#     }
#     
#     # if the merged dataset does exist, append to it
#     if (exists("datasetEvidence")){
#       datasetEvidence_i <- colnames(df)[i]
#       datasetEvidence_j <- colnames(df)[j]
#       datasetEvidence_Jaccard <-jaccard(df[,i], df[,j])
#       # jt = jaccard.test(df[,i], df[,j], method = 'bootstrap')
#       temp_dataset <- data.frame(datasetEvidence_i,datasetEvidence_j,datasetEvidence_Jaccard)
#       datasetEvidence<-full_join(datasetEvidence, temp_dataset)
#       rm(temp_dataset)
#       
#     }
#     
#   }}
#
# m <- dcast(datasetEvidence, datasetEvidence_i ~ datasetEvidence_j, value.var = "datasetEvidence_Jaccard")
m <- as.data.frame(m)

result <- m[-1]
row.names(result) <- m$datasetEvidence_i

result <- as.matrix(result)

# for figure export
# CorVar <- paste0(JaccardExport,EvidenceEntries)
# 
# # corrplot does not have an save funtionality as for example ggplot. Here are it is done instead:
# png(file.path(Results.dir,paste0(sprintf("%s.png",CorVar))), width = 600, height = 480,
#     units = "px", type = "cairo")
# par(mar=c(1,1,1,520)+0.1)
# dev.off()

# result[upper.tri(result)] <- NA
# # add row names as col1
# res = data.frame(col1 = row.names(result), result)
# # make long format for ggplot
# res.long = res %>% 
#   gather(col2, counts, Biological.Evidence.DNA:Toxicology)
# # replace dots added to names in col2 by data.frame() above
# res.long$col2 = gsub( '.', ' ', res.long$col2, fixed = TRUE)
# # fix BE and PG as '-' were also replaced with '.'
# res.long[res.long$col2 == 'Biological Evidence DNA','col2'] <- 'Biological Evidence-DNA'
# res.long[res.long$col2 == 'Paint Glass','col2'] <- 'Paint-Glass'
# # get vector of types for reordering the y-axis
# types.vec = row.names(result)
# 