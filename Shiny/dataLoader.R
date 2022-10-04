#Get all file names from data dir
# filenames = list.files(APP_DATA_DIR, pattern=".csv", full.names=TRUE)
# ALL_DATA = ldply(filenames, read_csv)
ALL_DATA = read_csv("data/All_Data_Shiny.csv")
ALL_DATA <- as.data.frame(ALL_DATA)
# ALL_DATA = ALL_DATA %>% distinct()
Data_Jacard_Evidence = read_csv("data/Jaccard_evidence.csv")
Data_Jacard_Reference = read_csv("data/Jaccard_reference.csv")

# source("Code/GlobalEnvironment.R", local = TRUE);
# source("Code/Functions/SearchAndReplace.R", local = TRUE);
 source("Code/KeywordExtractionCode.R", local = TRUE);

# rm(FILTERED_DATASET_KEYWORD_LIST)
FILTERED_DATASET_KEYWORD_LIST <- DatasetKeywordList

All_KEYWORD = DatasetKeywordList %>% select("AIKeywords") %>% distinct()
# All_KEYWORD <- data.frame(value=All_KEYWORD, label=All_KEYWORD)  # SelectizeInput() can be used with dataframe
#                                                                  # and the columns are "label" and "value" with choice select to NULL,
#                                                                  # however it can work by selecting the column instead
#Get all keywords from ALL_DATA
ALL_EVIDENCE_TYPES = ALL_DATA %>% select("Evidence Type") %>% distinct()
ALL_SUBEVIDENCE_TYPES = ALL_DATA %>% select("Reference List") %>% distinct()

EVIDENCE_LISTS = c("Reference List","Evidence Type")
EVIDENCE_LISTS = as.data.frame(EVIDENCE_LISTS)
names(EVIDENCE_LISTS)[1] <- c("List")
EVIDENCE_LISTS$List = as.character(EVIDENCE_LISTS$List)

KEYWORD_LISTS = c("Author Keywords","Database Keywords","Both")
KEYWORD_LISTS = as.data.frame(KEYWORD_LISTS)
names(KEYWORD_LISTS)[1] <- c("List")
KEYWORD_LISTS$List = as.character(KEYWORD_LISTS$List)
