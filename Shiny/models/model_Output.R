#Models are the server side functionally that determines how each "view" functions.
#Each model should only have the code for it's associated view
updateSelectizeInput(session, 'Output_Keywords', choices = All_KEYWORD$AIKeywords, server = TRUE)
getFilteredKeywordList = reactive({
  
  
  if(!is.null(input$Output_Keywords))
  {
    FILTERED_DATASET_KEYWORD_LIST <- DatasetKeywordList %>% filter(AIKeywords == input$Output_Keywords)
    
    FILTERED_DATASET_KEYWORD_LIST <- FILTERED_DATASET_KEYWORD_LIST %>%
      select(AIKeywords,Authors,TitleCorrected,Source.title,Year,Volume,Issue,Page.start,Page.end,DOI) %>%
      distinct()
    OUTPUT_KEYWORD_LIST <- FILTERED_DATASET_KEYWORD_LIST %>%
      group_by(Authors,TitleCorrected,Source.title,Year,Volume,Issue,Page.start,Page.end,DOI) %>%
      summarise(AIKeywords = paste(AIKeywords, collapse="; "), Freq = n())
    OUTPUT_KEYWORD_LIST <- as.data.frame(OUTPUT_KEYWORD_LIST)
    
    OUTPUT_KEYWORD_LIST <- OUTPUT_KEYWORD_LIST[order(OUTPUT_KEYWORD_LIST$Freq, decreasing = TRUE), ]
    
    colnames(OUTPUT_KEYWORD_LIST)[which(names(OUTPUT_KEYWORD_LIST) == "AIKeywords")] <- "Keyword(s)"
    colnames(OUTPUT_KEYWORD_LIST)[which(names(OUTPUT_KEYWORD_LIST) == "TitleCorrected")] <- "Title"
    colnames(OUTPUT_KEYWORD_LIST)[which(names(OUTPUT_KEYWORD_LIST) == "Source.title")] <- "Source"
    colnames(OUTPUT_KEYWORD_LIST)[which(names(OUTPUT_KEYWORD_LIST) == "Page.start")] <- "Page start"
    colnames(OUTPUT_KEYWORD_LIST)[which(names(OUTPUT_KEYWORD_LIST) == "Page.end")] <- "Page end"
    
    return(OUTPUT_KEYWORD_LIST[, c('Keyword(s)','Authors','Title','Source','Year','Volume','Issue','Page start','Page end','DOI')])
           
  }
  else{
    return(NULL)
  }
})


output$Output_data = renderTable({
    return(getFilteredKeywordList())
})

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("lrcfs-keyword-data-explorer-output", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(getFilteredKeywordList() , file, row.names = FALSE)
  }
)