#Models are the server side functionally that determines how each "view" functions.
#Each model should only have the code for it's associated view

observeEvent(input$Keyword_evidenceCategory,{
  evidenceKeywordCat = input$Keyword_evidenceCategory
  possibleEvidenceType = NULL
  if(evidenceKeywordCat == "Evidence Type")
  {
    possibleEvidenceType = ALL_DATA %>% select(evidenceKeywordCat) %>% distinct()
    possibleEvidenceType = possibleEvidenceType$`Evidence Type`
    
  }
  else{
    possibleEvidenceType = ALL_DATA %>% select(evidenceKeywordCat) %>% distinct()
    possibleEvidenceType = possibleEvidenceType$`Reference List`
  }
  
  #updateRadioButtons(session,"keyword_evidenceType", choices = character(0)) #remove all existing choices from the list
  updateRadioButtons(session,"keyword_evidenceType", inline = TRUE, choices = possibleEvidenceType) #add new choices
  updateRadioButtons(session,"keyword_evidenceType", inline = TRUE, choices = possibleEvidenceType, selected = character(0))
})

output$model_Keywords_graph1 = renderPlot({

  evidenceKeyword = input$keyword_evidenceType

  evidenceKeywordCat = input$Keyword_evidenceCategory

  keywordType = input$Keyword_selectionList
  
  #Return null (i.e. don't show a graph) if we haven't selected all the required values
  if(is.null(evidenceKeyword) || is.null(evidenceKeywordCat) || is.null(keywordType)){ return(NULL) }
  
 
  # #Choose what data to use based on evidenceKeyword choice
  # #Filter data based on evidence type
  datainterpol = ""
  if(evidenceKeywordCat == "Evidence Type")
  {
    datainterpol = ALL_DATA %>% filter(`Evidence Type` == evidenceKeyword)
  }
  else{
    datainterpol = ALL_DATA %>% filter(`Reference List` == evidenceKeyword)
  }
  if(count(datainterpol) > 0)
  {
  source("Code/GlobalEnvironment.R", local = TRUE);
  source("Code/Functions/SearchAndReplace.R", local = TRUE);
  source("Code/KeywordsCode.R", local = TRUE);

  # ggplot(SubsetKeywordNarrowRangeGraph, aes(x=Year, y=Keyword, fill=x))+
  #   geom_tile(colour="white",size=0.2);

  ggplot(GraphTemp1,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=x))+
    geom_tile(colour="white",size=0.2) +
    theme(axis.title.y=element_blank())
    
  }
  else
  {
    return(NULL)
  }
    #+
  # guides(fill=guide_legend(title="Count"))+
  # labs(x="Year",y="",title="")+
  # scale_y_discrete(expand=c(0,0))+
  # #scale_x_continuous(breaks=c(1925,1935,1945,1955,1965,1975,1985,1995,2005,2015))+
  # scale_fill_manual(values=c(pal),na.value = "grey90")+
  # #coord_fixed()+
  # theme_grey(base_size=8)+
  # theme(text = element_text(family = "Arial"),
  #       legend.position="right",legend.direction="vertical",
  #       legend.title=element_text(colour=textcol),
  #       legend.margin=margin(grid::unit(0,"cm")),
  #       legend.text=element_text(colour=textcol,size=7),
  #       legend.key.height=grid::unit(0.8,"cm"),
  #       legend.key.width=grid::unit(0.2,"cm"),
  #       axis.text.x=element_text(size=8,colour=textcol),
  #       axis.text.y=element_text(vjust=0.2,colour=textcol),
  #       axis.ticks=element_line(size=0.4),
  #       plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
  #       panel.border=element_blank(),
  #       plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
  #       plot.title=element_text(colour=textcol,hjust=0,size=12))
  
  #Plot histogram
  # ggplot(data, aes(Year))+geom_histogram();
})

