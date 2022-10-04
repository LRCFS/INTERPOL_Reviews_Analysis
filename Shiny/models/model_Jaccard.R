#Models are the server side functionally that determines how each "view" functions.
#Each model should only have the code for it's associated view

output$model_Jaccard_graph1 = renderPlot({
    #Access all data loaded from data folder with ALL_DATA
  #Access all data loaded from data folder with ALL_DATA
  
  evidenceList <- input$Jaccard_evidenceType

  if (evidenceList == "Reference List"){
    #   Index Keywords only
    m <- Data_Jacard_Reference
    # dat_csvReduced <- ALL_DATA %>%
    #   distinct() %>%
    #   select('Reference List',TitleCorrected,Year)
    # names(dat_csvReduced)[1]<-c("EvidenceType")
  }
  else {
    m <- Data_Jacard_Evidence
    # dat_csvReduced <- ALL_DATA %>%
    #   distinct() %>%
    #   select('Evidence Type',TitleCorrected,Year)
    # names(dat_csvReduced)[1]<-c("EvidenceType")
  }
  

#   view(UserSelectionJaccard)

  # datainterpol = ALL_DATA %>% filter(Evidence == evidenceList);
  
  source("Code/GlobalEnvironment.R", local = TRUE);
  # source("Code/Functions/SearchAndReplace.R", local = TRUE);
  # source("Code/Functions/Diacritics.R", local = TRUE);
  source("Code/JaccardCode.R", local = TRUE);
  
  # ggplot(res.long, aes(col1, col2, fill = counts)) + 
  #   geom_tile() + 
  #   # the darkest blue colour is too dark to see the black text
  #   # so make high value labels grey to be able to read them
  #   geom_text(aes(label = round(counts,5)), size = 2.9, fontface = 'bold', colour = ifelse(res.long$counts > 1900, 'grey', 'black')) +
  #   scale_fill_gradient(low = 'white', high = 'darkblue', na.value = 'white') +
  #   # reorder y-axis to make the desired diagonal
  #   scale_y_discrete(limits = types.vec[order(types.vec, decreasing  = TRUE)]) +
  #   scale_x_discrete(position = 'top') +
  #   labs(x = '', y = '') +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle=60, hjust =0),
  #         axis.ticks = element_blank(),
  #         panel.grid = element_blank(),
  #         panel.border = element_blank())

  corrplot(result, method="color",
           #change font size of names
           tl.cex = 0.7,
           type="upper", 
           addCoef.col = "black", # Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           # Combine with significance
           number.digits = 3,
           sig.level = 0.01, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag=FALSE,
           #Change font size of coefficient 
           number.cex=0.7,
           cl.lim = c(0,1)
  )

}, width=700, height = 400)