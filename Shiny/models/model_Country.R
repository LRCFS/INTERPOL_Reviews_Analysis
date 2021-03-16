#Models are the server side functionally that determines how each "view" functions.
#Each model should only have the code for it's associated view

source("Code/Functions/extractCountries.R", local = TRUE);

output$model_Country_graph1 = renderPlot({
  #Access all data loaded from data folder with ALL_DATA
  evidenceType = input$country_evidenceType
  
  print(paste0("Selected: ", evidenceType))
  
  #Filter data based on evidence type
  datainterpol = ALL_DATA %>% filter(`Evidence Type` == evidenceType);
  
  source("Code/GlobalEnvironment.R", local = TRUE);
  
  # extract county counts from countries
  cntry.dat = countryCounts(datainterpol$Countries)
  cntry.dat = cntry.dat %>% filter(!is.na(Country))
  
  # Plot figure country distribution
  ggplot(cntry.dat, aes(x=Country, y=Count, fill=Continent)) + 
    geom_col() +
    scale_fill_manual(values = c(brewer.pal(5, "Set1"), "gray"), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
    xlab('Country Affiliation') +
    ylab('Total Papers') +
    coord_flip() +
    theme_minimal() +
    theme()
  
})

# Institutions

output$model_Country_graph2 = renderPlot({
  
  #Access all data loaded from data folder with ALL_DATA
  evidenceType = input$country_evidenceType
  
  #Filter data based on evidence type
  datainterpol = ALL_DATA %>% filter(`Evidence Type` == evidenceType);
  
  source("Code/GlobalEnvironment.R", local = TRUE);
  
  # extract county counts from countries
  cntry.dat = countryCounts(datainterpol$Institutions)
  cntry.dat = cntry.dat %>% filter(!is.na(Country))
  
  # Plot figure country distribution
  ggplot(cntry.dat, aes(x=Country, y=Count, fill=Continent)) +
    geom_col() +
    scale_fill_manual(values = c(brewer.pal(5, "Set1"), "gray"), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
    xlab('Country Affiliation') +
    ylab('Total Papers') +
    coord_flip() +
    theme_minimal() +
    theme()
  
  
})