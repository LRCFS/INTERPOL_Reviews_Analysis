#Clear all lists from memory to avoid unintentional errors
rm(list = ls())

#Load required libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)

library(plyr)
library(ggplot2)
library(dplyr)
library(expss)
library(stringr)
library(tidyr)
library(maps)
library(countrycode)
library(RColorBrewer)
library(tidyverse) # for ubuntu 18.04 see post: https://blog.zenggyu.com/en/post/2018-01-29/installing-r-r-packages-e-g-tidyverse-and-rstudio-on-ubuntu-linux/
library(corrplot)
library(jaccard) # requires renv::install("bioc::qvalue") for packaging
## possible error message : "Error: package or namespace load failed for ‘jaccard’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
##                           there is no package called ‘qvalue’"
## qvalue is part of "BioManager"
## Can be installed follwing these steps in the Console 
# install.packages("BiocManager")
# BiocManager::install("qvalue")

library(reshape2)
library(readr)

#Load a global config file for easy access to application variables (APP_NAME_SHORT, APP_VER etc are set in here)
source("appConfig.R")

#Load all data from disk
source("dataLoader.R")

#Load common functions
#source("Code/Functions/Diacritics.R");

#Load any "views" - files that control how the page looks (UI controls)
source("views/view_FrontPage.R")
source("views/view_Keywords.R")
source("views/view_Country.R")
source("views/view_Jaccard.R")
source("views/view_Output.R")

#Create our UI object using shinyDashboardPlus
ui = dashboardPagePlus(title=paste0(APP_DEV_SHORT," - ",APP_NAME_SHORT," - v",APP_VER),
                       dashboardHeaderPlus(title = APP_NAME),
                       dashboardSidebar(
                         sidebarMenu(
                           menuItem("Introduction", tabName = "view_FrontPage", icon = icon("play")),
                           menuItem("Keywords Breakdown", tabName = "view_Keywords", icon = icon("tag")),
                           menuItem("Country Breakdown", tabName = "view_Country", icon = icon("globe-europe")),
                           menuItem("Jaccard", tabName = "view_Jaccard", icon = icon("signal")),
                           menuItem("Output", tabName = "view_Output", icon = icon("file-download"))
                         )
                       ),
                      
                       dashboardBody(
                         useShinyjs(), #Include the shinyJS functionality
                         tags$head(tags$link(rel = "shortcut icon", href = "images/favicon.ico")), #Define a favicon which needs to be created with something like: https://favicon.io/
                         tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/lrcfs.css")), #Include some default LRCFS styling
                         tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")), #Use the style.css file to add your custom styling
                         tabItems(
                           view_FrontPage,
                           view_Keywords,
                           view_Country,
                           view_Jaccard,
                           view_Output
                         )
                       ),
                      
                       footer = dashboardFooter( #The footer should remain fairly static between all LRCFS shiny dashboard apps
                         left_text = HTML(paste0("<div class='footerItem'><a href='",APP_LINK,"' target='_blank'>",APP_NAME,"</a> (v",APP_VER,") &copy;",format(Sys.time(), "%Y"),"</div>
										                      <div class='footerItem'>",APP_DOI_HTML,"</div>
                                           <div class='footerItem'><a href='https://www.dundee.ac.uk/leverhulme/' target='_blank'>Developed by ", APP_DEV_SHORT, "</a></div>
                                           <div class='footerItem'><a href='https://www.leverhulme.ac.uk/' target='_blank'>Funded by The Leverhulme Trust</a></div>")),
                         right_text = HTML("<div class='footerLogo'><a href='https://www.dundee.ac.uk/leverhulme/' target='_blank'><img src='images/lrcfs-logo-colour.png'  alt='Visit LRCFS website' /></a></div>
                                            <div class='footerLogo'><a href='https://www.leverhulme.ac.uk' target='_blank'><img src='images/lt-logo-colour.png' alt='Visit The Leverhulme Trust website' /></a></div>")
                       )
)

#Create the server object
#This is all our "server side" logic that controls the UI
server = function(input, output, session) {
  
  #Create a "model" for each associated "view"
  source("models/model_Keywords.R", local = TRUE)
  source("models/model_Country.R", local = TRUE)
  source("models/model_Jaccard.R", local = TRUE)
  source("models/model_Output.R", local = TRUE)
}

#Start the shiny app using the UI and SERVER elements defined above
shinyApp(ui, server)
