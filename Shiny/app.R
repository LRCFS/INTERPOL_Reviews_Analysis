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
#source("views/view_ExampleStartPage.R")
#source("views/view_AnotherTab.R")
source("views/view_FrontPage.R")
source("views/view_Keywords.R")
source("views/view_Country.R")
source("views/view_Jaccard.R")
source("views/view_Output.R")

#Create our UI object using shinyDashboardPlus
ui = dashboardPagePlus(title=paste0(APP_DEV_SHORT," - ",APP_NAME_SHORT," - v",APP_VER),
                       dashboardHeaderPlus(title = APP_NAME),
                       dashboardSidebar(
                         #Sidebar items created here load a "tabName". This is the name set in the "tabItem" object called below in the dashboardBody.
                         #Look at /views/view_ExampleStartPage.R and note how the "tabName" is set in the object.
                         #Icons defined here use the Font Awesome icons (https://fontawesome.com/v4.7.0/icon/)
                         sidebarMenu(
                           #menuItem("Example Start Page",tabName = "view_ExampleStartPage", icon = icon("play")),
                           #menuItem("Another Tab", tabName = "view_AnotherTab", icon = icon("address-book")),
                           menuItem("Introduction", tabName = "view_FrontPage", icon = icon("address-book")),
                           menuItem("Keywords Breakdown", tabName = "view_Keywords", icon = icon("address-book")),
                           menuItem("Country Breakdown", tabName = "view_Country", icon = icon("address-book")),
                           menuItem("Jaccard", tabName = "view_Jaccard", icon = icon("address-book")),
                           menuItem("Output", tabName = "view_Output", icon = icon("address-book"))
                         )
                       ),
                       
                       dashboardBody(
                         useShinyjs(), #Include the shinyJS functionality
                         tags$head(tags$link(rel = "shortcut icon", href = "images/favicon.ico")), #Define a favicon which needs to be created with something like: https://favicon.io/
                         tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/lrcfs.css")), #Include some default LRCFS styling
                         tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")), #Use the style.css file to add your custom styling
                         tabItems( #An example tabItem has been created in /views/view_ExampleStartPage.R. For additional tabs its recommended that you create views for each one.
                           #view_ExampleStartPage,
                           #view_AnotherTab,
                           view_FrontPage,
                           view_Keywords,
                           view_Country,
                           view_Jaccard,
                           view_Output
                         )
                       ),
                       
                       footer = dashboardFooter( #The footer should remain fairly static between all LRCFS shiny dashboard apps
                         left_text = HTML(paste0("<div class='footerItem'>",APP_NAME," (v",APP_VER,") &copy;",format(Sys.time(), "%Y"),"</div>
                                           <div class='footerItem'><a href='https://www.dundee.ac.uk/leverhulme/'>Developed by ", APP_DEV_SHORT, "</a></div>
                                           <div class='footerItem'><a href='https://www.leverhulme.ac.uk/'>Funded by The Leverhulme Trust</a></div>")),
                         right_text = HTML("<div class='footerLogo'><a href='https://www.dundee.ac.uk/leverhulme/'><img src='images/lrcfs-logo-colour.png'  alt='Visit LRCFS website' /></a></div>
                                            <div class='footerLogo'><a href='https://www.leverhulme.ac.uk'><img src='images/lt-logo-colour.png' alt='Visit The Leverhulme Trust website' /></a></div>")
                       )
)

#Create the server object
#This is all our "server side" logic that controls the UI
server = function(input, output, session) {
  
  #Create a "model" for each associated "view"
  #source("models/model_ExampleStartPage.R", local = TRUE)
  #source("models/model_AnotherTab.R", local = TRUE)
  source("models/model_Keywords.R", local = TRUE)
  source("models/model_Country.R", local = TRUE)
  source("models/model_Jaccard.R", local = TRUE)
  source("models/model_Output.R", local = TRUE)
}

#Start the shiny app using the UI and SERVER elements defined above
shinyApp(ui, server)
