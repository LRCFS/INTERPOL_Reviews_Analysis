
#############################################################
##### Country and Institution figures
# Number of individual country appearing on the figures
NumberCountry <- 20

#############################################################
##### Keyword Figures
# the number of keywords (top most frequent) appearing in the figure
number <- 40   # target number of keywords apearing in the keyword figure
maximum <- 45  # maximum number of keywords appearing in the keyword figure

# limit to the number keywords for loop and top(n)
Count <- number

# colour palette for the Keyword figure
pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#FF00FF","#330033")

#     Select one of the following three options
KeywordEntries <- "Author.Keywords"
# KeywordEntries <- "Database.Keywords"
# KeywordEntries <- "All.Keywords"

