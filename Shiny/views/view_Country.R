#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_Country = tabItem(tabName = "view_Country",
                       fluidRow(
                         box(title="Country of origin",width=12,
                             p("For the Country figure, duplicated countries in the Author's affiliation are removed. For Institutions, all the listed countries are counted.")
                         )
                       ),
                       fluidRow(
                         box(title="Inputs", width=12,
                             radioButtons(
                               "country_evidenceType",
                               "Evidence Type Filter",
                               choices = ALL_EVIDENCE_TYPES$`Evidence Type`,
                               selected = NULL,
                               inline = TRUE,
                             )
                         )
                       ),
                       
                       fluidRow(
                         box(title="Country",
                             plotOutput("model_Country_graph1")
                         ),
                         box(title="Institutions",
                             plotOutput("model_Country_graph2")
                         )
                       ),

)
