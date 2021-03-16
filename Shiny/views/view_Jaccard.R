#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_Jaccard = tabItem(tabName = "view_Jaccard",
                       fluidRow(
                         box(title="Jaccard Similarity Index", width=12,
                             p("Relationship between lists, using document titles. A value of 0 indicates no similarity and 1 identical sets.")
                         )
                       ),
                       
                        fluidRow(
                          box(title="Inputs", width=12,
                              radioButtons(
                                "Jaccard_evidenceType",
                                "Evidence List Filter",
                                choices = EVIDENCE_LISTS$List,
                                selected = NULL,
                                inline = TRUE,
                              )
                          )
                        ),

                        fluidRow(
                          box(title="Output", width=12,
                              plotOutput("model_Jaccard_graph1")
                          )
                        ),
                        
)