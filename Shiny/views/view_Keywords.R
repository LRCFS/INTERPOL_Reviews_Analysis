#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_Keywords = tabItem(tabName = "view_Keywords",
                        fluidRow(
                          box(title="Keywords",width=12,
                              p("The figure shows the most commonly used keywords in the selected reference input.")
                          )
                        ),
                                                fluidRow(
                          box(title="Evidence Category", width=6,
                              radioButtons(
                                "Keyword_evidenceCategory",
                                "Evidence List Filter",
                                choices = EVIDENCE_LISTS$List,
                                selected = NULL,
                                inline = TRUE,
                              )
                          ),
                          box(title="Keyword Lists", width=6,
                              radioButtons(
                                "Keyword_selectionList",
                                "Keyword Type Filter",
                                choices = KEYWORD_LISTS$List,
                                selected = NULL,
                                inline = TRUE,
                              )
                          )
                        ),
                        fluidRow(
                          box(title="Inputs", width=12,
                              radioButtons(
                                "keyword_evidenceType",
                                "Evidence Type Filter",
                                # choices = NULL,
                                 choices = character(0),
                                selected = character(0),
                                inline = TRUE,
                              )
                          )
                        ),
                        
                        
                        fluidRow(
                          box(title="Output", width=12,
                              plotOutput("model_Keywords_graph1")
                          )
                        )
                        
)
