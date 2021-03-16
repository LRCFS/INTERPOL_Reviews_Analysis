#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_Output = tabItem(tabName = "view_Output",
                      fluidRow(
                        box(width=12,
                        p("Document search using available keywords. The output list presented will contain records with any of the specified keywords, with documents having the most keyword matches appearing on top of the list.")
                        )
                      ),
                      fluidRow(
                        box(title="Keyword Search", width=12,
                            selectizeInput(
                              "Output_Keywords",
                              "Select Keyword(s)",
                              #choices = All_KEYWORD$AIKeywords,
                              choices = NULL,
                              multiple = TRUE
                              )
                            )
                        ),
                      fluidRow(
                        box(title="Output", width=12,
                            downloadButton("downloadData", "Download"),
                            tableOutput("Output_data")
                            )
                        )
                      )

