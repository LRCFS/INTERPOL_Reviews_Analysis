#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_ExampleStartPage = tabItem(tabName = "view_ExampleStartPage",
                                fluidRow(
                                  box(
                                    sliderInput("obs", 
                                                "Number of observations:", 
                                                min = 1, 
                                                max = 1000,
                                                value = 500)
                                  ),
                                  box(
                                    plotOutput("model_ExampleStartPage_graph1")
                                  )
                                )
)
