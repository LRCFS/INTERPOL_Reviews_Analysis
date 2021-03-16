#Models are the server side functionally that determines how each "view" functions.
#Each model should only have the code for it's assocaited view

output$model_ExampleStartPage_graph1 = renderPlot({
  # generate an rnorm distribution and plot it
  dist <- rnorm(input$obs)
  hist(dist)
})