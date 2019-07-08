library(shiny)

ui <- fluidPage(
 selectInput(inputId="states",label="State",choices=) 
  
)






















server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)