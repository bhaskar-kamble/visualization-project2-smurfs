


#https://rstudio.github.io/shinydashboard/get_started.html


## app.R ##
library(shiny)
library(shinydashboard)

messageData <- data.frame(from=c("Bhaskar","Didley"),message=c("Hurry up!","Calm down!"))

ui <- dashboardPage(
  
  dashboardHeader(title = "Basic dashboard",
                  dropdownMenuOutput("messageMenu"),
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName="dashboar",icon=icon("dashboard")),
      menuItem("Widgets",tabName="widdgets",icon=icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboar",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      tabItem(tabName="widdgets",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) { 
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$messageMenu <- renderMenu({
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
}

shinyApp(ui, server)