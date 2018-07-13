input <- list()
input$file$datapath <- "/Users/bryan/Documents/shinypracticedata.csv"
input$header <- TRUE

library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Shiny Practice"),
  dashboardSidebar(
    sidebarMenu(
     menuItem("Graphs", tabName = "graphs"),
     menuItem("Table", tabName = "table")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo{
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size : 24px;
      }
    '))),
    tabItems(
      tabItem(tabName= "graphs",
        fluidRow(
          box(plotOutput("plot", height = 500)),
          box(
            title = "Controls",
            wellPanel(
              fileInput(inputId = "file", 
                        label = "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv"),
                        buttonLabel = "Browse",
                        placeholder = "No file selected"
              ),
              tags$hr(),
              checkboxInput("header", "Header", TRUE)
            ),
            
            selectInput(inputId = "type",
                        label = "Choose plot type",
                        choices = c(
                          "Points" = "p",
                          "Lines" = "l",
                          "Points & Lines" = "b"
                        )
            ),
            
            selectInput(inputId = "xvar", 
                        label = "Choose X variable", 
                        choices = c(
                          "Time" = "time",
                          "Value" = "value",
                          "Group" = "group",
                          "ID" = "id"
                        )
            ),
                  
            selectInput(inputId = "yvar", 
                        label = "Choose Y variable",
                        choices = c(
                          "Value" = "value",
                          "Time" = "time",
                          "Group" = "group",
                          "ID" = "id"
                        )
            ),
            
            selectInput(inputId = "color1",
                        label = "Choose color for first group",
                        choices = c(
                          "Blue" = "blue",
                          "Red" = "red",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Orange" = "orange",
                          "Black" = "black",
                          "White" = "white",
                          "Pink" = "pink"
                        )
            ),
            
            selectInput(inputId = "color2",
                        label = "Choose color for second group",
                        choices = c(
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Orange" = "orange",
                          "Black" = "black",
                          "White" = "white",
                          "Pink" = "pink"
                        )
            )
          )
        )
      ),
      
      tabItem(tabName = "table",
              box(tableOutput("table"), width = 7)      
      )
    )
  )
)

server <- function(input, output){
  output$plot <- renderPlot({
    inFile <- input$file
    validate(
      need(input$file != "", label = "Data set")
    )
    dt <- read.csv(inFile$datapath, header = input$header)
    plot(dt[,input$xvar], 
         dt[,input$yvar],
         col = c(input$color1, input$color2)[dt$group],
         pch = 19,
         type = input$type, 
         xlab = toupper(input$xvar),
         ylab = toupper(input$yvar),
         xlim = c(0,6),
         ylim = c(-3,5),
         main = "Practice Data")
  })
  output$table <- renderTable({
    dataFile <- input$file
    validate(
      need(input$file != "", label = "Data set")
    )
    datavals <- read.csv(dataFile$datapath, header = input$header)
    data.means <- by(datavals, datavals$id, function(x){
      means <- mean(x$value)
      means
    })
    data.means <- factor(data.means)
    data.sems <- by(datavals, datavals$id, function(x){
      sems <- std(x$value)
      sems
    })
    data.sems <- factor(data.sems)
    ids <- c("A1", "A2", "A3", "A4", "A5", "B6", "B7", "B8", "B9", "B10")
    datafr <- data.frame(ids, data.means, data.sems)
    datafr
  })
}
std <- function(x) sd(x)/sqrt(5)

shinyApp(ui = ui, server = server)