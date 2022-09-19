## app.R ##
library(shiny)
library(shinydashboard)
library(data.table)
library(collapsibleTree) 

# Read in the RF model and tree file
model <- readRDS("model.rds")

# desin UI
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      tags$label(h3('Input parameters')),
      selectInput("Gender",
                  label = 'Gender',
                  choices = list('Male' = 0,
                                 'Female' = 1)),
      numericInput("Age", 
                   label = "Age", 
                   value = 0,min = 0),
      numericInput("Height", 
                   label = "Height(Cm)",  
                   value = 0,min = 0),
      numericInput("Weight", 
                   label = "Weight(Kg)",
                   value = 0,min = 0),
      numericInput("Duration", 
                   label = "Sport Duration(Minute)",  
                   value = 0,min = 0),
      numericInput("Heart_Rate", 
                   label = "Heart Rate(Per Minute)", 
                   value = 0,min = 0),
      numericInput("Body_Temp", 
                   label = "Body Tempreture(Degree Celsius)",
                   value = 0,min = 0),
      
      actionButton("submitbutton", "Submit", 
                   class = "btn btn-primary")
    ),
    mainPanel(
      tags$label(h3('Calculating Calories')),
      verbatimTextOutput('contents'),
      tableOutput("answer")
      )
    )
)

server <- function(input, output) {
  datasetInput <- reactive({ 
    if (input$Age >0 && input$Height >0 && input$Weight >0 && input$Duration >0 && input$Heart_Rate >0 && input$Body_Temp >0)
    {
      df <- data.frame(
        Name = c("Gender",
                 "Age",
                 "Height",
                 "Weight",
                 "Duration",
                 "Heart_Rate",
                 "Body_Temp"),
        Value = as.character(c(as.integer(input$Gender),
                               input$Age,
                               input$Height,
                               input$Weight,
                               input$Duration,
                               input$Heart_Rate,
                               input$Body_Temp)),
        stringsAsFactors = FALSE)
      # change data into dataframe
      Claries <- 0
      df <- rbind(df, Claries)
      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      predict_answer = as.double(predict(model,test))
      h1(sprintf("%0.1f Calories",predict_answer))
    }
    else{
      h1("Please Check Paramenters")
    }
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  output$answer <- renderPrint({
    
    if (input$submitbutton>0) {
      isolate(datasetInput()) 
    }
  })
}
shinyApp(ui, server)


