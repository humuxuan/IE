## app.R ##
library(shiny)
library(shinydashboard)
library(data.table)

# Read in the RF model and tree file
model <- readRDS("model.rds")

# desin UI
ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Exercise and Calories", titleWidth = 800), 
    dashboardSidebar(
      sidebarMenu(
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
    )),
  dashboardBody(
      tags$label(h3('Calculating Calories')),
      verbatimTextOutput('contents'),
      tableOutput("answer"),
      br(),
      br(),
      br(),
      tags$label(h3('Tips !!!')),
      br(),
      HTML("Regular physical activity is one of the most important things people can do to improve their health. 
           Moving more and sitting less have tremendous benefits for everyone, regardless of age, sex, race, ethnicity, or current fitness level.
           Here are some <b>useful tips</b> for different age group.
           <br/>
           <br/>
           <b>If you are disabled or pregnant, please view special</b>"),
      tags$style("#instructions {font-size:15px;
               color:black;
               display:block; }"),
      tags$style("#special_instructions {font-size:15px;
               color:black;
               display:block; }"),
      
      tabBox(
        # Title can include an icon
        height = "300px",
        tabPanel("Normal",
          fluidRow(
            column(width = 4, align = "left",uiOutput("text")),
            column(width = 8, htmlOutput( "instructions" )))
          ),
        tabPanel("Special",
                 radioButtons("radio",label = 'Special People',
                              choices = list("Disabilities" = 1, "Pregnant" = 2),selected = 1,inline=T),
                 fluidRow(
                   column(width = 4, align = "left",uiOutput("special_text")),
                   column(width = 8, htmlOutput( "special_instructions" )))
        )
        ,width = 12
          )
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
  
  # plot the image
  
  # special myval for special people 
  # special_myval <- eventReactive(
  #   input$submitbutton,{
  #   if (input$radio == 1){
  #     return(as.numeric(input$radio))
  #   }
  #   else{
  #     return(as.numeric(input$radio))
  #   }
  #   }
  # )
  
  #  myval for people
  myval <- eventReactive(
    input$submitbutton,{
      age <- as.numeric(input$Age)
      return (age)
    }
  )
  
  output$text <- renderUI({
    if(myval() >= 3 & myval() <= 5)
    {
      
      img(src='PA-age-group-icons-01.png',align = "left",width = 200, height = 200)
    }
    else if (myval() >=  6 & myval() <= 17)
    {
      img(src='PA-age-group-icons-02.png',align = "center",width = 200, height = 200)
    }
    else if (myval() >=  18 & myval() <= 64)
    {
      img(src='PA-age-group-icons-03.png',width = 200, height = 200)
    }
    else if (myval() >=  65)
    {
      img(src='OlderCoupleV2.png',width = 200, height = 200)
    }
    
  })
  
  output$instructions <- renderText({
    if(myval() >= 3 & myval() <= 5)
    {
      str1 <- paste('you are',myval(),' -- Preschool-Aged Children (3-5 years)')
      
      HTML(str1,'<br/>','<br/>',"<ul><li> Physical Activity <b>every day throughout the day.</b></li>
           <li><b>Active play</b> through a variety of enjoyable physical activities. </li></ul>")
    }
    else if (myval() >=  6 & myval() <= 17)
    {
      str1 <- paste('you are',myval(),' -- Children and Adolescents (6-17 years)')
      HTML(str1,'<br/>','<br/>',"<b>60 mins (1 hour)</b> or more of moderate-to-vigorous intensity physical activity daily.
                <br/>,As part of the 60 minutes, on at least 3 days a week, children and adolescents need: <br/>
           <ul>
           <li>
                Vigorous Activity such as running or soccer.
           </li>
           <li>
                 Activity that <br>strengthens muscles </br>such as climbing or push ups.
           </li>
           <li>
                Activity that <br>strengthens bones </br> such as gymnastics or jumping rope.
           </li>
           </ul>")
    }
    else if (myval() >=  18 & myval() <= 64)
    {
      str1 <- paste('you are',myval(),'-- Adults (18-64 years)')
      
      HTML(str1, "<br/><br/>
           <ui>
           <li>
           At least <b>150 minutes a week</b> of moderate intensity activity such as brisk walking.
           </li>
           <li>
           At least <b>2 days a week</b> of activities that strengthen muscles.
           </li>
           </ui>")
    }
    else if (myval() >=  65)
    {
      str1 <- paste('you are',myval(),'-- Older Adults (65 years and older)')
      
      HTML(
        str1, "<br/><br/>
           <ui>
           <li>
           At least 150 minutes a week of moderate intensity activity such as brisk walking.
           </li>
           <li>
           At least 2 days a week of activities that strengthen muscles.
           </li>
           <li>
           Activities to <b>improve balance </b>such as standing on one foot.
           </li>
           </ui>
        "
      )
    }
  })
  
  # special_myval for specal people plot and message
  output$special_text <- renderUI({
    if (as.numeric(input$radio) == 1){
      img(src='icon-disabililty-lg-small.png',align = "left",width = 200, height = 200)
    }
    else{
      img(src='icon-pregnant-woman-large.png',align = "left",width = 200, height = 200)
    }
  })
  
  output$special_instructions <- renderText({
    if (as.numeric(input$radio) == 1){
      HTML(
        "
        Adults with Chronic Conditions and Disabilities
        <br/><br/>
        <ui>
        <li>
        Get at least 150 minutes (for example, 30 minutes 5 days a week) of moderate-intensity aerobic physical activity a week.
        </li>
        <li>
        Get at least 2 days a week of muscle- strengthening activities that include all major muscle groups.
        </li>
        </ui>
        "
      )
    }
    else{
      HTML(
        "
        Pregnant and Postpartum Women
        <br/><br/>
        Get at least 150 minutes (for example, 30 minutes 5 days a week) of moderate intensity aerobic activity a week such as brisk walking during pregnancy and the postpartum period.
        "
      )
    }
  })
}
shinyApp(ui, server)


