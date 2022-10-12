# import packages
library(shinythemes)
library(plotrix)
library(shiny)
library(dplyr)
library(DT)
library(shinyjs)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(DBI)
library(RMySQL)
library(readxl)

 # connect mysql workbench
host = 'database-mysql.c67oqqqukqcy.ap-southeast-2.rds.amazonaws.com'
 port=3306
 dbname = "food"
 user='admin'
 password = 'Qwe1106626229'
 mydb <- dbConnect(MySQL(), dbname = dbname,user = user,password = password ,host = host, port = port)
 
 # define select sql function
 creat_query <- function(food_name){
     food_name = paste("'", food_name, "'", sep="")
     answer = paste('select * from new_dataset where Name =',food_name,seq='')
     
     return (answer)
 }
 
 # collect all the name of food
 result = dbSendQuery(mydb, "select Name from new_dataset")
 food_name = dbFetch(result)
 dbClearResult(result)# must
 food_name = food_name$Name
 #break connect
 dbDisconnect(mydb)
 
 # read food file contain recommeded servings
 recommend_service = read_excel('service.xlsx')

# ui design
ui <- dashboardPage(
    dashboardHeader(title = "Nutrition Calculator"),
    # select the food in sidebar
    dashboardSidebar(
      width = 220,
      # select Male or Female
      radioButtons("radio", h3("Select Gender"),
                   choices = list("Male" = 'Male', "Female" = 'Female'),selected = 1),
      
        selectizeInput(
            'food','Ingredient',choice = food_name,
            options = list(
                placeholder = 'Type to search for ingredient',
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        #  measurement after select food
        conditionalPanel('input.food != ""', 
                         selectizeInput('measure_unit', 'Measure Unit', choices = c("Select an ingredient" = "")),
                         numericInput('quantity', 'Numbers of Servings', value = 1, min = 0, step = 1)),
        # some action button
        conditionalPanel('input.food != ""',
                         actionButton("add", "Add ingredient"),
                         actionButton("remove", "Remove ingredient"))
        #actionButton("add", "Add ingredient"),
        #actionButton("remove", "Remove ingredient")
    ),
    # visualization in the dashboard body
    dashboardBody(
      # import css
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      
      # Nested HTML tags
      div(class = 'title_1', h1('Nutrition Calcualtor')),
      br(),
      br(),
      br(),
      fluidRow(
        valueBoxOutput("calories",width = 4),
        # valueBoxOutput("fat",width = 4),
        tabBox(
          # Title can include an icon
          title = "Basic information",
          height = "450px",
          tabPanel("Ingredients",
                   # box(title = "Ingredients",
                   #     status = "primary",
                   #     solidHeader = T,
                   #     width = 8,
                   #     collapsible = T,
                   #     div(DT::DTOutput("ing_df"),style = "font-size: 100%;"))
                   DT::DTOutput("ing_df"),
                   useShinyjs(),
                   inlineCSS(list("table" = "font-size: 18px"))
          ),
        width = 12),
        # column(5,
        #        verticalLayout(
        #          br(),
        #          br(),
        #          valueBoxOutput("calories",width = 4),
        #          valueBoxOutput("fat",width = 4)
        #        )
        #        )

        ),
        # fluidRow(
        # 
        #     box(title = "Ingredients",
        #         solidHeader = T,
        #         background = "yellow",
        #         width = 5,
        #         collapsible = T,
        #         div(DT::DTOutput("ing_df"),style = "font-size: 100%;")),
        #     valueBoxOutput("calories",width = 3)
        #     #valueBoxOutput("over_nutrient"),
        #     #valueBoxOutput("rich_nutrient")
        #  ),
        # fluidRow(
        #   box(title = "Nutrition Table",
        #       solidHeader = T,
        #       background  = "aqua",
        #       width = 8, 
        #       collapsible = T,
        #       collapsed = F,
        #       tags$p(textOutput("serving", inline = T)),
        #       div(DT::DTOutput("nutrient_table"), style = "font-size: 100%;"))
        # ),
      fluidRow(
        box(title = 'Servings for Each ingredients', solidHeader = T,
            background = 'black', width = 13, collapsible = T,
            HTML(
              "
           <ui>
           Please Pay attention the red line or green line, which shows the standard servings for Female and Male.
           After select Ingredent and Click add, Let we check if you exceed the standard
           <li>
           Red line: The Standard serving for Female
           </li>
           <li>
           Green line: The Standard serving for Male
           </li>
           </ui>
        "
            ),
            plotlyOutput('Servings_plot')
            )
      )
 )
 #skin = 'black'
)

server <- function(input, output, session){
    # make reactive to store ingredients
    ing_df <- shiny::reactiveValues()
     ing_df$df <- data.frame("quantity" = numeric(),
                             "quantity_for_serving" = numeric(),
                             "Measurement" = character(),
                             "Name" = character(),
                             "Category" = character(),
                             stringsAsFactors = F)
     ing_df$measure <- data.frame(
                                  "Name" = character(),
                                  "kj_per_serving"=numeric(),
                                 stringsAsFactors = F)

     # display nutrients necessary for label
      nutrition_df = eventReactive(input$food,{
          mydb <- dbConnect(MySQL(), dbname = dbname,user = user,password = password ,host = host, port = port)
     
            # create find sql
          find_sql = creat_query(input$food)
          find_nutuition = dbSendQuery(mydb, find_sql)
          nutrition_df = dbFetch(find_nutuition)
          dbClearResult(find_nutuition)
          #break connect
          dbDisconnect(mydb)
          # 鏈夎叮鐨勯棶????
          nutrition_df
      }
      )


     # step 2 update the measure unit for singular ingredient
      observe({
          units <- unique(paste0(nutrition_df()$Measurement))
          units <- paste0('1 serving = ',nutrition_df()$quantity_for_serving,units)
          updateSelectInput(session, "measure_unit", "Measure Unit", choices = units)
      })
    
      # step 3 update the ingredient dataframe
      observeEvent(input$remove, {
          isolate(ing_df$df<-ing_df$df[-(nrow(ing_df$df)),])
          isolate(ing_df$measure <- ing_df$measure[-nrow(ing_df$measure),])
          #print(ing_df$measure)
      })
    
      observeEvent(input$add, {
          temp = nutrition_df()$Category
          temp_quantity_for_serving= nutrition_df()$quantity_for_serving
          temp_Measurement = nutrition_df()$Measurement
          isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$quantity,
                                                       temp_quantity_for_serving,
                                                       temp_Measurement,
                                                      input$food,
                                                      temp
                                                      ))
         # get actual working ingredient dataframe for dplyr
          input_measure <- nutrition_df()[,c(2,5)]
          isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
          # update choices
          updateNumericInput(session, 'quantity', 'Numbers of Servings', 1)
          updateSelectizeInput(session, 'measure_unit', 'Measure Unit')
          #updateSelectInput(session, 'food', 'Ingredient', choices = food_name)
      })
      
      # calcualte the main nutuirtion
      sum_nutrition <- reactive({
        measure_food_df <- ing_df$measure
        temp_df = ing_df$df
        measure_food_df$quantity <- temp_df$quantity
        
        # claculate the sum of the nutrition for servings
        measure_food_df = measure_food_df %>% 
          mutate(
            KJ = as.numeric(kj_per_serving) * as.numeric(quantity)
          )
        
        # add all the nutrition together
        nutrition = sum(measure_food_df[,-c(1,2,3)])
        new_df <-  matrix(nutrition,nrow=1,ncol=1,byrow=TRUE)
        colnames(new_df) <- c("KJ")
        new_df
        
      })
      
      # calculate the service for them in one day
      sum_service <- reactive({
        temp_df <- ing_df$df %>% select(quantity, Category)
        sum_df = as.data.frame(matrix(nrow=0,ncol=2))
        colnames(sum_df) = c('type', 'x')
        if (nrow(temp_df) >=1){
          sum_df <- aggregate(as.numeric(temp_df$quantity), by=list(type = temp_df$Category),sum)
        }
        colnames(sum_df) = c('type', 'Ingredent_number')
        sum_df
      })
      
      # calculate the sum of sum_df
      change_df <- reactive({
        df_temp = ing_df$df
        df_temp$quantity = as.numeric(df_temp$quantity)
        df_temp$quantity_for_serving = as.numeric(df_temp$quantity_for_serving)
        #print(df_temp)
        if (nrow(df_temp) >=1){
          df_temp = aggregate(.~Measurement+Name+Category, df_temp, sum)
          df_temp$new_combine = paste(df_temp[,5],df_temp[,1])
          df_temp = df_temp[,c(4,6,2,3)]
        }
        df_temp
      })
      
      # output the value
      # plot the KJ
      # value boxes
      output$calories <- renderValueBox({
        valueBox(paste0(sum_nutrition()[,c(1)], "kcal"), 
                 "Calories", icon = icon("fire"), color = "yellow")
      })
      
      #  plot ingredients and nutrition table
      output$ing_df <- DT::renderDataTable(change_df(),
                                           colnames = c('Serving',"Quantity", "Food_name", "Category"),
                                           rownames=F, options = list(pageLength = 5))
      
      # plot the bar chart
      output$Servings_plot <- renderPlotly(
        {
          temp_Serving =sum_service()%>% select(type, Ingredent_number)
          colnames(temp_Serving) = c('Food','Serving')
          
          # plot bar chart
          if(is.null(input$radio)){
            Male_df = recommend_service %>% filter(Type =='Male')
            Female_df = recommend_service %>% filter(Type =='Female')
            
            
            # plot the bar_plot
            p = ggplot() +
              geom_line(data = Female_df, aes(x = Food, y = Serving,group = 1,colour="Female"),size=2) + 
              geom_point(data =Female_df, aes(x = Food, y = Serving,group = 1))+
              geom_line(data = Male_df, aes(x = Food, y = Serving,group = 1,colour="Male"),size=2) + 
              geom_point(data =Male_df, aes(x = Food, y = Serving,group = 1))+
              scale_colour_manual("", 
                                  breaks = c("Female", "Male"),
                                  values = c("red", "green"))+
              labs(title = "The recommende Servings for Male and Female")+
              theme(plot.title=element_text(hjust=0.5))
          } 
          else if (input$radio == 'Male')
          {
            Male_df = recommend_service %>% filter(Type =='Male')
            print(temp_Serving)
            
            # plot the bar_plot
            p = ggplot() +
              geom_line(data = Male_df, aes(x = Food, y = Serving,group = 1),color="Green",size=2) +
              geom_point(data =Male_df, aes(x = Food, y = Serving,group = 1))+
              geom_bar(data = temp_Serving, aes(x = Food, y = Serving,fill = Food,color=Food),stat="identity",alpha=0.2)+
              labs(title = 'The services you have eat(Male)')+
              theme(plot.title=element_text(hjust=0.5))
          }
          else 
          {
            Female_df = recommend_service %>% filter(Type =='Female')
            # plot the bar_plot
            p = ggplot() +
              geom_line(data = Female_df, aes(x = Food, y = Serving,group = 1),color="red",size=2) +
              geom_point(data =Female_df, aes(x = Food, y = Serving,group = 1))+
              geom_bar(data = temp_Serving, aes(x = Food, y = Serving,fill = Food,color=Food),stat="identity",alpha=0.2)+
              labs(title = 'The services you have eat(Female)')+
              theme(plot.title=element_text(hjust=0.5))
          }
          
          ggplotly(p, tooltip = c('Serving'))
        }
      )
      
}

# Run the application 
shinyApp(ui = ui, server = server)

