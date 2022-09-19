# import packages
library(shinythemes)
library(plotrix)
library(shiny)
library(dplyr)
library(DT)
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
     answer = paste('select * from new_nuitiion where Name =',food_name,seq='')
     
     return (answer)
 }
 
 # collect all the name of food
 result = dbSendQuery(mydb, "select Name from new_nuitiion")
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
                         numericInput('quantity', 'Quantity', value = 1, min = 0, step = 1)),
        # some action button
        actionButton("add", "Add ingredient"),
        actionButton("remove", "Remove ingredient"),
        numericInput("serving", "Number of servings contained", min = 0.01, step = 1, value = 1)
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
        valueBoxOutput("fat",width = 4),
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
                   DT::DTOutput("ing_df")
          ),
          tabPanel("Nutrition Table",
                   # box(title = "Nutrition Table",
                   #     status = "warning",
                   #     solidHeader = T,
                   #     width = 8,
                   #     collapsible = T,
                   #     collapsed = F,
                   #     tags$p(textOutput("serving", inline = T)),
                   #     div(DT::DTOutput("nutrient_table"), style = "font-size: 100%;")))
            DT::DTOutput("nutrient_table")
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
            background = 'maroon', width = 13, collapsible = T,
            plotlyOutput('Servings_plot')
            )
      ),
      
      fluidRow(
            box(title = "Nutrient distribution", solidHeader = T,
                background = "light-blue",
                width = 13, collapsible = T,
                plotlyOutput("macro_plot"))
        )
 )
 #skin = 'black'
)

server <- function(input, output, session){
    # make reactive to store ingredients
    ing_df <- shiny::reactiveValues()
     ing_df$df <- data.frame("quantity" = numeric(),
                             "Measurement" = character(),
                             "Name" = character(),
                             "Category" = character(),
                             stringsAsFactors = F)
     ing_df$measure <- data.frame(
                                  "Name" = character(),
                                  "KJ_Per_mea"=numeric(),
                                  "Protein" = numeric(),
                                  "Fat" = numeric(),
                                  "Sat.Fat" = numeric(),
                                  "Fiber" = numeric(),
                                  "Carbs" = numeric(),
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
          # 鏈夎叮鐨勯棶棰?
          nutrition_df
      }
      )


     # step 2 update the measure unit for singular ingredient
      observe({
          units <- unique(paste(nutrition_df()$Measurement ))
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
          isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$quantity,
                                                       input$measure_unit,
                                                      input$food,
                                                      temp
                                                      ))
         # get actual working ingredient dataframe for dplyr
          input_measure <- nutrition_df()[,c(2,4,5,6,7,8,9)]
          isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
          #print(typeof(ing_df$measure))
          # update choices
          updateNumericInput(session, 'quantity', 'Quantity', 1)
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
            KJ = as.numeric(KJ_Per_mea) * as.numeric(quantity) / input$serving,
            Protein = as.numeric(Protein) * as.numeric(quantity) / input$serving,
            Fat = as.numeric(Fat) * as.numeric(quantity)/input$serving,
            Sat.Fat = as.numeric(Sat.Fat)* as.numeric(quantity)/input$serving,
            Fiber = as.numeric(Fiber) * as.numeric(quantity)/input$serving,
            Carbs = as.numeric(Carbs) * as.numeric(quantity)/input$serving
          )
        
        # add all the nutrition together
        nutrition = mapply(sum, measure_food_df[,-c(1,2,8)])
        new_df <-  matrix(nutrition,nrow=1,ncol=6,byrow=TRUE)
        colnames(new_df) <- c("Protein", "Fat", "Sat.Fat", "Fiber", "Carbs","KJ")
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
      
      
      # output the value
      # plot the KJ
      # value boxes
      output$calories <- renderValueBox({
        valueBox(paste0(sum_nutrition()[,c(6)], "kcal"), 
                 "Calories", icon = icon("fire"), color = "yellow")
      })
      
      output$fat <- renderValueBox({
        valueBox(paste0(sum_nutrition()[,c(2)], "g"), 
                 "Fat", icon = icon("exclamation-triangle"), color = "red")
      })
      
      #  plot ingredients and nutrition table
      output$ing_df <- DT::renderDataTable(ing_df$df,
                                            colnames = c("Quantity", "Units", "Ingredient"),
                                           rownames=F, options = list(pageLength = 5))
      output$nutrient_table <- DT::renderDataTable(sum_nutrition())
      
      # plot the histogram for all the nutrition---> macro_plot
      output$macro_plot <- renderPlotly({
        macro_name =  c("Protein", "Fat", "Sat.Fat", "Fiber",  "Carbs")
        macro_ingredient <- sum_nutrition()[,-c(6)]
        macro_df = cbind.data.frame(macro_name,macro_ingredient)
      
        # use ggplot
        macro_plots = ggplot(macro_df,aes(macro_name,macro_ingredient))+
          geom_bar(stat = "identity",fill =c("#9933FF",
                                             "#33FFFF",
                                             "red",
                                             "darkblue",
                                             'pink'))+
          labs(title = "Nutrition ratio")+
          theme(plot.title=element_text(hjust=0.5))
        macro_plots
      })
      
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

