library(shiny)
dong <- as.list(dong_ip[2])

shinyUI(fluidPage(
  tags$br(),
  titlePanel("편의점 상권분석"),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  sidebarLayout(
  
    sidebarPanel(
      selectInput("select1", label = h3("동 선택"), 
                  choices = list("잠실본동", "명동", "여의동"), 
                  selected = "잠실본동"),
      textOutput("dong"),
      width=3,
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(

      leafletOutput("map",height=600),
      width = 9
    )
    
  ),
  tags$br(),
    sidebarPanel(
      plotOutput("rader"),
      tableOutput("rader_t"),
      width = 4,
    ),
    mainPanel(
      plotlyOutput("age"),
      width = 4,

    ),
    mainPanel(
      
      h4("상가 정보"),
      dataTableOutput("store_info"),
      width = 4,
    ),
  tags$br(),
  mainPanel(
    width=2
            ),

  mainPanel(
    h4("시간대별 생활인구"),
    plotlyOutput("time"),
  ),


  
  
 
  
  
))

