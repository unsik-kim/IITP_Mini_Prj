library(shiny)
library(shinymanager)

# credentials <- data.frame(
#   user = c("shiny", "shinymanager"), # mandatory
#   password = c("1234", "12345") # mandatory
# )



#dong <- as.list(dong_ip[2])

ui <- (fluidPage(
  tags$head(
    tags$style(HTML('#search{text-align:left}'))
  ),
  
  #verbatimTextOutput("auth_output"),

  titlePanel("상권분석"),
  tags$br(),

  
  sidebarLayout(
    
    mainPanel(style = "max-height: 900px",
      
      leafletOutput("map",height=900),
      width = 8
    ),
    sidebarPanel(useShinyjs(),

        wellPanel(id="login_form",
                  fluidRow(
                           column(4,textInput("user_id","",width = 150,label = "아이디")),
                           
                           column(4,passwordInput("user_pw","",width = 150,label = "비밀번호"))),

                  actionButton("login",label = "로그인"),
                  actionButton("submit_show",label = "회원가입")
        ),

        
        conditionalPanel(condition = "input.submit_show % 2 == 1",
                  
                  wellPanel(id="submit_form",
                  "아이디",textInput("submit_id","",width = 150),
                  "비밀번호",passwordInput("submit_pw","",width = 150),
                  "비밀번호 확인 : ",passwordInput("submit_pw_cf","",width = 150),
                  actionButton("submit",label = "회원가입")
                  )),
        wellPanel(id="logout_form",
                  fluidRow(
                    column(6,textOutput("login_meg"),
                           ),
                    column(6,actionButton("logout",label = "로그아웃")
                           ),
                  )
                  
                  ),
        fluidRow(column(5,
      selectInput("select_category", label = h3("분야 선택"), 
                    choices = list("커피전문점/카페/다방","편의점","라면김밥분식","후라이드/양념치킨","제과점"), 
                    selected = "편의점",width = 200)),
      
        column(5,
        selectInput("select1", label = h3("동 선택"), 
                  choices = list("잠실본동", "명동", "여의동"), 
                  selected = "잠실본동",width = 150)),
        column(2,
               br(),br(),br(),
        actionButton("search",label = "검색",style="align-item:center;")
      )),
      width=4,

    
      # Show a plot of the generated distribution

    
 
      tags$br(),
      wellPanel(id="data_chart",
                style = "overflow-y:scroll; max-height: 600px",
      
                h4("상권 정보"),
                dataTableOutput("store_info"),
                
                plotOutput("rader"),
                tableOutput("rader_t"),
                
                h4("개업/폐업률"),
                plotlyOutput("open_close"),
                
                h4("시간대별 생활인구"),
                plotlyOutput("time"),
                
                h4("연령별 인구"),
                plotlyOutput("age"),
                
                h4("상권랭킹"),
                plotlyOutput("rank"),
      
      
      )
      
      

  ),

  ),
  
  
 
  
  
))


#ui <- secure_app(ui)