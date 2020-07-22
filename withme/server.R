library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(ggplot2)
library(sp)
library(rgdal)
library(reshape2)
library(plyr)
library(ggradar)
library(dplyr)
library(leaflet)
library("lubridate")
library("MASS")
library("scales")
library(data.table)
library(shinyjs)
load(file="C:/Users/admin/Documents/R/data.RData")
load(file="C:/Users/admin/Documents/R/emd_dd.RData")
load(file="C:/Users/admin/Documents/R/emd_nn.RData")
load(file="C:/Users/admin/Documents/R/rader.RData")
load(file="C:/Users/admin/Documents/R/LOCAL_PEOPLE_DONG")
load(file="C:/Users/admin/Documents/R/dong_ip.RData")
load(file="C:/Users/admin/Documents/R/seoul2.RData")
#load(file="C:/Users/admin/Documents/R/crdentials.RData")

load(file = "C:/Users/admin/Documents/R/open_close_data.RData")

credit_pass = FALSE

#leflet 메인 지도 편의점 좌표 입력
m<-leaflet()%>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = emd_nn, 
              fillColor = "white", 
              fillOpacity = 0, 
              color = "black",
              stroke = T, 
              weight = 1, 
              opacity = 1,
              group = "regions",
              layerId = ~adm_dr_nm
  )


#rader 차트와 같이 나올 테이블
radertable<-function(name){
  rader <- rader%>%filter(행정동명==name,평임대료!="")
  min_max <- data.frame(행정동명=c("서울평균"),
                            평임대료=c(7.8),
                            점포수=c(22.31),
                            평균인구수=c(25000),
                            최대인구수=c(31375),
                            인구수ㅣ점포수=c(1120))
  rader<-rbind(rader,min_max)
  return(rader)
}
#rader 차트  -- 평임대료 점포수 평균인구 최대인구 인구수/점포수
raderchart <- function(name){
  rader <- rader%>%filter(행정동명==name,평임대료!="")
  
  min_max <- data.frame(행정동명=c("서울평균","max","min"),
                            평임대료=c(7.8,40,0),
                            점포수=c(22.31,100,0),
                            평균인구수=c(25000,200000,0),
                            최대인구수=c(31375,200000,0),
                            인구수ㅣ점포수=c(1120,8000,0))
  
  rader<-rbind(rader,min_max)
  
  #rescale 
  rescale_df=function(data1,groupvar=NULL){
    if(is.null(groupvar)) df=data1
    else df=data1[,-which(names(data1) %in% groupvar)]
    
    select=sapply(df,is.numeric)
    df[select]=lapply(df[select], scales::rescale)
    if(!is.null(groupvar)) {
      df=cbind(df,data1[[groupvar]])
      colnames(df)[length(df)]=groupvar
    }        
    df
  }
  
  rescaled=rescale_df(rader)
  rescaled <- rescaled %>% filter(행정동명!="max",행정동명!="min")
  return(rescaled)
}
#행정동명으로 행정동코드 받아오기
get_code <- function(name){
  d <- dong_ip%>%filter(행정동명==name)
  d<-d[1,1]
  a<-as.numeric(d)
  return(a)
}

#연령대별 인구
pie_chart <- function(code){
  #행정동 선택
  md_age <- LOCAL_PEOPLE_DONG%>% filter(행정동코드 == code)
  
  #연령대 묶어주기
  md_age$md_underteen <- md_age$underteen + md_age$fe_underteen
  md_age$md_teen <- md_age$teen_1 + md_age$teen_2 + md_age$teen_3 + md_age$teen_4
  md_age$md_twenty <- md_age$twenty_1 + md_age$twenty_2 + md_age$twenty_3 + md_age$twenty_4
  md_age$md_thirty <- md_age$thirty_1 + md_age$thirty_2 + md_age$thirty_3 + md_age$thirty_4
  md_age$md_fourty <- md_age$fourty_1 + md_age$fourty_2 + md_age$fourty_3 + md_age$fourty_4
  md_age$md_fifty <- md_age$fifty_1 + md_age$fifty_2 + md_age$fifty_3 + md_age$fifty_4
  md_age$md_sixty <- md_age$sixty_1 + md_age$sixty_2 + md_age$seventies +md_age$sixty_3 + md_age$sixty_4 + md_age$fe_seventies
  
  
  #e데이터 연령별 추출
  md_age1 <- md_age[1:4]
  md_age2 <- md_age[34:40]
  md_age <- cbind(md_age1, md_age2)
  
  df_md_age <- data.frame(
    underteen = mean(md_age$md_underteen),
    teens= mean(md_age$md_teen),
    twenties = mean(md_age$md_twenty),
    thirties = mean(md_age$md_thirty),
    fourties = mean(md_age$md_fourty),
    fifties = mean(md_age$md_fifty),
    sixties = mean(md_age$md_sixty))
  
  df_md_age
  
  return(df_md_age)
  
}

#시간대별 유동인구
time_popul <- function(code_num){
  #동 데이터 추출
  code = code_num
  population <- LOCAL_PEOPLE_DONG %>% filter(행정동코드==code)
  
  #변수이름 영어로 변경
  
  colnames(population) <- c("DateID", "시","code", "Total")
  
  
  
  #날짜,시간대,행정동코드, 생활인구수만 추출
  
  population <- population[1:4]
  
  
  #요일추출
  population$newDate <- ymd(population$DateID)
  
  
  population$day <- wday(population$newDate)
  
  
  #요일중 화,금,토만 추출
  population <- population %>% filter(day %in% c(3, 6, 7))
  population$day <- as.character(population$day)
  population$day[population$day==3] <- as.character("화")
  population$day[population$day==6] <- as.character("금")
  population$day[population$day==7] <- as.character("토")
  
  population
  
  #일별, 시간대별 유동인구수
  
  data <- data.table(population)
  
  date_time_grouping <- data.frame(data[,.(생활인구 = mean(Total)), .(day,시)])
  
  
  return(date_time_grouping)
}

#상권 랭킹
rank_chart<- function(code){
  
  md_shop <- seoul2 %>%filter(행정동코드 == code)
  md_shop <- md_shop %>% filter(상권업종소분류명%in%c("커피전문점/카페/다방","편의점","라면김밥분식","후라이드/양념치킨","제과점"))
  numOfGroup_md <- data.frame(table(md_shop$상권업종소분류명))
  
  return(numOfGroup_md)
}

#개업/폐업률
open_close_chart <- function(category,code){
  per_data1 <- data.frame(t(oriData %>% 
                              filter(CD==11&종목==category) %>% 
                              select("2018개업률", "2018폐업률", "2019개업률", "2019폐업률", "2020개업률", "2020폐업률")))
  names(per_data1)[1] <- c("수치")
  
  per_data2 <- data.frame(t(oriData %>% 
                              filter(CD==code&종목==category) %>% 
                              select("2018개업률", "2018폐업률", "2019개업률", "2019폐업률", "2020개업률", "2020폐업률")))
  names(per_data2)[1] <- c("수치")
  
  per_data <- data.frame("년도"=c("2018","2019","2020"),
                         "개폐업률"=c("평균개업률", "평균폐업률", "지역개업률", "지역폐업률", "평균개업률", "평균폐업률", "지역개업률", "지역폐업률", "평균개업률", "평균폐업률", "지역개업률", "지역폐업률"), 
                         "수치"=c(per_data1[1:2,1], per_data2[1:2,1],per_data1[3:4,1], per_data2[3:4,1],per_data1[5:6,1], per_data2[5:6,1]))
  
  return(per_data)
}

credentials <- read.csv("C:/Users/admin/Documents/R/crdentials.csv")

#서버 시작

server <- shinyServer(function(input, output) {
  
  # user_Data <- reactivePoll(1000, NULL,
  #                           # This function returns the time that log_file was last modified
  #                           checkFunc = function() {
  #                             
  #                           },
  #                           # This function returns the content of log_file
  #                           valueFunc = function() {
  #                             load(file="C:/Users/admin/Documents/R/crdentials.RData")
  #                           }
  # )
  
  user_Data <- reactiveFileReader(100,NULL,"C:/Users/admin/Documents/R/crdentials.csv",read.csv)
  
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  output$auth_output <- renderPrint({
    if(is.null(res_auth))
      print(null)
    else
      reactiveValuesToList(res_auth)
  })
  
  output$map <- renderLeaflet({
    m
  })
  
  
  # # 지도 클릭시 이벤트
  # observeEvent(input$map_shape_click,{
  #   click <- input$map_shape_click
  #   if(!is.null(click$id)){
  #   print(click$lng)
  #   print(click$lat)
  #   print(click$id)
  #   output$map <- renderLeaflet({
  #     m%>%
  #       setView(lng=click$lng, lat=click$lat, zoom=14)%>%
  #       addPolygons(data = emd_nn%>%filter(adm_dr_nm==click$id), 
  #                   fillColor = "red", 
  #                   fillOpacity = 0.5,
  #                   color = "black",
  #                   stroke = T, 
  #                   weight = 1, 
  #                   group = "regions")%>%
  #       addCircles(data=data2%>%filter(행정동명==click$id), lng=~경도, lat=~위도,color="white", label=~(상호명))
  #   }
  #   )
  #   
  #   #rader 차트 출력
  #   output$rader <- renderPlot({
  #     p<-raderchart(click$id)
  #     ggradar(p)+ theme(legend.position = "top")
  #     
  #   })
  #   #rader 테이블 출력
  #   output$rader_t <- renderTable({
  #     p<-radertable(click$id)
  #   })
  #   #편의점 정보 출력
  #   output$store_info <- renderDataTable({
  #     data2 %>% filter(행정동명==click$id) %>%
  #       as.data.frame(matrix(rnorm(100),5,5))
  #   },
  #   
  #   options = list(
  #     scrollY = 200,
  #     pageLength=5)
  #   )
  #   #시간대별 생활인구 출력
  #   output$time <- renderPlotly({
  #     code <- get_code(click$id)
  #     date_time<-time_popul(code)
  #     popul_plot <- ggplot(data=date_time, aes(x =시, y =  생활인구, colour=day)) +
  #       geom_line() +
  #       geom_point(size=2, shape=1) +
  #       coord_cartesian(xlim=c(1,24), expand = FALSE)
  #     ggplotly(popul_plot)
  #   })
  #   
  #   #연령대별 인구수
  #   output$age <- renderPlotly({
  #     code <- get_code(click$id)
  #     df<-pie_chart(code)
  #     df <- t(df)
  #     df <- data.frame(df)
  #     name_df <- c("10대 이하", "10대", "20대", "30대", "40대", "50대", "60대 이상")
  #     name_df<-data.frame(name_df)
  #     df <- cbind(name_df,df)
  #     
  #     df<-df[order(df$df),]
  #     title2 <-as.character(df[7,1])
  #     title1=paste(title2,c("가 가장 많습니다."))
  #     
  #     plot_ly(df,labels=~name_df,values=~df)  %>% 
  #       add_pie(hole = 0.5)%>%
  #       layout(title=title1,
  #              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #     
  #   })
  # }})
  # 검색 이벤트
  observeEvent(input$search,{
    dong_code <- input$select1
    print(dong_code)
    category <- input$select_category
    lon <- switch (dong_code,
                   "사직동" = 127.0825,
                   "잠실본동" = 127.0825,
                   "명동" = 126.9838,
                   "여의동" = 126.9268,
    )
    lat <- switch (dong_code,
                   "사직동" = 37.50573,
                   "잠실본동" = 37.50573,
                   "명동" = 37.56275,
                   "여의동" = 37.5268,
    )
    
    #leaflet 지도 현재 위치 표시하기      
    output$map <- renderLeaflet({
      m%>%setView(lng=lon, lat=lat, zoom=14)%>%
        addPolygons(data = emd_nn%>%filter(adm_dr_nm==dong_code), 
                    fillColor = "red", 
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = T, 
                    weight = 1, 
                    group = "regions")%>%
        addCircles(data=seoul2%>%filter(상권업종소분류명==category), lng=~경도, lat=~위도, label=~(상호명))
    }
    )
    
    
    #rader 차트 출력
    output$rader <- renderPlot({
      p<-raderchart(dong_code)
      ggradar(p)+ theme(legend.position = "top")
      
    })
    #rader 테이블 출력
    output$rader_t <- renderTable({
      p<-radertable(dong_code)
    })
    #상가 정보 출력
    output$store_info <- renderDataTable({
      seoul2 %>% filter(상권업종소분류명==category,행정동명==dong_code) %>%
        select(상호명,지점명)%>%
        as.data.frame(matrix(rnorm(100),5,5))
    },
    
    options = list(
      scrollY = 200,
      pageLength=5)
    )
    #시간대별 생활인구 출력
    output$time <- renderPlotly({
      code <- get_code(dong_code)
      date_time<-time_popul(code)
      popul_plot <- ggplot(data=date_time, aes(x =시, y =  생활인구, colour=day)) +
        geom_line() +
        geom_point(size=2, shape=1) +
        coord_cartesian(xlim=c(1,24), expand = FALSE)+theme_bw()
      ggplotly(popul_plot) %>% layout(legend = list(orientation = "h", x = 0., y = -0.2))
    })
    
    #연령대별 인구 출력
    output$age <- renderPlotly({
      code <- get_code(dong_code)
      df<-pie_chart(code)
      df <- t(df)
      df <- data.frame(df)
      name_df <- c("10대 이하", "10대", "20대", "30대", "40대", "50대", "60대 이상")
      name_df<-data.frame(name_df)
      df <- cbind(name_df,df)
      
      df<-df[order(df$df),]
      title2 <-as.character(df[7,1])
      title1=paste(title2,c("가 가장 많습니다."))
      
      plot_ly(df,labels=~name_df,values=~df)  %>% 
        add_pie(hole = 0.5)%>%
        layout(title=title1,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
    #상권 랭킹
    output$rank <- renderPlotly({
      code <- get_code(dong_code)
      dd <- rank_chart(code)
      d<-ggplot(data = dd, aes(x=Var1, y=Freq, fill=Var1)) + geom_col()+theme_bw()
      
      ggplotly(d)%>% layout(showlegend = FALSE)
    })
    
    #개업 폐업
    output$open_close <- renderPlotly({
      code <- get_code(dong_code)
      md <- open_close_chart(category,code)
      md_convi_plot <- ggplot(data=md, aes(x=년도, y= 수치, fill = 개폐업률)) +
        geom_bar(stat="identity", position=position_dodge()) + ggtitle("년도별 개업률 및 폐업률") +
        geom_text(aes(label=수치),position = position_dodge(0.9))+
        theme_bw()
      fig <- ggplotly(md_convi_plot)
      fig
    })
    
  })
  useShinyjs()
  showElement(id="login_form")
  hideElement(id="logout_form")
  hideElement(id="data_chart")
  
  observe({
    #credentials<-as.data.frame(user_Data())
    
  })
  observeEvent(input$login,{
    credentials<-data.frame(user_Data())
    credentials <- credentials %>% dplyr::select(user,password)
    #credentials=data.frame(crede())
    print(credentials)
    if(input$user_id == ""){
      showNotification("아이디를 입력하시오")
    }
    else if(input$user_pw == ""){
      showNotification("비밀번호를 입력하시오")
    }
    else{
      check_id <- credentials %>% filter(user== input$user_id)
      print(credentials)
      if(nrow(check_id)!=0){
        if(check_id[2] == input$user_pw){
          output$login_meg <- renderText({
            i<-paste(c("환영합니다. ",input$user_id,"님"))
            i
          })
          hideElement(id="login_form")
          showElement(id="logout_form")
          showElement(id="data_chart")
          hideElement(id="submit_form")
        }
        else
        {
          showNotification("회원정보가 일치하지 않습니다.")
        }
      }
      else{
        showNotification("회원정보가 일치하지 않습니다.")
      }
    }
    
    
  }
  )
  
  
  observeEvent(input$submit,{
    id=input$submit_id
    pw=input$submit_pw
    credentials<-data.frame(user_Data())
    credentials <- credentials %>% dplyr::select(user,password)
    if(id == ""){
      showNotification("아이디를 입력하시오")
    }
    else if(nrow(credentials %>% filter(user== input$submit_id)) != 0){
      showNotification("같은 아이디가 존재합니다. 다시입력해 주세요")
    }
    else if(pw == ""){
      showNotification("비밀번호를 입력하시오")
    }
    else if(input$submit_pw_cf != input$submit_pw){
      showNotification("비밀번호가 일치하지 않습니다.")
    }
    else{
      showNotification("회원가입 성공.")
      print(crede())
      credentials<-data.frame(crede())
      print(credentials)
      
      write.csv(credentials,"C:/Users/admin/Documents/R/crdentials.csv")
      credentials<-data.frame(user_Data())
      credentials <- credentials %>% dplyr::select(user,password)
      print(credentials)
      credit_pass =TRUE
      
      
    }
    
    
  })
  crede<-eventReactive(input$submit,{
    credentials<-data.frame(user_Data())
    credentials <- credentials %>% dplyr::select(user,password)
    new_user <- data.frame(
      user = c(input$submit_id),
      password = c(input$submit_pw)
      
    )
    credentials <- rbind(credentials,new_user)
    credentials
    
    
  })
  
  observeEvent(input$logout,{
    showElement(id="login_form")
    hideElement(id="logout_form")
    hideElement(id="data_chart")
    showElement(id="submit_form")
    
  })
  
})

