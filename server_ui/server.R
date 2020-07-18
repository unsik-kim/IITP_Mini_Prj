library(shiny)
library(tidyverse)
library(sf)

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
load(file="C:/Users/admin/Documents/R/data.RData")
load(file="C:/Users/admin/Documents/R/emd_dd.RData")
load(file="C:/Users/admin/Documents/R/emd_nn.RData")
load(file="C:/Users/admin/Documents/R/rader.RData")
load(file="C:/Users/admin/Documents/R/LOCAL_PEOPLE_DONG")
load(file="C:/Users/admin/Documents/R/dong_ip.RData")
 
r = read.csv("C:/Users/admin/Documents/R/rader.csv")
rader <- r
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
              ) %>%
  addCircles(data=data2, lng=~경도, lat=~위도, label=~(상호명))

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
  
  #date_time_grouping <- population %>%
  #filter(day=="화")%>%
  #group_by(Timeline) %>%
  #summarise(Date_Time_mean = mean(Total))
  #실행
  
  
  #시계열 그래프 그리기

  return(date_time_grouping)
}

shinyServer(function(input, output) {
  
  
  
  
  #output$value <- renderPrint({ input$action })

  #m%>%setView(lng=127.0865, lat=38.51033, zoom=15)


  # 지도 클릭시 이벤트
  observeEvent(input$map_shape_click,{
    click <- input$map_shape_click
    if(!is.null(click$id)){
    print(click$lng)
    print(click$lat)
    print(click$id)
    output$map <- renderLeaflet({
      m%>%
        setView(lng=click$lng, lat=click$lat, zoom=14)%>%
        addPolygons(data = emd_nn%>%filter(adm_dr_nm==click$id), 
                    fillColor = "red", 
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = T, 
                    weight = 1, 
                    group = "regions")%>%
        addCircles(data=data2%>%filter(행정동명==click$id), lng=~경도, lat=~위도,color="white", label=~(상호명))
    }
    )
    
    #rader 차트 출력
    output$rader <- renderPlot({
      p<-raderchart(click$id)
      ggradar(p)+ theme(legend.position = "top")
      
    })
    #rader 테이블 출력
    output$rader_t <- renderTable({
      p<-radertable(click$id)
    })
    #편의점 정보 출력
    output$store_info <- renderDataTable({
      data2 %>% filter(행정동명==click$id) %>%
        as.data.frame(matrix(rnorm(100),5,5))
    },
    
    options = list(
      scrollY = 200,
      pageLength=5)
    )
    #시간대별 생활인구 출력
    output$time <- renderPlotly({
      code <- get_code(click$id)
      date_time<-time_popul(code)
      popul_plot <- ggplot(data=date_time, aes(x =시, y =  생활인구, colour=day)) +
        geom_line() +
        geom_point(size=2, shape=1) +
        coord_cartesian(xlim=c(1,24), expand = FALSE)
      ggplotly(popul_plot)
    })
  }})
  
  # select 선택시 이벤트
  observeEvent(input$select1,{
    if(!is.null(input$select1)){
    lon <- switch (input$select1,
                   "사직동" = 127.0825,
      "잠실본동" = 127.0825,
      "명동" = 126.9838,
      "여의동" = 126.9268,
    )
    lat <- switch (input$select1,
                   "사직동" = 37.50573,
                   "잠실본동" = 37.50573,
                   "명동" = 37.56275,
                   "여의동" = 37.5268,
    )

    #leaflet 지도 현재 위치 표시하기      
    output$map <- renderLeaflet({
      m%>%setView(lng=lon, lat=lat, zoom=14)%>%
        addPolygons(data = emd_nn%>%filter(adm_dr_nm==input$select1), 
                    fillColor = "red", 
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = T, 
                    weight = 1, 
                    group = "regions")%>%
        addCircles(data=data2%>%filter(행정동명==input$select1), lng=~경도, lat=~위도,color="white", label=~(상호명))
    }
    )
    
    #rader 차트 출력
    output$rader <- renderPlot({
      p<-raderchart(input$select1)
      ggradar(p)+ theme(legend.position = "top")
      
    })
    #rader 테이블 출력
    output$rader_t <- renderTable({
      p<-radertable(input$select1)
    })
    #편의점 정보 출력
    output$store_info <- renderDataTable({
      data2 %>% filter(행정동명==input$select1) %>%
        as.data.frame(matrix(rnorm(100),5,5))
    },
    
    options = list(
                   scrollY = 200,
                   pageLength=5)
    )
    #시간대별 생활인구 출력
    output$time <- renderPlotly({
      code <- get_code(input$select1)
      date_time<-time_popul(code)
      popul_plot <- ggplot(data=date_time, aes(x =시, y =  생활인구, colour=day)) +
        geom_line() +
        geom_point(size=2, shape=1) +
        coord_cartesian(xlim=c(1,24), expand = FALSE)
      ggplotly(popul_plot)
    })
    
    #연령대별 인구 출력
    output$age <- renderPlotly({
      code <- get_code(input$select1)
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
    
  }})
})