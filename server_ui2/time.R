library(ggplot2)
library("lubridate")
library(dplyr)
library(ggplotly)
library("MASS")
library("scales")
load("C:/Users/admin/Documents/GitHub/IITP_Mini_Prj/server_ui2/LOCAL_PEOPLE_DONG")

time_popul <- function(code){
#동 데이터 추출
population <- LOCAL_PEOPLE_DONG %>% filter(행정동코드 == 11710641)

#변수이름 영어로 변경

colnames(population) <- c("DateID", "Timeline","code", "Total")



#날짜,시간대,행정동코드, 생활인구수만 추출

population <- population[1:4]


#요일추출
population$newDate <- ymd(population$DateID)


population$day <- wday(population$newDate)




#요일중 화,금,토만 추출
population <- population %>% filter(day %in% c("화", "금", "토"))
population



#일별, 시간대별 유동인구수
library(data.table)
data <- data.table(population)
data[,.(Date_Time_mean = mean(Total)), .(day,Timeline)]

date_time_grouping <- data.frame(data[,.(Date_Time_mean = mean(Total)), .(day,Timeline)])

#date_time_grouping <- population %>%
  #filter(day=="화")%>%
  #group_by(Timeline) %>%
  #summarise(Date_Time_mean = mean(Total))
#실행


#시계열 그래프 그리기
popul_plot <- ggplot(data=date_time_grouping, aes(x =Timeline, y =  Date_Time_mean, colour=day)) +
  geom_line() +
  geom_point(size=2, shape=1) +
  coord_cartesian(xlim=c(1,24), expand = FALSE)
return(popul_plot)
}
popul_plot <- time_popul(11710641)
ggplotly(popul_plot)

