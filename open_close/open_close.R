install.packages("shiny")
install.packages("ggplot2")
install.packages("readxl")
install.packages("tidyverse")
install.packages("gapminder")
library(shiny)
library(ggplot2)
library(readxl)
library("dplyr")
library("plotly")
library(tidyverse)
library(gapminder)

# 행정코드 11140550 명동, 11560540 여의동, 11710650 잠실본동
region_code <- 11560540

# 종목
store_name <- "편의점"


# 데이터 합산
oriData <- union(data.frame("종목"="편의점",read_excel("서울시_편의점_폐업률.xlsx")), data.frame("종목"="치킨집",read_excel("서울시_치킨집_폐업률.xlsx")))
oriData <- union(oriData, data.frame("종목"="카페",read_excel("서울시_카페_폐업률.xlsx")))
oriData <- union(oriData, data.frame("종목"="제과점",read_excel("서울시_제과점_폐업률.xlsx")))
oriData <- union(oriData, data.frame("종목"="분식집",read_excel("서울시_분식집_폐업률.xlsx")))



# 이름바꾸기
names(oriData)[5] <- c("2018개업수")
names(oriData)[6] <- c("2018폐업수")
names(oriData)[7] <- c("2018개업률")
names(oriData)[8] <- c("2018폐업률")
names(oriData)[9] <- c("2019개업수")
names(oriData)[10] <- c("2019폐업수")
names(oriData)[11] <- c("2019개업률")
names(oriData)[12] <- c("2019폐업률")
names(oriData)[13] <- c("2020개업수")
names(oriData)[14] <- c("2020폐업수")
names(oriData)[15] <- c("2020개업률")
names(oriData)[16] <- c("2020폐업률")


# 그래프 변환용 데이터프레임 생성
per_data1 <- data.frame(t(oriData %>% filter(CD==11&종목==store_name) %>% select("2018개업률", "2018폐업률", "2019개업률", "2019폐업률", "2020개업률", "2020폐업률")))
names(per_data1)[1] <- c("수치")
per_data2 <- data.frame(t(oriData %>% filter(CD==region_code&종목==store_name) %>% select("2018개업률", "2018폐업률", "2019개업률", "2019폐업률", "2020개업률", "2020폐업률")))
names(per_data2)[1] <- c("수치")
per_data <- data.frame("년도"=c("2018-서울평균", "2018-서울평균", "2018", "2018", "2019-서울평균", "2019-서울평균", "2019", "2019", "2020-서울평균", "2020-서울평균", "2020", "2020"),
                       "개폐업률"=c("평균개업률", "평균폐업률", "지역개업률", "지역폐업률", "평균개업률", "평균폐업률", "지역개업률", "지역폐업률", "평균개업률", "평균폐업률", "지역개업률", "지역폐업률"), 
                       "수치"=c(per_data1[1:2,1], per_data2[1:2,1],per_data1[3:4,1], per_data2[3:4,1],per_data1[5:6,1], per_data2[5:6,1]))


# 개폐업률
data_plot <- ggplot(data=per_data, aes(x=년도, y= 수치, fill = 개폐업률)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("년도별 개업률 및 폐업률") +
  theme_bw()
fig3 <- ggplotly(data_plot)
fig3
