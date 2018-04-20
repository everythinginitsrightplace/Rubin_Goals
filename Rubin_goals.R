library(lubridate)
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(XML)
library(data.table)
library(stringr)
library(jsonlite)
library(reshape2)
library(rvest)
library(readxl)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(highcharter)
library(ggplot2)

### Graph with season places
Rubin_Seasons <-  read_excel("season places.xlsx")
ggplot(data = Rubin_Seasons, aes(Season, Place)) +
  geom_point()+
  geom_line()+
  #ggtitle("Количество погибших в результате терактов в России") +
  labs(x = "Сезоны", 
       y = "Место в турнирной таблице")+
  ylim(16, 1)+
  theme_economist_white()

Rubin_Seasons_1 <- Rubin_Seasons %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(Place), nchar(Season)) * 7, 55), 'px">', 
                          '<b>Сезон:</b> ', Season,
                          '<br><b>Место:</b> ', Place)) %>% 
  ungroup

Rubin_chart <- hchart(Rubin_Seasons_1, 'scatter', hcaes(x = Season, y = as.integer(Place))) %>% 
  hc_add_series(data = Rubin_Seasons$Place, type = 'line') %>%
  #hc_tooltip(formatter = JS(paste0("function() {return this.point;}")), useHTML = T) %>% 
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(sample(brewer.pal(n_distinct(Rubin_Seasons$Season), 'Paired')), 'black')) %>% 
  hc_xAxis(title = list(text = 'Сезоны'), gridLineDashStyle = "Solid", labels = list(enabled = T)) %>% 
  hc_yAxis(max = 16, min = 1, tickAmount = 16,  reversed = T, opposite = F, title = list(text = 'Место в турнирной таблице')) %>% 
  hc_title(text = 'ФК "Рубин" в Российской футбольной Премьер-Лиге') %>% 
  hc_subtitle(text = 'Сезоны 2003 - 2016/2017') %>% 
  hc_add_theme(hc_theme_chalk())


######## Russian cities where Rubin won
#######
#####
####
###
##
#

Rubin_Goals <-  read_excel("Таблица с соперниками Рубина в РФПЛ.xlsx", sheet = 2)
f <- subset(Rubin_Goals, !City=="Kazan")
#Rubin_Goals_1 <- subset(Rubin_Goals, !Goals==1 & !Goals==2)
#glimpse(Rubin_Goals)
f <- f[, c(1:2,4:5,3)]
#f <- f[, c(1,3,4,2)]
names(f) <- c("Team", "City", "lat", "lon", "z")
f <- subset(f, !z ==1 & !City=="Moscow")
f <- f %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(City), nchar(z)) * 7, 55), 'px">', 
                          '<b>Город:</b> ', City,
                          '<br><b>Количество голов "Рубин":</b> ', z)) %>% 
  ungroup

f$City <- c("Пермь", "Грозный", "Екатеринбург", "Самара", "Раменское", "Ярославль", "Краснодар", "Химки")

hcmap("countries/ru/custom/ru-all-disputed", showInLegend = F) %>% 
  #hc_add_series(data = f, type = "mapbubble", maxSize = '10%', color = ifelse(f$Kazan ==1, "green", "red")) %>% 
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
  hc_colors(brewer.pal(8, 'Dark2')) %>%
  hc_add_series(data = f, type = "mapbubble", name = "Города",maxSize = '7%') %>%
  hc_mapNavigation(enabled = TRUE)  %>% 
  hc_title(text = 'В каких городах "Рубин" забил больше всего за матч в рамках РФПЛ') %>% 
  hc_subtitle(text = 'Не включены Москва (по ней отдельная карта), Оренбург (забил один гол) и Казань') %>% 
  hc_add_theme(hc_theme_chalk())