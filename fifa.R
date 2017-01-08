library(dplyr)
library(tidyr)
library(zoo)
library(rCharts)
library(rbokeh)
library(rvest)
library(stringi)
library(RColorBrewer)
library(colorRamps)

source("scrap_meczy.R", local = TRUE, encoding = "UTF-8")
source("trenerzy.R", local = TRUE, encoding = "UTF-8")

dane_raw <- read.table("dane.txt")
colnames(dane_raw) <- c("rok", 1:12, "srednia")
dane_raw <- dane_raw[,-14]

dane <- dane_raw %>%  
  gather(miesiac, pozycja, -rok) %>%
  mutate(pozycja = as.numeric(pozycja), 
         miesiac = as.numeric(miesiac)) %>%
  arrange(rok, miesiac) %>%
  filter(!is.na(pozycja)) %>%
  mutate(date_formated = as.POSIXct(strptime(paste0(rok, "-", formatC(miesiac, width = 2, flag = "0"), "-", "01-01"), "%Y-%m-%d-%I"), tz = "GMT")) %>%
  mutate(date_ts = as.integer(date_formated)*1000) %>%
  mutate(pozycja_przod = lead(pozycja),
         data_przod = lead(date_formated))

mecze = mecze %>%
  filter(!is.na(Wynik))

mecze_final = mecze %>%
  left_join(dane, by = c("Rok" = "rok", "Miesiac" = "miesiac")) %>%
  mutate(pozycja_graph = as.numeric(Data - date_formated)/as.numeric(data_przod - date_formated)/86400 * (pozycja_przod - pozycja) + pozycja) %>%
  select(-date_formated, -date_ts) %>%
  filter(!is.na(pozycja)) %>%
  mutate(Typ = ifelse(MS, "Mistrzostwa świata", 
                      ifelse(eMS, "Eliminacje mistrzostw świata",
                             ifelse(ME, "Mistrzostwa Europy", 
                                    ifelse(eME, "Eliminacje mistrzostw Europy", "Towarzyski")))),
         Typ2 = ifelse(MS, "Mistrzostwa", 
                       ifelse(eMS, "Eliminacje",
                              ifelse(ME, "Mistrzostwa", 
                                     ifelse(eME, "Eliminacje", "Towarzyski")))),
         Rezultat = ifelse(Wygrana == 1, "Wygrana",
                           ifelse(Wygrana == 0, "Remis","Przegrana"))) %>%
  rename(Pozycja = pozycja, Data_graph = Data) %>%
  mutate(Data = as.Date(Data_graph))

plot(dane$date_formated, dane$pozycja)



# p1 <- nPlot(pozycja ~ ts,  data = dane, type = "lineWithFocusChart")
# p1$xAxis(axisLabel = 'Data', tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d ));}!#" )
# p1$yAxis(axisLabel = 'Pozycja w rankingu', tickFormat="#!function(d) {return d3.scale.linear().domain([0,-100]).range([0,100])(d)}!#")
# p1$chart(forceY = c(0,-100))
# p1$set(width = 1200, height = 700)
# 
# p1$save("rCharts1.html", standalone=TRUE)

DOT_SIZE = 5
HOVER_COLUMNS = c("Miejsce", "Gospodarz", "Gosc", "Wynik", "Typ", "Pozycja", "Data")

trenerzy_lista = unique(trenerzy$Trener)
trenerzy_kolory = primary.colors(11)

p = figure(width = 1100, height = 600 )%>%
  ly_rect(xleft = Poczatek, xright = Koniec, ybottom = -200, ytop = 200, color = Trener,
          data = trenerzy, alpha = 0.4) %>%
  ly_points(Data_graph, pozycja_graph, data = filter(mecze_final, Typ == "Towarzyski"), color = Rezultat, size = 3,
            hover = HOVER_COLUMNS) %>%
  ly_points(Data_graph, pozycja_graph, data = filter(mecze_final, stri_detect_fixed(Typ, "Eliminacje")), color = Rezultat, size = 5,
            hover = HOVER_COLUMNS) %>%
  ly_points(Data_graph, pozycja_graph, data = filter(mecze_final, stri_detect_fixed(Typ, "Mistrzostwa")), color = Rezultat, size = 6.75,
            hover = HOVER_COLUMNS) %>%
  ly_lines(date_formated, pozycja, data = dane) %>%
  set_palette(discrete_color = pal_color(c("red", "yellow", "green", trenerzy_kolory))) %>%
  y_range(c(100,1)) %>%
  x_range(c(min(mecze_final$Data_graph), max(mecze_final$Data_graph) + 200000000)) %>%
  x_axis(label = "") %>%
  y_axis(label = "Pozycja Polski w rankingu FIFA") %>%
  theme_grid(grid_line_alpha = 0) %>%
  tool_crosshair()

p



# for(i in 1:14){
#   p <- p %>%
#     ly_rect(xleft = Poczatek, xright = Koniec, ybottom = -200, ytop = 200, color = trenerzy_kolory[i],
#             data = trenerzy %>% filter(Trener == trenerzy_lista[i]), alpha = 0.5)
# }

rbokeh2html(p, file = "fifa.html", pretty = TRUE, secure = TRUE)
