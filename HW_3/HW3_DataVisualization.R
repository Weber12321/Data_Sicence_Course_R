library(csv)
library(ggmap)
library(ggplot2)


# 本圖疊加北市人口熱量圖與公共圖書館位置點陣圖，用以了解是否人口密度多的地方也有相應的圖書館數來服務地區民眾


#load 2 files
lib <- read.csv("TaipeiLibrary.csv", header = TRUE) 
pop <- read.csv("TaipeiPop.csv", header = TRUE)

# ggplot in the map
map <- get_map(location = c(lon = 121.551484, lat = 25.054342), zoom = 12, language = "zh-TW")
newmap <- ggmap(map) + 
  #using heat map to illustrate the population of different districts  
  stat_density2d(data = pop, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon", size = 1, bins = 5) +
  scale_fill_gradient("Population") +
  scale_alpha(range = c(.3, .6), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
  # those point indicate the position of Taipei's public library
  geom_point(aes(x = lon, y = lat), data = lib)
  # not a good idea to put on labels
  # geom_text(aes(label = library), nudge_x=0, nudge_y=0, data = lib)
  
  
newmap
