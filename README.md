# Intro-Geospatial-Data
# An Introduction to Geospatial Data Analysis using R

###### 00. LOAD PACKAGES AND LIBRARIES
install.packages("mapproj")
install.packages('spData')
#install.packages("spDataLarge")
library(mapproj)
library(tidyverse)
library(maps)
library(ggrepel)
library(viridis)
library(plotly)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(dplyr)
library(raster)
library(spData)
#library(spDataLarge)

###### 1. READ KAZ MAP #######################################################
#1.1 Go here: http://www.diva-gis.org/gdata
#1.2 Select Kazakhstan, then select Administrative areas, then download
#1.3 Locate downloaded file "KAZ_adm" on your desktop 
#1.4 Specify the location and read map data as follows:
#library(sf)
mapkz <- st_read("Desktop/KAZ_adm/KAZ_adm2.shp")

#1.5 Add fill layer to mapkz
#library(tmap)
tm_shape(mapkz) + tm_fill() 
#1.6 Add county borders to mapkz shape
tm_shape(mapkz) + tm_borders() 
#1.7 Add fill and border layers to mapkz shape
tm_shape(mapkz) + tm_fill() + tm_borders() 
#1.8 Another way to do that
tm_shape(mapkz) + tm_polygons()

###### 3. PREPARE DATASET ####################################################
Sys.setlocale(locale = "Russian") # transliterator
#3.1 Load pop.xlsx
pop <- read_excel("Desktop/pop.xlsx")
#3.2 View it
#3.3 Merge map data with population data
#library(dplyr)
mappop <- merge(mapkz, pop, by = "ID_2", all = F)
#3.4 Pepare borders by county
#library(tmaptools)
states <- mappop %>% aggregate_map(by = "NAME_2")

####### 4. CHOROPLETH MAP ####################################################
#4.1 Add population layers to mapkz shape
mymap <- tm_shape(mappop) + tm_fill(col = "population")
mymap2 <- mymap + tm_shape(states) +  tm_borders(col="grey")
mymap2

#4.2 Random between (10000, 50000): Beautify your map
mymap <- tm_shape(mappop) + 
  tm_polygons(
    col = "Random",        # Select your variable
    palette = "YlGn",      # Change color of your map
    title = "Distribution of random number by county", # Add Title to your map
    breaks = c(10000, 20000, 30000, 40000, 50000), # breaks
    labels = c("Low", "Medium", "High", "Wow High"), # What do you want to put here?
    border.col = "white" #try to add this: Any difference?
    )

mymap2 <- mymap + tm_shape(states) + # Adding a layer of counties
  tm_borders(col="grey") # specifying the color of the borders
mymap2 # view your map

# find more fun here: https://rdrr.io/cran/tmap/man/tm_polygons.html

####### 5. INTERACTIVE MAP! (Wow!!!)
#5.1 Try it!
tmap_mode("view") #tmap mode set to interactive viewing
mymap2  # Isn't it fun? 
#5.2 Go back to the class =)
tmap_mode("plot") # Go back to plotting mode

### All above and more you can find here: https://geocompr.github.io/geocompkg/articles/solutions08.html

####### 5. BUBBLE PLOTS: Let's try UK now =) ########################################################
## Code is copied from here: https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2/
# Libraries

#5.1 Get the world polygon and extract UK (Would you like to try your country?)
#library(maps)
#library(ggplot2)
#library(tidyverse)
UK <- map_data("world") %>% filter(region=="UK")
Belgium <- map_data("world") %>% filter(region=="Belgium")
KZ <- map_data("world") %>% filter(region=="Kazakhstan")

#5.1 Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data = world.cities %>% filter(country.etc=="UK")
data2 = world.cities %>% filter(country.etc=="Belgium")
data3 = world.cities %>% filter(country.etc=="Kazakhstan")

#5.2 Basic Map
#5.2.1 UK basic map
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data = data, aes(x=long, y=lat)) +
  theme_void() + ylim(50,59) + coord_map() 

#5.2.2 Belgium basic map 
ggplot() +
  geom_polygon(data = Belgium, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data = data2, aes(x=long, y=lat)) + theme_void() + coord_map() 


#5.3 Map with big cities 
#library(ggrepel)
#5.3.1 UK
ggplot() +
geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")

#5.3.2 Belgium
ggplot() +
  geom_polygon(   data = Belgium, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(     data = data2, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel(data = data2 %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point(     data = data2 %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void()  + coord_map() +
  theme(legend.position="none")

#5.3.3 KZ
ggplot() +
  geom_polygon(   data = KZ, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(     data = data3, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel(data = data3 %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point(     data = data3 %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void()  + coord_map() +
  theme(legend.position="none")

#5.4 Let's add more information 
#library(viridis)
#5.4.1 UK 
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() 
# What's wrong with yeallow bubble? 

ggplot() +
  geom_polygon(data = Belgium, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data2, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void()  + coord_map()

#5.5 How about to customise the bubbles?
# UK 
mybreaks=c(0.02, 0.04, 0.08, 1, 7)

data %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) %>%
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(  aes(x=long, y=lat, size=pop, color=pop, alpha=pop), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in M)" ) +
  theme_void() + ylim(50,59) + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in the UK") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

#Belgium
mybreaks2 = c(0.02, 0.04, 0.08, 1, 7)
data2 %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) %>%
  ggplot() +
  geom_polygon(data = Belgium, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(  aes(x=long, y=lat, size=pop, color=pop, alpha=pop), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1, 9), breaks = mybreaks2) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks = mybreaks2) +
  scale_color_viridis(option="magma", trans="log", breaks = mybreaks2, name="Population (in M)" ) +
  theme_void()  + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in Belgium") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )


#library(maps)
#library(ggplot2)
#library(tidyverse)
#library(ggrepel)
#library(viridis)
#KZ
data3 %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) %>%
  ggplot() +
  geom_polygon(data = KZ, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(  aes(x=long, y=lat, size=pop, color=pop, alpha=pop), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1, 10), breaks=mybreaks2) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks=mybreaks2) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks2, name="Population (in M)" ) +
  theme_void()  + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in the Kazakhstan") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )


##### Homework: How about WORLD map? ######################################

