########################################################################

####################HPAI 2022-2023 EPIZOOTIC ARTICLE ###################

################by Maryem Ben Salem and  Mathieu Andraud ###############



#install packages
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("grid")
install.packages("stringr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringi")
install.packages("lubridate")
install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
install.packages("RColorBrewer")
install.packages("randomcoloR")
install.packages("readr")
install.packages("ggmap")
install.packages("rgdal")
install.packages("shiny")
install.packages("viridis")
install.packages("readxl")
install.packages("openxlsx")
install.packages("leaflet")
install.packages("cluster")
install.packages("Rtsne")
install.packages("randomForestExplainer")
install.packages("randomForest")
install.packages("patchwork")


###upload packages
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(randomcoloR)
library(readr)
library(ggmap)
library(rgdal)
library(shiny)
library(viridis)
library(readxl)
library(openxlsx)
library(leaflet)
library(cluster)
library(Rtsne)
library(randomForestExplainer)
library(randomForest)
library(patchwork)


#set work diractory 

setwd ("C:/Users/m.bensalem/Documents/HPAI_code_git/HPAI-/HPAI_code")


#### Data preparation ----------------------------------------------------------

WAVE1_ALL<-prepareData("fictive_database.xlsx")

##This data is fictive. Outbreak location were sampled so no real outbreak location were kept.
##The aim of this data set is to show the layout of the data set and to be able to run the code. 
##The results will not be similar to our results.
--------------------------------------------------------------------------------
  
#### Describe the epizootic with its 2 waves -----------------------------------


##Barplot

# Transform the data column to a date format
WAVE1_ALL$Outbreak_date <- as.Date(WAVE1_ALL$Outbreak_date)

# Define the separation date
seper <- as.Date('2022-08-01')

# Filter the dataframe to keep only rows with dates on or after the cutoff date
Full_data <- WAVE1_ALL %>%
  filter(Outbreak_date  >= seper)

Full_data$Outbreak_date  <- as.Date(Full_data$Outbreak_date , format = "%Y-%m-%d") 


# Define the cutoff date
seper_1 <- as.Date('2023-03-31')

# Create a new column to categorize the outbreak dates in 2 waves
Full_data1 <- Full_data %>%
  mutate(Wave = ifelse(Outbreak_date  <= seper_1, "Wave 1", "Wave 2"))



# create a new column for the equivalent date of the start of the week  
##(Monday as a first day of the week)
Full_data1$Week_start <- Full_data1$Outbreak_date  - wday(Full_data1$Outbreak_date , week_start = 1) + 1

# Create a Week_start column for weekly aggregation
Full_data1_week <- Full_data1 %>%
  mutate(Week_start = as.Date(cut(Outbreak_date , "week")), 
         Wave = ifelse(Outbreak_date  <= seper_1, "Wave 1", "Wave 2")) %>%
  group_by(Week_start, Wave) %>%
  summarise(outbreak_numbers = n(), .groups = 'drop')

g <- ggplot(Full_data1_week, aes(x = Week_start, y = outbreak_numbers, fill = Wave)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%m /%Y", date_breaks = "1 week") +
  scale_fill_manual(values = c("Wave 1" = "orange", "Wave 2" = "lightblue")) +
  labs(
    x = "Declaration time",
    y = "Number of HPAI outbreaks",
    fill = "Wave") +
  theme_minimal() +
  theme(axis.title = element_text(), text = element_text(family = "Rubik")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(g)



## visualize with a map the 2 waves

# Prepare map data: shapefile is downloadable from gadm.org

france_shapefile <- st_read("X:/Maryem_MIASE/1.Data/2022-2023/gadm41_FRA_1.shp")

# Define the map theme and coordinates
maptheme <- theme(panel.grid = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  legend.position = "none",
                  panel.background = element_rect(fill = "lightgrey"),
                  plot.margin = unit(c(0, 0, 0, 0), 'cm'),
                  aspect.ratio = 1)

mapcoords <- coord_sf()

# Define country shapes
country_shapes_all <- geom_sf(data = france_shapefile, fill = "white", color = "black", size = 0.3)

# Plot the map
Map <- ggplot() + 
  country_shapes_all + 
  maptheme +
  geom_point(data = Full_data1, aes(x = Long, y = Lat, color = Wave), size = 2) + 
  scale_color_manual(values = c("Wave 1" = "orange", "Wave 2" = "powderblue")) + 
  mapcoords

# Display the map
print(Map)



#create one graph with both the map and the bar plot

map_waves<-Map + ggtitle("A")
G_waves <- g  + ggtitle("B")


WAVES <- arrangeGrob(arrangeGrob(map_waves),arrangeGrob(G_waves),ncol=2, 
                     widths=c(1,1))
plot(WAVES)




--------------------------------------------------------------------------------

  
  
#### Clustering and descriptive analysis ---------------------------------------

##Using the clustering function to check clusters for different scenarios 


#Scenario A with all variables (farm and environmental)
ScenA<-clustering1(data = WAVE1_ALL,index2remove = c(1:3, 7,19), Scenario = "A")

#Scenario B with just the environmental variables
ScenB<-clustering1(data = WAVE1_ALL,index2remove = c(1:7, 19), Scenario = "B")

#Visualize the silhouette plot for the 2 scenarios

b <- arrangeGrob(ScenA[[2]],ScenB[[2]], ncol=2, 
                 widths=c(1,1))
plot(b)


#Visualize the clusters plot for the 2 scenarios

c <- arrangeGrob(ScenA[[3]],ScenB[[3]], ncol=2, 
                 widths=c(1,1))
plot(c)


#Visualize the clusters maps for the 2 scenarios
x <- arrangeGrob(ScenA[[6]],ScenB[[6]], ncol=2, 
                 widths=c(1,1))
plot(x)


# Visualize plot +map for scenario A

xx <- arrangeGrob(ScenA[[6]],ScenA[[3]], ncol=2, 
                  widths=c(1,1))
plot(xx)



# Visualize plot +map for scenario B

xx1 <- arrangeGrob(ScenB[[6]],ScenB[[3]], ncol=2, 
                   widths=c(1,1))
plot(xx1)

#Visualize  plot +map for 2 scenarios A and B

cx1 <- arrangeGrob(xx,xx1, ncol = 1)
plot(cx1)




#describe Distance_PRZ_Category et DistCases_7days, DistCases_coast in scenario A

grid.draw(ScenA[[7]])


#describe NB_Farms, NB_Farms+W and NB_Farms-W in scenario A
grid.draw(ScenA[[8]])

#describe the rest in A

grid.draw(ScenA[[9]])

#describe Distance_PRZ_Category et DistCases_7days, DistCases_coast in scenario B
grid.draw(ScenB[[7]])

#describe NB_Farms, NB_Farms+W and NB_Farms-W in scenario B
grid.draw(ScenB[[8]])

#describe the rest in B
grid.draw(ScenB[[9]])



##Using the FarmChar function to describe the farm characteristics for scenario A

HC_ScenA<-FarmChar(ScenA[[4]])


--------------------------------------------------------------------------------



#### Random forests ------------------------------------------------------------

##Using the RandForA forest to evaluate variable importance for the different scenarios

p <- RandForA_heatmap_combined(ScenA[[4]], ScenB[[4]])+ 
  ggtitle("Variable Importance from Random Forest Fit")+
  theme(plot.title = element_text(hjust = 0.5))
print(p)


--------------------------------------------------------------------------------













