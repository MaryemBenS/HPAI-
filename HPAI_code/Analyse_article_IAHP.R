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

setwd ("C:/Users/m.bensalem/Documents/HPAI_22_23_Git")


#### Data preparation ----------------------------------------------------------

WAVE1_ALL<-prepareData("./Data/fictive_database.xlsx")

##This data is fictive. Outbreak location were sampled so no real outbreak location were kept.
##The aim of this data set is to show the layout of the data set and to be able to run the code. 
##The results will not be similar to our results.
--------------------------------------------------------------------------------
  
#### Describe the epizootic with its 2 waves -----------------------------------


##Barplot

# Transform the data column to a date format
WAVE1_ALL$Outbreak_date  <- as.Date(WAVE1_ALL$Outbreak_date , format = "%Y-%m-%d") 

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

# Prepare map data
Fr <- map_data('france')

# Define the map theme and coordinates
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "none") + # Hide the legend
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "lightgrey")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), 'cm'),
        aspect.ratio = 1)

mapcoords <- coord_fixed(ratio = 1)

# Define country shapes
country_shapes_all <- geom_polygon(data = Fr, aes(x = long, y = lat, group = group),
                                   fill = "white", color = "black", size = 0.3)

# Plot the map
Map <- ggplot() + 
  country_shapes_all + 
  maptheme +
  geom_point(data = Full_data1, aes(x = Long, y = Lat, color = Wave), size = 3) + 
  scale_color_manual(values = c("Wave 1" = "orange", "Wave 2" = "lightblue")) + 
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

#Scenario A with just the farm characteristics
ScenA<-clustering1(data = WAVE1_ALL,index2remove = c(1:3, 7:20), Scenario = "A")

#Scenario B with all variables (farm and environmental)
ScenB<-clustering1(data = WAVE1_ALL,index2remove = c(1:3, 7,19), Scenario = "B")

#Scenario C with just the environmental variables
ScenC<-clustering1(data = WAVE1_ALL,index2remove = c(1:7, 19), Scenario = "C")

#Visualize the silhouette plot for the 3 scenarios

b <- arrangeGrob(ScenA[[2]],ScenB[[2]],ScenC[[2]], ncol=3, 
                 widths=c(1,1,1))
plot(b)


#Visualize the clusters plot for the 3 scenarios

c <- arrangeGrob(ScenA[[3]],ScenB[[3]],ScenC[[3]], ncol=3, 
                 widths=c(1,1,1))
plot(c)


#Visualize the clusters maps for the 3 scenarios
x <- arrangeGrob(ScenA[[6]],ScenB[[6]],ScenC[[6]], ncol=3, 
                 widths=c(1,1,1))
plot(x)


##Using the FarmChar function to describe the farm characteristics for 
##senario A and B 

HC_ScenA<-FarmChar(ScenA[[4]])
HC_ScenB<-FarmChar(ScenB[[4]])



#For Scenario B  descriptive all variables

t <- arrangeGrob(ScenB[[1]],HC_ScenB, ncol=2, 
                 widths=c(1,1))
plot(t)

#For scenario C 

v <- arrangeGrob(ScenC[[1]])
plot(v)

--------------------------------------------------------------------------------



#### Random forests ------------------------------------------------------------

##Using the RandForA forest to evaluate variable importance for the different scenarios

# Define a global color palette with 15 distinct colors using randomcoloR 
#a color for each variable
global_palette <- setNames(distinctColorPalette(15), c(
  "Production Type", "Dist. Case", "Dist. Coast", "Dist. Water Surf.",
  "Nb. Water Surf.", "Water Surf. Cover", "Nb. Farms", "Nb. Farms +W",
  "Nb. Farms -W", "Nb. Roads", "Road Length", "Dist. Roads",
  "Dist. To PRZ", "Species", "Type"
))


RFA_ScenA<-RandForA(ScenA[[4]])+ ggtitle("A")
RFA_ScenB<-RandForA(ScenB[[4]])+ ggtitle("B")
RFA_ScenC<-RandForA(ScenC[[4]])+ ggtitle("C")


A <- arrangeGrob(arrangeGrob(RFA_ScenA),arrangeGrob(RFA_ScenB,RFA_ScenC, ncol=1)
                    ,ncol=2, 
                    widths=c(1,1))
plot(A)

##Create a one plot with global labels and title

# Remove individual legends
AA <- RFA_ScenA + theme(legend.position = "none")
BA <- RFA_ScenB + theme(legend.position = "none")
CA <- RFA_ScenC + theme(legend.position = "none")


# Combine the plots
combined_plot <- (AA + RFA_ScenB + CA) +
  plot_layout(ncol = 3, guides = "collect")

# Add global labels and title
combined_plot <- combined_plot +
  plot_annotation(
    title = "Variable Importance from Random Forest Fit",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
  ) 


# Create a replica plot for the vertical caption
vertical_caption <- ggplot() + 
  annotate("text", x = 1, y = 1, label = "Variable Importance \n(Mean Decrease in Accuracy)", 
           angle = 90, size = 5, hjust = 0.5) +
  theme_void()

# Combine the vertical caption with the combined plot
final_plot <- (vertical_caption | combined_plot) +
  plot_layout(widths = c(1, 10)) +
  plot_annotation(
    title = "Variable Importance from Random Forest Fit",
    caption = "Farm and Environmental characteristics",
    theme = theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 14, hjust = 0.5, vjust = 1),
      axis.title.y = element_blank()  # 
      
    )
  ) & 
  labs(
    x = "Farm and Environmental characteristics"
  )

# Display the combined plot
print(final_plot)


--------------------------------------------------------------------------------













