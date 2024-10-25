########################################################################

####################HPAI 2022-2023 EPIZOOTIC ARTICLE ###################

################by Maryem Ben Salem and  Mathieu Andraud ###############


###Functions###


# Data preparation -------------------------------------------------------------
#prepareData is a function that prepares the database for the analysis
#it creates an new categorical variable for the distance to PRZ in order to classify it in 3 categories
#it also allows to switch the Na for the distance to cases variables with 1000km

prepareData<-function(file){
  wave1_all <- read_excel(file)
  
  #Eliminate the observation with production type with "NA"
  
  WAVE1_ALL<- subset(wave1_all, Production_type != "NA")
  names(WAVE1_ALL)[names(WAVE1_ALL) == "Distance_ZRP_Category"] <- "Distance_PRZ_Category"
  
 
  # Replace NAs for DistCases_7days with 1000km
  
  WAVE1_ALL$DistCases_7days[is.na(WAVE1_ALL$DistCases_7days)] <- 1000
  WAVE1_ALL
}

--------------------------------------------------------------------------------



# Clustering and descriptive analysis -------------------------------------

##clustering1 is a function that checks for clusters among  a subset of the dataset in a specific scenarios (2 scenarios A and B)
## It returns a list of 9 outputs
# 1. Descriptive analysis of explanatory variables - Environmental variables only
# 2. Silhouette plot
# 3. Cluster plot (cloud)
# 4. Full Data with clustering 
# 5. Specific scenario dataset with clustering
# 6. Maps
# 7. Describe Distance_PRZ_Category et DistCases_7days, DistCases_coast 
#8. Describe NB_Farms, NB_Farms+W and NB_Farms-W 
# 9.  Describe describe the rest 

clustering1 <-function(data,index2remove,Scenario){
  data <- as.data.frame(data)
  
  data$Distance_PRZ_Category<-as.factor(data$Distance_PRZ_Category)
  ProceedData <- data[, -index2remove]
  
  # Remove rows with missing values
  ProceedData  <- na.omit(ProceedData)
  
  # Convert appropriate columns to factors
  for (i in 1:ncol(ProceedData)){
    if (is.character(ProceedData [,i])) 
      ProceedData [, i]<-as.factor(ProceedData [, i]) }
  
  # Calculate the Gower distance 
  
  gower_dist <- daisy(ProceedData, metric = "gower")
  
  # Initialize sil_width with NA values for all elements in the range 2:8
  sil_width <- rep(NA, 8)  
  
  # Adapt the loop to the range of cluster numbers
  for (i in 2:8) {
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)
    sil_width[i] <- pam_fit$silinfo$avg.width  
  }
  tmp=data.frame(Clusters=1:8,Sil=sil_width)
  
  # Customize the plot 
  SilPlot <- ggplot(tmp,aes(x=Clusters,y=Sil))+geom_point()+geom_line()+
    xlab("Number of clusters")+ylab("Silhouette width") + ggtitle(paste(Scenario))+  
    theme(plot.title = element_text(hjust = 0, vjust = 1))
  
  # Exclude the first NA value
  k <- which.max(sil_width)
  # Perform clustering using the Gower distance matrix
  pam_fit <- pam(gower_dist, diss = TRUE, k)
  
  # Assign cluster labels to the dataframe
  ProceedData <- ProceedData %>% mutate(cluster = pam_fit$clustering)
  
  # Perform t-SNE on the Gower distance matrix
  # here I am defining my perplexity because my dataset is too small. For the real dataset I left it by default (30) as shown in the below code line
  # tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  
  
  perplexity_value <- 3
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = perplexity_value)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  
  # Define the colors for each cluster (consistent across both plots)
  my_colors <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
  
  # Create the 2D cloud plot
  cloud <- ggplot(tsne_data, aes(x = X, y = Y)) +
    geom_point(aes(color = factor(cluster))) + 
    scale_color_manual(values = my_colors) + 
    ggtitle(paste(Scenario)) +  
    theme(plot.title = element_text(hjust = 0, vjust = 1))+
    labs(color = "Cluster")
  
  # Save the info from tmp
  tmp <- data
  
  # Add the cluster info into the dataset with the localization info
  tmp$cluster <- ProceedData$cluster
  
  # Convert the outbreak data to a data frame
  cases_df <- as.data.frame(tmp)
  
  # Assign the colors for each cluster based on `my_colors`
  cases_df$cluster_colors <- my_colors[cases_df$cluster]
  # 
  # Create the map with the outbreaks in different colors according to the cluster
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "none") +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'cm'))
  
  #Shapefile downlodable on gadm.org
  
  Fr <- st_read("X:/Maryem_MIASE/1.Data/2022-2023/gadm41_FRA_1.shp")

  
  # Boundries of France
  country_shapes_all <- geom_sf(data = Fr, fill = "white", color = "black", size = 0.3)
  
  #  Map with outbreaks
  Map <- ggplot() +
    country_shapes_all +
    maptheme +
    geom_point(data = cases_df, aes(x = Long, y = Lat, color = cluster_colors), size = 1) +
    scale_color_identity() +
    ggtitle(paste(Scenario)) +
    coord_sf(crs = st_crs(Fr))+
    theme(aspect.ratio = 1)
  
  ##Descriptive analysis of explanatory variables - Environmental  variables only
  
  VariablesViz<-NULL
  VariablesViz1 <- NULL
  VariablesViz2 <- NULL
  VariablesViz3 <- NULL
  
  if (ncol(ProceedData)>4){
    a0=ggplot(cases_df,aes(x=Distance_PRZ_Category))+
      geom_bar(stat = 'count', fill = "lightblue")+
      facet_wrap( ~ as.factor(cluster) , scales="free")+
      xlab("Dist. To PRZ") + theme(axis.text.x=element_text(angle=45,hjust=1))
    a=ggplot(cases_df,aes(x=as.factor(cluster),y=DistCases_7days))+
      geom_boxplot()+
      geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Dist. Case")
    b=ggplot(cases_df,aes(x=as.factor(cluster),y=DistCases_Litt))+
      geom_boxplot()+
      geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Dist. Coast")
    c=ggplot(cases_df,aes(x=as.factor(cluster),y=DistCases_watSurf))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Dist. Water Surf.")
    d=ggplot(cases_df,aes(x=as.factor(cluster),y=NB_WatSurf))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Nb. Water Surf.")
    e=ggplot(cases_df,aes(x=as.factor(cluster),y=PR_WatSurf))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Water Surf. Cover")
    f=ggplot(cases_df,aes(x=as.factor(cluster),y=NB_Farms))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Nb. Farms")
    g=ggplot(cases_df,aes(x=as.factor(cluster),y=NB_FarmsP))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Nb Farms +W")
    h=ggplot(cases_df,aes(x=as.factor(cluster),y=NB_FarmsNP))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Nb Farms -W")
    i=ggplot(cases_df,aes(x=as.factor(cluster),y=Nb_roads))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Nb Roads")
    j=ggplot(cases_df,aes(x=as.factor(cluster),y=Roads_length))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Road Length")
    k=ggplot(cases_df,aes(x=as.factor(cluster),y=Distance_roads))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Dist. Roads")
    
    VariablesViz<-grid.arrange(a0,a,b,c,d,e,f,g,h,i,j,k, ncol=3)
    
    
    
    VariablesViz1 <- grid.arrange(a0,a,b, ncol=1)
    VariablesViz2 <- grid.arrange(f,g,h, ncol=1)
    VariablesViz3 <- grid.arrange(c,d,e,i,j,k, ncol=3)
  }
  
  if (ncol(ProceedData) > 4) {
    # First set of plots
    a0 = ggplot(cases_df, aes(x = Distance_PRZ_Category)) +
      geom_bar(stat = 'count', fill = "lightblue") +
      facet_wrap(~ as.factor(cluster), scales = "free") +
      xlab("Dist. To PRZ") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    a = ggplot(cases_df, aes(x = as.factor(cluster), y = DistCases_7days)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Dist. Case")
    
    b = ggplot(cases_df, aes(x = as.factor(cluster), y = DistCases_Litt)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Dist. Coast")
    
    VariablesViz1 <- grid.arrange(a0, a, b, ncol = 1)
    
    # Second set of plots
    f = ggplot(cases_df, aes(x = as.factor(cluster), y = NB_Farms)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Nb. Farms")
    
    g = ggplot(cases_df, aes(x = as.factor(cluster), y = NB_FarmsP)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Nb Farms +W")
    
    h = ggplot(cases_df, aes(x = as.factor(cluster), y = NB_FarmsNP)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Nb Farms -W")
    
    VariablesViz2 <- grid.arrange(f, g, h, ncol = 1)
    
    # Third set of plots
    c = ggplot(cases_df, aes(x = as.factor(cluster), y = DistCases_watSurf)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Dist. Water Surf.")
    
    d = ggplot(cases_df, aes(x = as.factor(cluster), y = NB_WatSurf)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Nb. Water Surf.")
    
    e = ggplot(cases_df, aes(x = as.factor(cluster), y = PR_WatSurf)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Water Surf. Cover")
    
    i = ggplot(cases_df, aes(x = as.factor(cluster), y = Nb_roads)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Nb Roads")
    
    j = ggplot(cases_df, aes(x = as.factor(cluster), y = Roads_length)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Road Length")
    
    k = ggplot(cases_df, aes(x = as.factor(cluster), y = Distance_roads)) +
      geom_boxplot() +
      geom_jitter(colour = 'lightblue', alpha = 0.5) +
      xlab("cluster") + ylab("Dist. Roads")
    
    VariablesViz3 <- grid.arrange(c, d, e, i, j, k, ncol = 2)
  }
  
  assign(paste0("outputs_Scen",Scenario),
         list(VariablesViz,SilPlot,cloud, ProceedData,tmp,Map,VariablesViz1,VariablesViz2,VariablesViz3))
  return(eval(parse(text=paste0("outputs_Scen",Scenario))))
}

--------------------------------------------------------------------------------
  
# Descriptive analysis of farm characteristics ---------------------------------

##FarmChar is a function that describes the distribution of the farm characteristics
##among the different clusters
# 

FarmChar <- function(data) {
  
  data$Production_type2 <- toupper(abbreviate(data$Production_type))
  data$Species2 <- toupper(abbreviate(data$species))
  data$Type2 <- toupper(abbreviate(data$Type))
  
  # Vérifier et assigner les valeurs conditionnelles pour `Species2`
  if ("Multi-species with waterfowl" %in% data$species) {
    data$Species2[data$species == 'Multi-species with waterfowl'] <- "MS+W"
  }
  
  if ("Multi-species without waterfowl" %in% data$species) {
    data$Species2[data$species == 'Multi-species without waterfowl'] <- "MS-W"
  }
  
  # graphs
  a0 <- ggplot(data, aes(x = Type2)) + geom_bar(stat = 'count', fill = "lightblue") +
    facet_wrap(~ as.factor(cluster), scales = "free") +
    xlab("Type") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  a <- ggplot(data, aes(x = Production_type2)) + geom_bar(stat = 'count', fill = "lightblue") +
    xlab("Production_type") +
    facet_wrap(~ as.factor(cluster), scales = "free") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  b <- ggplot(data, aes(x = Species2)) + geom_bar(stat = 'count', fill = "lightblue") +
    facet_wrap(~ as.factor(cluster), scales = "free") +
    xlab("Species") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  
  HC_plot <- grid.arrange(a0, a, b, nrow = 2)
  HC_plot <- arrangeGrob(arrangeGrob(a0, a, ncol = 1), b, ncol = 2, widths = c(1, 1))
  plot(HC_plot)
  
  return(HC_plot)
}
--------------------------------------------------------------------------------
  
 
  
   
  
# Applying random forest to evaluate variable importance in clustering ----------

## RandForA_heatmap_combined is a function that applies a random forest algorithm and examines 
##variable importance through the mean decrease of accuracy
  
RandForA_heatmap_combined <- function(data_scenario1, data_scenario2) {
 
  compute_importance <- function(data, scenario_name) {
    set.seed(123)
    data$cluster <- as.factor(data$cluster)
    
    # RF model explaining clusters using other variables
    resrf <- randomForest(cluster ~ ., data = data, ntree = 500, localImp = TRUE)
    
    # Variable importance using decrease in accuracy
    var_importance <- data.frame(variable = setdiff(colnames(data), "cluster"),
                                 importance = as.vector(importance(resrf)[, "MeanDecreaseAccuracy"]),
                                 scenario = scenario_name)
    
    var_importance$importance[is.na(var_importance$importance)] <- 0
    var_importance$importance <- ifelse(var_importance$importance == 0, 1e-10, var_importance$importance)
    return(var_importance)
  }
  
  # Compute importance for both scenarios
  importance_scenario1 <- compute_importance(data_scenario1, "Scenario A")
  importance_scenario2 <- compute_importance(data_scenario2, "Scenario B")
  
  # Combine both scenarios into one dataframe
  importance_combined <- rbind(importance_scenario1, importance_scenario2)
  
  # Eliminate  duplicated columns
  if(any(duplicated(names(importance_combined)))) {
    stop("Erreur : duplicated columns detected")
  }
  
  # Verify columns' names are correct
  print(names(importance_combined))
  
  # Rename variables
  importance_combined$variable <- as.character(importance_combined$variable)
  importance_combined$variable[importance_combined$variable == "Production_type"] <- "Production Type"
  importance_combined$variable[importance_combined$variable == "DistCases_7days"] <- "Dist. Case"
  importance_combined$variable[importance_combined$variable == "DistCases_Litt"] <- "Dist. Coast"
  importance_combined$variable[importance_combined$variable == "DistCases_watSurf"] <- "Dist. Water Surf."
  importance_combined$variable[importance_combined$variable == "NB_WatSurf"] <- "Nb. Water Surf."
  importance_combined$variable[importance_combined$variable == "PR_WatSurf"] <- "Water Surf. Cover"
  importance_combined$variable[importance_combined$variable == "NB_Farms"] <- "Nb. Farms"
  importance_combined$variable[importance_combined$variable == "NB_FarmsP"] <- "Nb. Farms +W"
  importance_combined$variable[importance_combined$variable == "NB_FarmsNP"] <- "Nb. Farms -W"
  importance_combined$variable[importance_combined$variable == "Nb_roads"] <- "Nb. Roads"
  importance_combined$variable[importance_combined$variable == "Roads_length"] <- "Road Length"
  importance_combined$variable[importance_combined$variable == "Distance_roads"] <- "Dist. Roads"
  importance_combined$variable[importance_combined$variable == "distance_ZRP"] <- "Dist. To PRZ"
  importance_combined$variable[importance_combined$variable == "Distance_PRZ_Category"] <- "Dist. To PRZ"
  
  # Convert scenario and variable to factors 
  importance_combined$scenario <- as.factor(importance_combined$scenario)
  importance_combined$variable <- factor(importance_combined$variable, levels = unique(importance_combined$variable))
  
  # Create the heatmap
  p <- ggplot(importance_combined, aes(x = scenario, y = variable, fill = importance)) +
    geom_tile(color = "white") +  
    scale_fill_gradient(low = "white", high = "orange") +  
    theme_minimal() + 
    theme(
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    ) +
    labs(fill = "Importance")  
  
  return(p)
}
  
  
--------------------------------------------------------------------------------  
