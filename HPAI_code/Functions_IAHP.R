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
  
  # # Create a temporary variable to hold updated values
  # updated_category <- ifelse(WAVE1_ALL$distance_PRZ == 0.000, "Inside",
  #                            ifelse(WAVE1_ALL$distance_PRZ > 0 & WAVE1_ALL$distance_PRZ <= 2, "Neighbouring",
  #                                   ifelse(WAVE1_ALL$distance_PRZ > 2, "Outside", "unknown")))
  # 
  # # Assign the updated values to Distance_PRZ_Category
  # WAVE1_ALL$Distance_PRZ_Category <- updated_category
 
  # Replace NAs for DistCases_7days with 1000km
  
  WAVE1_ALL$DistCases_7days[is.na(WAVE1_ALL$DistCases_7days)] <- 1000
  WAVE1_ALL
}

--------------------------------------------------------------------------------



# Clustering and descriptive analysis -------------------------------------

##clustering1 is a function that checks for clusters among  a subset of the dataset in a specific scenarios (3 scenarios A, B and C)
## It returns a list of six outputs
# 1. Descriptive analysis of explanatory variables - Environmental variables only
# 2. Silhouette plot
# 3. Cluster plot (cloud)
# 4. Full Data with clustering 
# 5. Specific scenario dataset with clustering
# 6. Maps. 

clustering1 <-function(data,index2remove,Scenario){
  data <- as.data.frame(data)
  # clusters Scenario A with only type, Species and production type
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
  
  # Create the plot 
  cloud <- ggplot(tsne_data, aes(x = X, y = Y)) +
    geom_point(aes(color = cluster))+ ggtitle(paste(Scenario))+  
    theme(plot.title = element_text(hjust = 0, vjust = 1))
  
  
  # Save the info from tmp
  tmp<-data
  #  Add the cluster info into the dataset with the localisation info
  tmp$cluster <- ProceedData$cluster
  
  ## Cluster visualisation 
  # Convert the outbreak data to a data frame
  cases_df <- as.data.frame(tmp)
  
  # Define the colors for each cluster
  my_colors <- plasma(k)
  
  # Create a color vector for the clusters and initialize the cluster_colors vector with NAs
  cases_df$cluster_colors <- rep(NA, nrow(cases_df))
  
  for (i in 1:k){
    cases_df$cluster_colors[cases_df$cluster == i] <- my_colors[i]
  }
  
  # create the map with the outbreaks in different colors according to the cluster
  
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "none") +
    theme(panel.grid = element_blank()) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(plot.margin = unit(c(0, 0, 00, 0), 'cm'))
  
  mapcoords <- coord_fixed()
  
  Fr= map_data('france')

  country_shapes_all <- geom_polygon(data = Fr, aes(x = long, y = lat, group = group),
                                     fill = "white", color = "black", size = 0.3)

  Map <-ggplot() + country_shapes_all + maptheme +
    geom_point(data = cases_df, aes(x = `Long`, y = `Lat`, color = cluster_colors), size = 3) + 
    scale_color_identity() + 
    ggtitle(paste(Scenario)) + 
    coord_fixed(ratio = 1)
  
 ##Descriptive analysis of explanatory variables - Environmental  variables only
  
  VariablesViz<-NULL
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
      xlab("cluster")+ylab("Dist. Littoral")
    c=ggplot(cases_df,aes(x=as.factor(cluster),y=DistCases_watSurf))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Dist. Water Surf.")
    d=ggplot(cases_df,aes(x=as.factor(cluster),y=NB_WatSurf))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Nb. Water Surf")
    e=ggplot(cases_df,aes(x=as.factor(cluster),y=PR_WatSurf))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Water Surf Cover")
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
      xlab("cluster")+ylab("Road length")
    k=ggplot(cases_df,aes(x=as.factor(cluster),y=Distance_roads))+
      geom_boxplot()+geom_jitter(colour='lightblue',alpha=0.5)+
      xlab("cluster")+ylab("Dist. Roads")
    
    VariablesViz<-grid.arrange(a0,a,b,c,d,e,f,g,h,i,j,k, ncol=3)
  }
  
  
  assign(paste0("outputs_Scen",Scenario),
         list(VariablesViz,SilPlot,cloud, ProceedData,tmp,Map))
  return(eval(parse(text=paste0("outputs_Scen",Scenario))))
}

--------------------------------------------------------------------------------
  
# Descriptive analysis of farm characteristics ---------------------------------

##FarmChar is a function that describes the distribution of the farm characteristics
##among the different clusters

FarmChar<-function(data){
  data$Production_type2=toupper(abbreviate(ScenB[[4]]$Production_type))
  data$Species2=toupper(abbreviate(ScenB[[4]]$Species))
  data$Type2=toupper(abbreviate(ScenB[[4]]$Type))
  data$Species2[data$Species=='Multi-Species with waterfowl']="MS+W"
  data$Species2[data$Species=='Multi-Species without waterfowl']="MS-W"
  
  
  a0=ggplot(data,aes(x=Type2))+geom_bar(stat = 'count', fill = "lightblue")+
    facet_wrap( ~ as.factor(cluster) , scales="free")+
    xlab("Type") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  a=ggplot(data,aes(x=Production_type2))+geom_bar(stat = 'count', fill = "lightblue")+
    xlab("Production_type")+
    facet_wrap( ~ as.factor(cluster) , scales="free")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  b=ggplot(data,aes(x=Species2))+geom_bar(stat = 'count', fill = "lightblue")+
    facet_wrap( ~ as.factor(cluster) , scales="free")+
    xlab("Species")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  HC_plot<-grid.arrange(a0,a,b, nrow=2)
  HC_plot<-arrangeGrob(arrangeGrob(a0,a, ncol=1), b, ncol=2, widths=c(1,1))
  plot(HC_plot)
  return(HC_plot)
}
--------------------------------------------------------------------------------
  
 
  
   
  
# Applying random forest to evaluate variable importance in clustering ----------

## RandForA is a function that applies a random forest algorithm and examines 
##variable importance through the mean decrease of accuracy

  
  RandForA <- function(data) {
    tmp <- prepareData("./Data/fictive_database.xlsx")
    # dataOrigin <- tmp[,-c(1:3, 7, 19)]
    dataOrigin <- tmp %>% select(-c(1:3, 7, 19))
    
    set.seed(123)
    data$cluster <- as.factor(data$cluster)
    
    # RF model explaining clusters using other variables
    resrf <- randomForest(cluster ~ ., data = data, ntree = 500, localImp = TRUE)
    # print the model summary
    print(resrf)  
    Namescolumns <- setdiff(colnames(dataOrigin), "cluster")
    # Variable importance using decrease in accuracy
    var_importance <- data.frame(variable = setdiff(colnames(data), "cluster"),
                                 importance = as.vector(importance(resrf)[, "MeanDecreaseAccuracy"]))
    
    # Replace NA values with 0 
    var_importance$importance[is.na(var_importance$importance)] <- 0
    # Add a small constant to ensure all bars are displayed
    var_importance$importance <- ifelse(var_importance$importance == 0, 1e-10, var_importance$importance)
    # Arrange and factor levels for variables
    var_importance <- arrange(var_importance, desc(importance))
    var_importance$variable <- factor(var_importance$variable, levels = var_importance$variable)
    
    # Rename variables for better readability
    levels(var_importance$variable)[levels(var_importance$variable) == "Production_type"] <- "Production Type"
    levels(var_importance$variable)[levels(var_importance$variable) == "DistCases_7days"] <- "Dist. Case"
    levels(var_importance$variable)[levels(var_importance$variable) == "DistCases_Litt"] <- "Dist. Coast"
    levels(var_importance$variable)[levels(var_importance$variable) == "DistCases_watSurf"] <- "Dist. Water Surf."
    levels(var_importance$variable)[levels(var_importance$variable) == "NB_WatSurf"] <- "Nb. Water Surf."
    levels(var_importance$variable)[levels(var_importance$variable) == "PR_WatSurf"] <- "Water Surf. Cover"
    levels(var_importance$variable)[levels(var_importance$variable) == "NB_Farms"] <- "Nb. Farms"
    levels(var_importance$variable)[levels(var_importance$variable) == "NB_FarmsP"] <- "Nb. Farms +W"
    levels(var_importance$variable)[levels(var_importance$variable) == "NB_FarmsNP"] <- "Nb. Farms -W"
    levels(var_importance$variable)[levels(var_importance$variable) == "Nb_roads"] <- "Nb. Roads"
    levels(var_importance$variable)[levels(var_importance$variable) == "Roads_length"] <- "Road Length"
    levels(var_importance$variable)[levels(var_importance$variable) == "Distance_roads"] <- "Dist. Roads"
    levels(var_importance$variable)[levels(var_importance$variable) == "distance_ZRP"] <- "Dist. To PRZ"
    levels(var_importance$variable)[levels(var_importance$variable) == "Distance_PRZ_Category"] <- "Dist. To PRZ"
    
    # Ensure all variables in var_importance have colors assigned
    complete_palette <- global_palette[match(levels(var_importance$variable), names(global_palette))]
    
    # Plot
    p <- ggplot(var_importance, aes(x = variable, y = importance, fill = variable)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = complete_palette) + 
      scale_x_discrete(drop = FALSE) + 
      theme_minimal() + 
      theme(
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(size = 12), 
        axis.ticks.y = element_line(), 
        legend.position = "right", 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10) 
      ) +
      labs(fill = "Variable") + 
      expand_limits(y = c(min(var_importance$importance), max(var_importance$importance))) 
   
    return(p)
  }
  
  
--------------------------------------------------------------------------------  
