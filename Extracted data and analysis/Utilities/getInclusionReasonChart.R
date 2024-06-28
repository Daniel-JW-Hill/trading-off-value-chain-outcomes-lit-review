#Creates dot chart for inclusion reasons

getInclusionReasonPlots = function(Other_inclusion_dimennsions_all){

  #First we make a new summary_data frame to collect all the relevant information
  inclusion_reasons = unique(c(as.character(Other_inclusion_dimennsions_all[[1]][,1]),
                               as.character(Other_inclusion_dimennsions_all[[2]][,1]),
                               as.character(Other_inclusion_dimennsions_all[[3]][,1]))) 
  
  summary_data = data.frame(cbind(inclusion_reasons, 
                                rep(0, length(inclusion_reasons)), 
                                rep(0, length(inclusion_reasons)), 
                                rep(0, length(inclusion_reasons))))
  
  colnames(summary_data) = c("Other_inclusion_dimensions", "Exclusionary", "Inclusionary", "Positively inclusionary")
  
  for (r in 1:nrow(Other_inclusion_dimennsions_all[[1]])){
    dimension = as.character(Other_inclusion_dimennsions_all[[1]]$Other_exclusion_criteria[r])
    row_idx = which(summary_data[,1] == dimension)
    summary_data[row_idx,2] = Other_inclusion_dimennsions_all[[1]]$Freq[r]
  }
  
  for (r in 1:nrow(Other_inclusion_dimennsions_all[[2]])){
    dimension = as.character(Other_inclusion_dimennsions_all[[2]]$Other_inclusion_criteria[r])
    row_idx = which(summary_data[,1] == dimension)
    summary_data[row_idx,3] = Other_inclusion_dimennsions_all[[2]]$Freq[r]
  }
  
  for (r in 1:nrow(Other_inclusion_dimennsions_all[[3]])){
    dimension = as.character(Other_inclusion_dimennsions_all[[3]]$Other_posinclusion_criteria[r])
    row_idx = which(summary_data[,1] == dimension)
    summary_data[row_idx,4] = Other_inclusion_dimennsions_all[[3]]$Freq[r]
  }
  
  # Manipulate the dataframe to allow for ggplot. 
  summary_data = gather(summary_data, key = "Frequency_Group", value = "Frequency", -Other_inclusion_dimensions)
  
  

  
  # Customize the order of the x-axis
  custom_order = c("Other",
                    "Risk averse producers", 
                    "Other land measures and land improvements",
                    "Land title/tenure",
                    "Other assets",
                    "Migrant households",
                    "Information access",
                    "Ethnicity",
                    "Distance to market/town/road",
                    "Distance to firm/cooperative",
                    "Credit access")
                    
  
  summary_data$Other_inclusion_dimensions = factor(
    summary_data$Other_inclusion_dimensions,
    levels = custom_order
  )

  # Ensure consistent order for Frequency_Group levels
  summary_data$Frequency_Group = factor(
    summary_data$Frequency_Group,
    levels = c("Exclusionary", "Inclusionary", "Positively inclusionary")
  )
  
  summary_data$Frequency = as.numeric(summary_data$Frequency)
  # Create the ggplot
  plot_1 = ggplot(summary_data, aes(x = Other_inclusion_dimensions, y = Frequency, fill = Frequency_Group)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(
              values = c("#F8766D", "#00BFC4", "#7CAE00"),
              labels = c("Exclusionary", "Inclusionary", "Positively inclusionary"),
              na.translate = FALSE
            ) +
            theme_minimal() +
            labs(
              x = "Other inclusion/exclusion dimension",
              y = "Frequency observed",
              fill = "Frequency_Group"
            ) +
            geom_text(
              aes(label = Frequency),
              position = position_dodge(width = 0.9),
              hjust = -0.5,
              color = "black",
              size = 3
            ) +
            theme(
              legend.title = element_blank(),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              legend.position = "bottom"
            ) +
            coord_flip() 
          
  print(plot_1)
  
  

  return(plot_1)

}


