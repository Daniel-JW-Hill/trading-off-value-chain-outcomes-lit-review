

getInclusionBarChart = function(data, title){

  # Define the inclusion columns
  inclusion_columns = c( 'overall_inclusion', 'Land_inclusion', 'Gender_inclusion', 'Educ_Exper_inclusion', 'social_inclusion', 'hhsize_inclusion')
  
  # Create a subset of the data with the inclusion columns
  data_subset = data[, inclusion_columns]
  
  # Rename inclusion dimensions
  colnames(data_subset)[which(names(data_subset) == "overall_inclusion")] = "Overall inclusion"
  colnames(data_subset)[which(names(data_subset) == "Land_inclusion")] = "Landholdings or related"
  colnames(data_subset)[which(names(data_subset) == "Gender_inclusion")] = "Gender"
  colnames(data_subset)[which(names(data_subset) == "Educ_Exper_inclusion")] = "Education and/or experience"
  colnames(data_subset)[which(names(data_subset) == "social_inclusion")] = "Social indicators"
  colnames(data_subset)[which(names(data_subset) == "hhsize_inclusion")] = "Working age adults in household"

  
  #rename inclusion column
  inclusion_columns = c( 'Overall inclusion', 'Landholdings or related', 'Gender', 'Education and/or experience', 'Social indicators', 'Working age adults in household')
  
  # Replace "Inconclusive" with NA
  data_subset[data_subset == "Inconclusive"] = NA
  
  # Reshape the data into long format
  long_data = reshape2::melt(data_subset, id.vars = NULL, measure.vars = inclusion_columns, variable.name = "Column", value.name = "Outcome")
  
  # Set the order of outcome levels for each column
  outcome_levels = c("Exclusionary", "Inclusionary", "Positively Inclusionary")
  long_data$Outcome = factor(long_data$Outcome, levels = outcome_levels)
  
  # Calculate the count of non-NA values for each outcome
  summary_data = long_data %>%
    group_by(Column, Outcome) %>%
    summarize(Count = sum(!is.na(Outcome)))
  
  # Remove NA values
  summary_data = summary_data[!is.na(summary_data$Outcome), ]
  
  #Add a NA row for overall inclusion for the chart
  overall_row = data.frame(Column = "Overall inclusion", Outcome = "Positively Inclusionary", Count = 0)
  summary_data  = rbind(  summary_data , overall_row)
  
  # Reorder the levels of the "Column" variable with "Overall inclusion" appearing first
  summary_data$Column = reorder(summary_data$Column, match(summary_data$Column, "Overall inclusion"))
  
  
  # Plot the bar chart
 plot_1 =  ggplot(summary_data, aes(x = Column, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c( plot_1 =  ggplot(summary_data, aes(x = Column, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("coral1", "#69b3a2", "#B19CD9"), labels = outcome_levels, na.translate = FALSE) +
    labs(x = "Inclusion dimension", y = "Count of case studies", fill = "Outcome") +
    geom_text(data = subset(summary_data, !is.na(Count)), aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size=10), legend.position = "bottom")+
    labs(title = title), "#69b3a2", "#B19CD9"), labels = outcome_levels, na.translate = FALSE) +
    labs(x = "Inclusion dimension", y = "Count of case studies", fill = "Outcome") +
    geom_text(data = subset(summary_data, !is.na(Count)), aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size=10), legend.position = "bottom")+
    labs(title = title)
  
  
  # Calculate the percentage of total for each outcome
  summary_data = long_data %>%
    group_by(Column) %>%
    mutate(Total = sum(!is.na(Outcome))) %>%
    group_by(Column, Outcome) %>%
    summarize(Percentage = sum(!is.na(Outcome))/first(Total) * 100, Count = sum(!is.na(Outcome)))
  
  # Remove NA counts
  summary_data = summary_data[!is.na(summary_data$Outcome), ]
  
  
  # Plot the bar chart
  plot_2 =  ggplot(summary_data, aes(x = Column, y = Percentage, fill = Outcome)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("coral1", "#69b3a2", "#B19CD9"), labels = outcome_levels, na.translate = FALSE) +
    labs(x = "Column", y = "Percentage", fill = "Outcome") +
    geom_text(data = subset(summary_data, !is.na(Percentage)), aes(label = paste(round(Percentage), '%')), position = position_stack(vjust = 0.5), color = "black", size = 5) +
    theme_minimal() +
    theme(axis.text.x=element_text(size=12, angle=45, vjust = 1, hjust=1), axis.title.x = element_blank())+
    theme(legend.title = element_blank(),legend.text=element_text(size=12), legend.position = "bottom")+
    theme(axis.text.y = element_blank() , axis.ticks.y = element_blank(), axis.title.y = element_blank())+
    labs(title = title)
  
  return(list(plot_1, plot_2))
}
  
