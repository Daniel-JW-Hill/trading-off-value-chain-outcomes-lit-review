

getHeatMaps = function(data, titlename, inclusion_criteria, value_criteria, inclusion_title, value_title){
  
  data[[value_criteria]] = factor(data[[value_criteria]], levels = c("High", "Low", "Null value or conflicting value outcomes"))
  
  if (length(unique(data[[inclusion_criteria]]))==4){
    data[[inclusion_criteria]]= factor(data[[inclusion_criteria]], levels = c("Inclusionary", "Exclusionary", "Positively Inclusionary", "Inconclusive"))
  } else {
    data[[inclusion_criteria]] = factor(data[[inclusion_criteria]], levels = c("Exclusionary", "Inclusionary"))
  }
  
  
  # Calculate the total number of observations
  total_observations = nrow(data)
  
  # Group the data by overall_value and land_inclusion, calculate the count and percentage
  grouped_data = data %>%
                  group_by(data[[value_criteria]], data[[inclusion_criteria]]) %>%
                  summarise(count = n()) %>%
                  mutate(percentage = count / total_observations * 100)
  colnames(grouped_data) = c(value_criteria, inclusion_criteria, 'count', 'percentage')
  
  # Expand the data frame to include all combinations if there are zero outcomes
  expanded_data =  expand.grid(
                   value_criteria = levels(data[[value_criteria]]),
                   inclusion_criteria = levels(data[[inclusion_criteria]]),
                   stringsAsFactors = FALSE) %>%
                   complete(value_criteria, inclusion_criteria, fill = list(count = 0))  # Add missing combinations with count 0
   
  colnames(expanded_data) = c(value_criteria, inclusion_criteria)
               
  # Merge the expanded data frame with the grouped data
  final_data = merge(expanded_data, grouped_data, by = c(value_criteria, inclusion_criteria), all.x = TRUE)
  
  # Replace NA values with 0 for combinations not present in the original data
  final_data[is.na(final_data)] = 0
  
  final_data = final_data[!(final_data[[inclusion_criteria]] == "Inconclusive"),]
  
  #Reorder for chart
  value_order = c("Low", "Null value or conflicting value outcomes", "High")
  
  if (length(unique(final_data[,2]))==4){
  inclusion_order = c("Exclusionary", "Inclusionary", "Positively Inclusionary")
  } else {
    inclusion_order = c("Exclusionary", "Inclusionary")
  }
  
  final_data_new = data.frame(matrix(NA, ncol = 4, nrow = (length(value_order) * length(inclusion_order))))
  colnames(final_data_new) = c(value_title, inclusion_title, 'count', 'percentage')
  counter = 1
  for (v in 1:length(value_order)){
    for (i in 1:length(inclusion_order)){
      final_data_new[counter,1] = value_order[v]
      final_data_new[counter,2] = inclusion_order[i]
      final_data_new[counter,3] = final_data$count[which(final_data[,1] == value_order[v] & final_data[,2] == inclusion_order[i])]
      final_data_new[counter,4] = final_data$percentage[which(final_data[,1] == value_order[v] & final_data[,2] == inclusion_order[i])]
      counter = counter + 1
    }
  }
  
  final_data_new[,1] = factor(final_data_new[,1], levels = c("Low", "Null value or conflicting value outcomes", "High"))
  
  if (length(unique(final_data_new[,2]))==3){
  final_data_new[,2] = factor(final_data_new[,2], levels = c("Exclusionary", "Inclusionary", "Positively Inclusionary"))
  } else {
  final_data_new[,2] = factor(final_data_new[,2], levels = c("Exclusionary", "Inclusionary"))
  }

  #Create heat map for outcomes
  plot = ggplot(final_data_new, aes(final_data_new[,2], final_data_new[,1] , fill = percentage)) +
          geom_tile(show.legend = FALSE) +
          geom_text(aes(label = paste0(round(percentage,0), "%")), color = "white") +
          labs(x = inclusion_title, y = value_title) +
          labs(title = titlename)
  
  return(plot)
}