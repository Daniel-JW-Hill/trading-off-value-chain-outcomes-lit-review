# This function generates the conditional probability of inclusion, given a value outcome, across all relevant value domains. 

getCondFreqAll = function(data, title, value_criteria_list, inclusion_criteria, value_title) {


  summary_data = as.data.frame(matrix(data = NA, nrow = 1, ncol = 4))
  

  # Iterate through the value criteria, and get the percentages needed for the single chart. 
  for (value_criteria in value_criteria_list) {
    
    filtered_data = data[!(data[[value_criteria]] %in% c("Inconclusive", NA)), ]
    filtered_data = filtered_data[!(filtered_data[[inclusion_criteria]] %in% c("Inconclusive", NA)), ]

    table_outcomes = table(filtered_data[[value_criteria]], filtered_data[[inclusion_criteria]])
    conditional_prob = proportions(table_outcomes, margin = 1)

    # Convert the conditional probabilities to a dataframe
    prob_df = as.data.frame(conditional_prob)
    prob_df = prob_df[prob_df$Var2 != "Exclusionary", ] #Only keep conditional probability of inclusion given a value outcome
    prob_df = cbind(prob_df, rep(value_criteria,nrow(prob_df)))
    
    #Reorder
    column_order = c("rep(value_criteria, nrow(prob_df))", "Var1", "Var2", "Freq")
    prob_df =prob_df %>%
      select(all_of(column_order))
    
    colnames(prob_df) = colnames(summary_data)
    summary_data = rbind(summary_data, prob_df)
    
  }
  
  summary_data = na.omit(summary_data)
  colnames(summary_data) = c("Value_Dimension", "Value_Outcome", "Inclusion_Outcome", "Prob_outcome")
  summary_data$Value_Outcome = factor(summary_data$Value_Outcome , levels = c("Low", "Null", "High")) 
  summary_data$Prob_outcome = round(summary_data$Prob_outcome,3)

  x_axis_order = c("value_gender","value_risks","value_hhconsumption", "value_farming")
  x_axis_labels = c("Outcomes for women","Farm and household risk","Household consumption measures","Farm performance")
  
  plot_1 = ggplot(summary_data, aes(x = factor(Value_Dimension, levels = x_axis_order), y = Prob_outcome, fill = Value_Outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#F8766D", "#00BFC4", "#7CAE00"), labels = c("Low value", "Null value", "High value"), na.translate = FALSE) +
    labs(x = "Value dimension", y = "Conditional probability that a value chain is inclusive given a value outcome", fill = "Value_Outcome") +
    geom_text(aes(label = paste0(round(Prob_outcome * 100, 2), "%")), position = position_dodge(width = 0.9), hjust = 1.1, color = "black", size = 4) +
    theme_minimal()+
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 12)) +
    scale_x_discrete(labels = x_axis_labels) +  
    coord_flip()
  
  # Add horizontal lines between value outcomes
  for (i in 1:(length(x_axis_order) - 1)) {
    plot_1 = plot_1 + geom_vline(xintercept = i + 0.5, linetype = "dotted", color = "gray60", linewidth = 1)
  }
  
  return(plot_1)
}

















