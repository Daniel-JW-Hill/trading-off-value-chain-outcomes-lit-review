#Retrieved conditional frequency tables and chart

getCondFreq = function(data, title, value_criteria, inclusion_criteria, value_title){

#Filter data to only include those with both value and inclusion information
filtered_data = data[!(data[[value_criteria]] %in% c("Inconclusive", NA)), ]
filtered_data = filtered_data[!(filtered_data[[inclusion_criteria]] %in% c("Inconclusive", NA)),]

table_outcomes = table(filtered_data[[value_criteria]], filtered_data[[inclusion_criteria]])

conditional_prob =  proportions(table_outcomes, margin = 1)

# Convert the conditional probabilities to a dataframe
prob_df = as.data.frame(conditional_prob)

prob_df = prob_df[prob_df$Var2 != "Exclusionary", ]

if (value_criteria == "overall_value"){
prob_df$Var1 = factor(prob_df$Var1, levels = c("Low", "Null value or Inconclusive", "High"))
} else {
  prob_df$Var1 = factor(prob_df$Var1, levels = c("Low", "Null", "High")) 
}

# Plot the bar chart with percentage labels
plot_1 = ggplot(prob_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = value_title, y = "Conditional frequency of Inclusion given value outcome",
       title = title) +
  geom_text(aes(label = paste0(round(Freq * 100, 2), "%")), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

return(list(plot_1, table_outcomes, conditional_prob))
}

