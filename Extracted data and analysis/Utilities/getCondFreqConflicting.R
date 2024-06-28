#Retrieved conditional frequency tables and chart

getCondFreqConflicting = function(data, title ,inclusion_criteria){

#Filter data to only include those with both value and inclusion information
filtered_data = data[!(data$conflicting_value %in% c("Inconclusive", NA)), ]
filtered_data = filtered_data[!(filtered_data[[inclusion_criteria]] %in% c("Inconclusive", NA)),]

table_outcomes = table(filtered_data$conflicting_value, filtered_data[[inclusion_criteria]])

conditional_prob =  proportions(table_outcomes, margin = 1)

# Convert the conditional probabilities to a dataframe
prob_df = as.data.frame(conditional_prob)

prob_df = prob_df[prob_df$Var2 != "Exclusionary", ]
prob_df$Var1 = factor(prob_df$Var1, levels = c("All low value", "Conflicting low and high value outcomes", "All high value", "All null value"))

# Plot the bar chart with percentage labels
plot_1 = ggplot(prob_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = value_title, y = "Conditional frequency of Inclusion given value outcomes",
       title = title) +
  geom_text(aes(label = paste0(round(Freq * 100, 2), "%")), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

return(list(plot_1, table_outcomes, conditional_prob))
}

