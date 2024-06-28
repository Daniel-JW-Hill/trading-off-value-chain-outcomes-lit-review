#Creates bar chart for year

createYearChart= function(data, titlename, colour){
  
  observations_by_year = data %>%
    group_by(Study_year) %>%
    summarise(count = n())
  
  # Plot the bar chart
  chart =  ggplot(observations_by_year, aes(x = Study_year, y = count)) +
            geom_bar(stat = "identity", fill = colour) +
            labs(x = "Year", y = "Number of Observations") +
            theme_minimal()+
            labs(title = titlename)
  
  return(chart)
  
}