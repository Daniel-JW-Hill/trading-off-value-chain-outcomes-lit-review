# Creates char to summarise crop types

createCropChart = function(data, titlename, palette){
  library(ggrepel)
  observations_by_crop = data %>%
    group_by(Products_marketed_summary) %>%
    summarise(count = n())
  
  #get pie chart positions
  observations_by_crop = observations_by_crop %>% 
    mutate(csum = rev(cumsum(rev(count))), 
           pos = count/2 + lead(csum, 1),
           pos = if_else(is.na(pos), count/2, pos))
  
  
  # Create the pie chart
chart =   ggplot(observations_by_crop, aes(x = "", y = count, fill = as.factor(Products_marketed_summary))) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          scale_fill_brewer(palette = palette) +
          labs(fill = "Products marketed") +
          geom_label_repel(data = observations_by_crop,
                           aes(y = pos, label = count),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          theme_void()+
          labs(title = "All value chains")
 
  
  return(chart) 
}