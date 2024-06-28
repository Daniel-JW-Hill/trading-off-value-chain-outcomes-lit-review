#Creates the world map

createWorldMap = function(data, titlename, lowcolour, highcolour){
  
  #Load libraries 
  library(tidyverse)
  library(rvest)
  library(magrittr)
  library(ggmap)
  library(stringr)
  library(ggplot2)
  
  #Create country dataframe - All case studies, just cooperatives and just contracts/Nucleus
  aggregated_country_df = data %>% 
    group_by(Country) %>% 
    summarise(Observations = n())
  
  # Get world map
  map.world = map_data("world")
  
  #Recode any country names to match world map, and merge
  #inspect
  #as.factor(aggregated_country_df$Country) %>% levels()
  aggregated_country_df$Country = recode(aggregated_country_df$Country 
                                         ,"Taiwan (China)" = 'Taiwan'
                                         ,"Cote d'Ivoire" = 'Ivory Coast'
  )
  
  map.world_joined = left_join(map.world, aggregated_country_df, by = c('region' = 'Country'), relationship = "many-to-many")
  
  #get list of other countries to ignore in plot
  other_countries =  unique(map.world_joined$region[is.na(map.world_joined$Observation)])
  
  # Merge the world map data with your observations data (assuming you have a dataframe named 'observations_df' with columns 'region' and 'Observations')
  merged_data = map.world_joined
  
  # Calculate the centroid of each country
  centroid_data = aggregate(cbind(long, lat) ~ region, map.world_joined, FUN = mean)
  centroid_data = left_join(centroid_data, aggregated_country_df, by = c('region' = 'Country'))
  
  # Plot the map with fill color based on the number of observations and labels
chart =   ggplot() +
          geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = Observations)) +
          geom_polygon(data = subset(merged_data, region %in% other_countries), aes(x = long, y = lat, group = group), fill = "lightgrey") +
          geom_text(data = centroid_data , aes(x = long, y = lat, label = Observations), color = "black", size = 3) +
          scale_fill_gradient(low = lowcolour, high = highcolour) +
          theme_minimal()+
          labs(title = titlename)

return(chart)
  
  
}