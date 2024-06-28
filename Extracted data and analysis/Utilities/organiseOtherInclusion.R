
organiseOtherInclusion = function(data){

#Table and arrange the columns relating to exclusion and inclusion
other_exclusion_dimensions = table(c(data$Other_exclusionary_1, data$Other_exclusionary_2, data$Other_exclusionary_3))
other_exclusion_dimensions  = other_exclusion_dimensions %>%
  as.data.frame() %>%
  arrange(desc(Freq))

other_exclusion_dimensions = cbind(other_exclusion_dimensions, rep(NA, nrow(other_exclusion_dimensions)))
colnames(other_exclusion_dimensions) = c("Other_exclusion_dimension_original", "Frequency", "Other_exclusion_criteria")

# Aggregate up related dimensions for exclusion
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Caste"] = 'Ethnicity'
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Ethnicity"] = 'Ethnicity'
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Indigenous/Dalit"] = 'Ethnicity'

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Credit access"] = "Credit access"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Cash flow constraints"] = "Credit access"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to firm"] = "Distance to firm/cooperative"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to cooperative"] = "Distance to firm/cooperative"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance"] = "Distance to firm/cooperative"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to market"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to road"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to town"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "distance to market"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "distance to road"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance from market"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance from roads"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to roads"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "distance to main road"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Distance to township"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Surfaced road"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Road surfaces"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Transport access"] = "Distance to market/town/road"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Market access"] = "Distance to market/town/road"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Land title/tenure"] = "Land title/tenure"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Land title/tenures"] = "Land title/tenure"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Land inheritance"] = "Land title/tenure"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Irrigation"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Soil quality"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Altitude"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Land value"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Terrain"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Water access"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Soil characteristics"] = "Other land measures and land improvements"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Irrigation access"] = "Other land measures and land improvements"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Assets"] = "Other assets"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Livestock units"] = "Other assets"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "assets"] = "Other assets"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Income"] = "Other assets"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "income"] = "Other assets"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Information access"] = "Information access"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Phone ownership"] = "Information access"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Phone usage"] = "Information access"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "use of cell phone"] = "Information access"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Risk averse producers"] = "Risk averse producers"
other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "risk averse producers"] = "Risk averse producers"

other_exclusion_dimensions$Other_exclusion_criteria[other_exclusion_dimensions$Other_exclusion_dimension_original == "Migrants"] = "Migrant households"
other_exclusion_dimensions$Other_exclusion_criteria[is.na(other_exclusion_dimensions$Other_exclusion_criteria)] = "Other"

other_exclusion_dimensions = xtabs(Frequency ~Other_exclusion_criteria , other_exclusion_dimensions)
other_exclusion_dimensions = other_exclusion_dimensions %>% 
  as.data.frame()%>% 
  arrange(desc(Freq))

other_exclusion_dimensions$Other_exclusion_criteria = as.factor(other_exclusion_dimensions$Other_exclusion_criteria) 
other_exclusion_dimensions$Other_exclusion_criteria = factor(other_exclusion_dimensions$Other_exclusion_criteria, 
                                                             levels = c( levels(other_exclusion_dimensions$Other_exclusion_criteria)[!levels(other_exclusion_dimensions$Other_exclusion_criteria) == "Other"], 
                                                                        "Other"))

# Other inclusion dimensions        
other_inclusion_dimensions = table(c(data$Other_inclusionary_1, data$Other_inclusionary_2))
other_inclusion_dimensions  = other_inclusion_dimensions  %>%
  as.data.frame() %>%
  arrange(desc(Freq))

other_inclusion_dimensions = cbind(other_inclusion_dimensions, rep(NA, nrow(other_inclusion_dimensions)))
colnames(other_inclusion_dimensions) = c("Other_inclusion_dimension_original", "Frequency", "Other_inclusion_criteria")

other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Caste"] = 'Ethnicity'
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Minority group in village"] = 'Ethnicity'
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Scheduled caste"] = 'Ethnicity'
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Credit access"] = "Credit access"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Distance to firm"] = "Distance to firm/cooperative"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Distance to market"] = "Distance to market/town/road"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Distance to markets"] = "Distance to market/town/road"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Distance to road"] = "Distance to market/town/road"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "distance to road"] = "Distance to market/town/road"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Market access"] = "Distance to market/town/road"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Land title/tenure"] = "Land title/tenure"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Soil quality"] = "Other land measures and land improvements"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Assets"] = "Other assets"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Information access"] = "Information access"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Risk averse producers"] = "Risk averse producers"
other_inclusion_dimensions$Other_inclusion_criteria[other_inclusion_dimensions$Other_inclusion_dimension_original == "Migrants"] = "Migrant households"
other_inclusion_dimensions$Other_inclusion_criteria[is.na(other_inclusion_dimensions$Other_inclusion_criteria)] = "Other"

other_inclusion_dimensions = xtabs(Frequency ~Other_inclusion_criteria , other_inclusion_dimensions)
other_inclusion_dimensions = other_inclusion_dimensions %>% 
  as.data.frame()%>% 
  arrange(desc(Freq))

other_inclusion_dimensions$Other_inclusion_criteria = as.factor(other_inclusion_dimensions$Other_inclusion_criteria) 
other_inclusion_dimensions$Other_inclusion_criteria = factor(other_inclusion_dimensions$Other_inclusion_criteria, 
                                                             levels = c( levels(other_inclusion_dimensions$Other_inclusion_criteria)[!levels(other_inclusion_dimensions$Other_inclusion_criteria) == "Other"], 
                                                                         "Other"))


#Positively inclusive dimensions
other_posinclusion_dimensions = table(c(data$Other_positivelyinclusionary_1, data$Other_positivelyinclusionary_2))
other_posinclusion_dimensions  = other_posinclusion_dimensions  %>%
  as.data.frame() %>%
  arrange(desc(Freq))

other_posinclusion_dimensions = cbind(other_posinclusion_dimensions, rep(NA, nrow(other_posinclusion_dimensions)))
colnames(other_posinclusion_dimensions) = c("Other_posinclusion_dimension_original", "Frequency", "Other_posinclusion_criteria")

# Aggregate up related dimensions for exclusion
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Caste"] = 'Ethnicity'
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Scheduled caste"] = 'Ethnicity'

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Credit access"] = "Credit access"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Distance to firm"] = "Distance to firm/cooperative"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Distance"] = "Distance to firm/cooperative"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "distance to market"] = "Distance to market/town/road"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Distance to market"] = "Distance to market/town/road"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Distance to markets"] = "Distance to market/town/road"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "distance to road"] = "Distance to market/town/road"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Distance to road"] = "Distance to market/town/road"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Distance to capital"] = "Distance to market/town/road"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Land title/tenure"] = "Land title/tenure"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "renting landholders"] = "Land title/tenure"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Soil quality"] = "Other land measures and land improvements"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Assets"] = "Other assets"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Phone ownership"] = "Information access"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Phone usage"] = "Information access"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Risk averse producers."] = "Risk averse producers"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "risk averse producers"] = "Risk averse producers"
other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Risk prone producers"] = "Risk averse producers"

other_posinclusion_dimensions$Other_posinclusion_criteria[other_posinclusion_dimensions$Other_posinclusion_dimension_original == "Migrants"] = "Migrant households"
other_posinclusion_dimensions$Other_posinclusion_criteria[is.na(other_posinclusion_dimensions$Other_posinclusion_criteria)] = "Other"

other_posinclusion_dimensions = xtabs(Frequency ~Other_posinclusion_criteria , other_posinclusion_dimensions)
other_posinclusion_dimensions = other_posinclusion_dimensions %>% 
  as.data.frame()%>% 
  arrange(desc(Freq))

other_posinclusion_dimensions$Other_posinclusion_criteria = as.factor(other_posinclusion_dimensions$Other_posinclusion_criteria) 
other_posinclusion_dimensions$Other_posinclusion_criteria = factor(other_posinclusion_dimensions$Other_posinclusion_criteria, 
                                                             levels = c( levels(other_posinclusion_dimensions$Other_posinclusion_criteria)[!levels(other_posinclusion_dimensions$Other_posinclusion_criteria) == "Other"], 
                                                                         "Other"))

return(list(other_exclusion_dimensions ,other_inclusion_dimensions ,other_posinclusion_dimensions))

}

