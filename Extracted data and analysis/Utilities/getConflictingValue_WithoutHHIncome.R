
getConflictingValue_WithoutHHIncome = function(data){

data$multiple_value_dimensions = rep(NA, nrow(data))
data$conflicting_value = rep("Conflicting low and high value outcomes", nrow(data))


#Now find overall value score
value_columns = c('value_farming',
                  'value_nethhincome',
                  'value_risks',	
                  'value_consumption',
                  'value_foodseurity',	
                  'value_womenseconomic',	
                  'value_womensempowerment',	
                  'value_children',	
                  'value_employment',	
                  'value_wages',	
                  'value_broaderenvironment',	
                  'value_landinputmarkets',
                  'Low_value_other',
                  'Null_value_other',
                  'High_value_other')



#   retrieve overall value
  for (n in 1:nrow(data)){
    data_subset = data.frame(data[n,value_columns])
    #Adjust the columns so HH income is made part of the farming column
    value_farming = data_subset$value_farming
    value_hhincome = data_subset$value_nethhincome
    
    if (value_farming == "High" && value_hhincome %in% c("Null", "Inconclusive")){
      data_subset$value_farming = "High"
    } else if (value_farming == "Low" && value_hhincome %in% c("Null", "Inconclusive")){
      data_subset$value_farming = "Low"
    } else if (value_hhincome == "High" && value_farming %in% c("Null", "Inconclusive")){
      data_subset$value_farming = "High"
    } else if (value_hhincome == "Low" && value_farming %in% c("Null", "Inconclusive")){
      data_subset$value_farming = "Low" 
    } else if (value_hhincome == "Null" && value_farming == 'Null'){
      data_subset$value_farming = "Null"
    } else {
      data_subset$value_farming = "Inconclusive"
    }
    
    data_subset = subset(data_subset, select =  - value_nethhincome)
    
    if(is.na(data_subset$Low_value_other) == FALSE){
      data_subset$Low_value_other = 'Low' 
    } else {
      data_subset$Low_value_other = 'Inconclusive' 
    }
    
    if(is.na(data_subset$Null_value_other) == FALSE){
      data_subset$Null_value_other = 'Null' 
    } else {
      data_subset$Null_value_other = 'Inconclusive' 
    }
    
    if(is.na(data_subset$High_value_other) == FALSE){
      data_subset$High_value_other = 'High' 
    } else {
      data_subset$High_value_other = 'Inconclusive' 
    }
    
    if (all(data_subset == 'Inconclusive')) {
      data$conflicting_value[n] = NA
    } else if (all(data_subset %in% c('Inconclusive', 'High', 'Null')) && any(data_subset == 'High')) { #i.e. at least one high value and remainder are null or otherwise
      data$conflicting_value[n] = 'All high value'
    } else if (all(data_subset %in% c('Inconclusive', 'Low', 'Null')) && any(data_subset == 'Low')) { #i.e. at least one low value and remainder are null or otherwise
      data$conflicting_value[n] = 'All low value'
    } else if (all(data_subset %in% c('Inconclusive', 'Null'))) { #i.e. only null results are reported
      data$conflicting_value[n] = 'All null value' 
    } else {
      data$conflicting_value[n] = 'Conflicting low and high value outcomes'
    }
    
    # Check if more than one value dimension is equal to "High", "Low", or "Null"
    high_count = sum(data_subset == 'High')
    low_count  = sum(data_subset == 'Low')
    null_count = sum(data_subset == 'Null')
    total_count = high_count + low_count + null_count 
    
    if (total_count > 1) {
      data$multiple_value_dimensions[n] = "Yes"
    } else {
      data$multiple_value_dimensions[n] = "No"
    }
  }

    return(data)
}



