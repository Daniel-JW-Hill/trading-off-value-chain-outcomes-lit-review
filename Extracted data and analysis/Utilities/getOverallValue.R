#manipulates the value data to get overall value assessment

getOverallValue = function(data){
  
  #Aggregate value from farming (hierarchy of farm profits, income, then yield)
  data$value_farming = rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    value_yields = data$value_yields[n]
    value_farmincome = data$value_farmincome[n]
    value_farmprofits = data$value_farmprofits[n]
    
    if (value_farmprofits == 'High') {
      data$value_farming[n] = "High"
      next
    } else if (value_farmprofits == 'Null') {
      data$value_farming[n] = "Null"
      next
    } else if (value_farmprofits == 'Low') {
      data$value_farming[n] = "Low"
      next
    }
    

    if (value_farmincome == 'High') {
      data$value_farming[n] = "High"
      next
    } else if (value_farmincome == 'Null') {
      data$value_farming[n] = "Null"
      next
    } else if (value_farmincome == 'Low') {
      data$value_farming[n] = "Low"
      next
    }
    
    if (value_yields == 'High') {
      data$value_farming[n] = "High"
    } else if (value_yields == 'Null') {
      data$value_farming[n] = "Null"
    } else if (value_yields == 'Low') {
      data$value_farming[n] = "Low"
    }
    
  } 
  
  #Aggregate household consumption scores - either household income, other consumption measures, food security/nutrition, children outcomes
  #Hierarchy considers consumption measures as equal first, then hh net income. 
  data$value_hhconsumption= rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    value_hhincome = data$value_nethhincome[n]
    value_consumption = data$value_consumption[n]
    value_foodsecurity = data$value_foodseurity[n]
    value_children = data$value_children[n]

    if (value_consumption  == 'High' & value_foodsecurity %in% c('High', 'Inconclusive') & value_children %in% c('High', 'Inconclusive')){
       data$value_hhconsumption[n] = 'High'
       next
    } else if (value_foodsecurity  == 'High' & value_consumption %in% c('High', 'Inconclusive')& value_children %in% c('High', 'Inconclusive')){
       data$value_hhconsumption[n] = 'High'
       next
    } else if (value_children == 'High' & value_consumption %in% c('High', 'Inconclusive')& value_foodsecurity %in% c('High', 'Inconclusive')){
      data$value_hhconsumption[n] = 'High'
      next
    } else if (value_foodsecurity  == 'Low' & value_consumption %in% c('Low', 'Inconclusive')& value_children %in% c('Low', 'Inconclusive')){
        data$value_hhconsumption[n] = 'Low'
       next
    } else if (value_consumption  == 'Low' & value_foodsecurity %in% c('Low', 'Inconclusive')& value_children %in% c('Low', 'Inconclusive')) {
       data$value_hhconsumption[n] = 'Low'
       next 
    } else if (value_children  == 'Low' & value_consumption %in% c('Low', 'Inconclusive')& value_foodsecurity %in% c('Low', 'Inconclusive')) {
      data$value_hhconsumption[n] = 'Low'
      next 
    } else {
       data$value_hhconsumption[n] = "Null"
       next
    }
       
    # if neither are considered, consider hh income
     if (value_hhincome == 'High') {
      data$value_hhconsumption[n] = "High"
    } else if (value_hhincome == 'Null') {
      data$value_hhconsumption[n] = "Null"
    } else if (value_hhincome == 'Low') {
      data$value_hhconsumption[n] = "Low"
    }
  }
  
  # Aggregate related production value outcomes
  # Farm labour, input support, market information, on farm environment outcomes
  data$value_farmancilliary= rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    value_farmlabour = data$value_farmlabour[n]
    value_inputaccess = data$value_inputaccess[n]
    value_marketinformation = data$value_marketinformation[n]
    value_farmenvironment = data$value_farmenvironment[n]
    
    if (value_farmlabour  == 'High' & value_inputaccess %in% c('High', 'Inconclusive') & value_marketinformation %in% c('High', 'Inconclusive')& value_farmenvironment %in% c('High', 'Inconclusive')){
      data$value_farmancilliary[n] = 'High'
    } else if (value_inputaccess  == 'High' & value_farmlabour %in% c('High', 'Inconclusive')& value_marketinformation %in% c('High', 'Inconclusive')& value_farmenvironment %in% c('High', 'Inconclusive')){
      data$value_farmancilliary[n] = 'High'
    } else if (value_marketinformation == 'High' & value_farmlabour %in% c('High', 'Inconclusive')& value_inputaccess %in% c('High', 'Inconclusive') & value_farmenvironment %in% c('High', 'Inconclusive')){
      data$value_farmancilliary[n] = 'High'
    } else if (value_farmenvironment == 'High' & value_farmlabour %in% c('High', 'Inconclusive')& value_inputaccess %in% c('High', 'Inconclusive') & value_marketinformation %in% c('High', 'Inconclusive')){
      data$value_farmancilliary[n] = 'High'
   
    } else if (value_inputaccess  == 'Low' & value_farmlabour %in% c('Low', 'Inconclusive')& value_marketinformation %in% c('Low', 'Inconclusive')& value_farmenvironment %in% c('Low', 'Inconclusive')){
      data$value_farmancilliary[n] = 'Low'
    } else if (value_farmlabour  == 'Low' & value_inputaccess %in% c('Low', 'Inconclusive')& value_marketinformation %in% c('Low', 'Inconclusive')& value_farmenvironment %in% c('Low', 'Inconclusive')) {
      data$value_farmancilliary[n] = 'Low'
    } else if (value_marketinformation  == 'Low' & value_farmlabour %in% c('Low', 'Inconclusive')& value_inputaccess %in% c('Low', 'Inconclusive')& value_farmenvironment %in% c('Low', 'Inconclusive')) {
      data$value_farmancilliary[n] = 'Low'
    } else if (value_farmenvironment  == 'Low' & value_farmlabour %in% c('Low', 'Inconclusive')& value_inputaccess %in% c('Low', 'Inconclusive')& value_marketinformation %in% c('Low', 'Inconclusive')) {
      data$value_farmancilliary[n] = 'Low'
    
    } else {
      data$value_farmancilliary[n] = "Null"
    }
  }
  
  
  # Aggregate gender outcomes - economic or empowerment
  data$value_gender= rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    value_womeneconomic = data$value_womenseconomic[n]
    value_womenempowerment = data$value_womensempowerment[n]

    if (value_womeneconomic  == 'High' & value_womenempowerment %in% c('High', 'Inconclusive')){
      data$value_gender[n] = 'High'
      next
    } else if (value_womenempowerment  == 'Low' & value_womeneconomic %in% c('Low', 'Inconclusive')){
      data$value_gender[n] = 'Low'
      next
    } else {
      data$value_gender[n] = "Null"
    }
  }
  
  # Aggregate employment outcomes - wages and/or employment
  # Aggregate gender outcomes - economic or empowerment
  data$value_offfarm= rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    value_wages = data$value_wages[n]
    value_employment = data$value_employment[n]
    
    if (value_wages  == 'High' & value_employment %in% c('High', 'Inconclusive')){
      data$value_offfarm[n] = 'High'
      next
    } else if (value_employment  == 'Low' & value_wages %in% c('Low', 'Inconclusive')){
      data$value_offfarm[n] = 'Low'
      next
    } else {
      data$value_offfarm[n] = "Null"
    }
  }
  
  # Aggregate spillovers - off farm environment and land/input markets. 
  data$value_spillovers = rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    value_broaderenvironment = data$value_broaderenvironment[n]
    value_landinputmarkets = data$value_landinputmarkets[n]
    
    if (value_broaderenvironment  == 'High' & value_landinputmarkets %in% c('High', 'Inconclusive')){
      data$value_spillovers[n] = 'High'
      next
    } else if (value_landinputmarkets  == 'Low' & value_broaderenvironment %in% c('Low', 'Inconclusive')){
      data$value_spillovers[n] = 'Low'
      next
    } else {
      data$value_spillovers[n] = "Null"
    }
  }
  
  # Aggregate all non farming value outcomes 
  data$value_other = rep('Inconclusive', nrow(data))
  for (n in 1:nrow(data)){
    consumption = data$value_hhconsumption[n]
    gender = data$value_gender[n]
    risks = data$value_risks[n]
    offfarm = data$value_offfarm[n]
    farmancilliary = data$value_farmancilliary[n]
    
    value_outcomes = c(consumption,
                       gender,
                       risks,
                       offfarm,
                       farmancilliary)
    
    if (all(value_outcomes %in% c("High", "Null", "Inconclusive")) && any(value_outcomes %in% c("High"))){
      data$value_other[n]= 'High'
      next
    } else if (all(value_outcomes %in% c("Low", "Null", "Inconclusive"))&& any(value_outcomes %in% c("Low"))){
      data$value_other[n]= 'Low'
      next
    } else {
      data$value_other[n]= 'Null'
    }
  }
    
  
    #Now find overall value score
    value_columns = c('value_farming',
    	                'value_nethhincome',
                      'value_risks',	
                      'value_consumption',
                      'value_farmlabour',	
                      'value_inputaccess',
                      'value_marketinformation',
                      'value_foodseurity',	
                      'value_womenseconomic',	
                      'value_womensempowerment',	
                      'value_children',	
                      'value_employment',	
                      'value_wages',	
                      'value_farmenvironment',	
                      'value_broaderenvironment',	
                      'value_landinputmarkets',	
                      'Low_value_other',	
                      'Null_value_other', 
                      'High_value_other')
    
    #retrieve overall value
    data$overall_value = rep(NA, nrow(data))
  
    for (n in 1:nrow(data)){
      data_subset = data.frame(data[n,value_columns])
      data_subset[is.na(data_subset)] = 'Inconclusive'
      
      if(data_subset$Low_value_other != 'Inconclusive'){
        data_subset$Low_value_other = 'Low' 
      }
      
      if(data_subset$Null_value_other != 'Inconclusive'){
        data_subset$Null_value_other = 'Null' 
      }
      
      if(data_subset$High_value_other != 'Inconclusive'){
        data_subset$High_value_other = 'High' 
      }
      
      #if no assessments, set as NA
      if (all(data_subset == 'Inconclusive') == TRUE) {
        data$overall_value[n] = NA
        next
      }
      
      if (all(data_subset %in% c('Inconclusive', 'High', 'Null')) && any(data_subset == 'High')) { #i.e. at least one high value and remainder are null or otherwise
        data$overall_value[n] = 'High'
      } else if (all(data_subset %in% c('Inconclusive', 'Low', 'Null')) && any(data_subset == 'Low')) { #i.e. at least one low value and remainder are null or otherwise
        data$overall_value[n] = 'Low'
      } else {
        data$overall_value[n] = 'Null or inconclusive'
      }  
      
    }  
    
    data$overall_value[is.na(data$overall_value)] = "Null or inconclusive"
    
    return(data)
}