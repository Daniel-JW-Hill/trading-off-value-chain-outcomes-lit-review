#manipulates the inclusion to get overall value assessment

getOverallInclusion = function(data){
  
  inclusion_columns = c('Land_inclusion',
                        'Gender_inclusion',
                        'Educ_Exper_inclusion',
                        'social_inclusion',
                        'hhsize_inclusion')
  
  overall_inclusion = rep(NA, nrow(data))
  for (n in 1:nrow(data)){
    data_subset = data.frame(data[n,inclusion_columns])
    data_subset[is.na(data_subset)] = 'Inconclusive'
    
    #if no assessments, set as NA
    if (all(data_subset == 'Inconclusive') == TRUE) {
      overall_inclusion[n] = NA
      next
    }
    
    if(data$inclusion_adjusted[n] == 1){ #If manually flagged as inclusionary because of correlated preference factors driving inclusion outcomes
      overall_inclusion[n] ='Inclusionary'
      next
    }
    
    if (all(data_subset %in% c('Inconclusive', 'Inclusionary', 'Positively Inclusionary')) && (any(data_subset == 'Inclusionary'))) { #i.e. at least one high value and remainder are null or otherwise
      overall_inclusion[n] = 'Inclusionary'
    } else if (all(data_subset %in% c('Inconclusive', 'Inclusionary', 'Positively Inclusionary')) && (any(data_subset == 'Positively Inclusionary'))) {
      overall_inclusion[n] = 'Inclusionary'
    } else {
      overall_inclusion[n] = 'Exclusionary'
    }
  }

    

  return(overall_inclusion)
}
  
