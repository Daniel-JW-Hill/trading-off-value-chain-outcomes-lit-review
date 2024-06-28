#Returns counts of case studies where reasons are provided. 

inclusion_reasons_total_count = function(data, type = "inclusion"){
  
  if (type == "inclusion"){
  
    count = 0
     for (i in 1:nrow(data)){
       if (is.na(data[i,"inclusion_reason_1"])){
        next
      } else {
        count = count+1
      }
  }
      return(count)
    
  
  } else if (type == "exclusion"){
  
    count = 0
    for (i in 1:nrow(data)){
      if (is.na(data[i,"exclusion_reason_1"])){
        next
      } else {
        count = count+1
      }
    } 
      return(count)
    }
  

}
  
  

