
organiseParticipationReasons = function(data){
  
  require(dplyr)
  
  exclusion_reasons = as.data.frame(table(c(data$exclusion_reason_1, data$exclusion_reason_2,data$exclusion_reason_3)))
  
  exclusion_reasons = exclusion_reasons %>%
                      as.data.frame() %>%
                      arrange(desc(Freq)) %>%
                      mutate(Group = case_when(
                        Var1 %in% c("Producer side costs", "Producer side transaction and/or information search costs") ~ "Producer side",
                        Var1 %in% c("Explicit buyer side constraints", "Implicit buyer side constraints") ~ "Buyer side constraints",
                        Var1 %in% c("Social and/or cultural barriers to participation") ~ "Either producer or buyer side constraints",
                        Var1 %in% c("Farmer preferences for alternative livelihood strategies") ~ "Farmer preferences",
                        TRUE ~ "Other"
                      ))
  
  
  inclusion_reasons = as.data.frame(table(c(data$inclusion_reason_1, data$inclusion_reason_2,data$inclusion_reason_3)))
  
  inclusion_reasons$Var1 = as.vector(inclusion_reasons$Var1)
  inclusion_reasons$Var1[inclusion_reasons$Var1 == "Farmer preferences for alternative livelihood strategies"] = 'Target population preferences to engage in this value chain relative to other livelihood strategies'
  inclusion_reasons$Var1[inclusion_reasons$Var1 == "Producer side costs"] = 'Lower costs for producer relative to alternatives'
  inclusion_reasons$Var1[inclusion_reasons$Var1 == "Producer side transaction and/or information search costs"] = 'Lower costs for buyer to engage with target population'
  inclusion_reasons$Var1[inclusion_reasons$Var1 == "Explicit buyer side constraints"] = 'Lower costs for buyer to engage with target population'
  inclusion_reasons$Var1[inclusion_reasons$Var1 == "Social and/or cultural barriers to participation"] = 'Lower costs for buyer to engage with target population'
  inclusion_reasons$Var1[inclusion_reasons$Var1 == "Explicit intervention"] = 'Explicit targeting of target population for development reasons'
  
  inclusion_reasons = xtabs(Freq~Var1, inclusion_reasons)
  
  inclusion_reasons  = inclusion_reasons %>%
    as.data.frame() %>%
    arrange(desc(Freq))%>%
    mutate(Group = case_when(
      Var1 %in% c('Target population preferences to engage in this value chain relative to other livelihood strategies', 'Lower costs for producer relative to alternatives') ~ "Producer side benefits relative to alternatives",
      Var1 %in% c( 'Lower costs for buyer to engage with target population', 'Explicit targeting of target population for development reasons') ~ "Buyer side choice to engage with target population",
      TRUE ~ "Other"
    ))
  
  return(list(exclusion_reasons, inclusion_reasons))
  
  
}




