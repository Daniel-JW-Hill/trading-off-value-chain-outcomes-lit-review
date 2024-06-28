# Conditional Frequency - probit model

getCondFreqProbit = function(data){
  
  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(glm2)
  library(ggplot2)
  library(broom)
  library(DescTools)

  #Create dummy indicators
  data_filtered = data%>%
    mutate(
      Grains_Legumes_Rice_Sugarcane = ifelse(Products_marketed_summary %in% c("Grains and legumes", "Rice", "Sugarcane"), 1, 0),
      Annual_Crops = ifelse(Products_marketed_summary == "Annual crops", 1, 0),
      Livestock = ifelse(Products_marketed_summary %in% c("Livestock and livestock products"), 1, 0),
      Other_Products = ifelse(Products_marketed_summary %in% c("Agroforestry and orchard products", "Oil palm", "Other or various"), 1, 0)
    )
  
  data_filtered = data_filtered %>%
    mutate(
      Contract_Farming_Nucleus_Estate = ifelse(value_chain_type %in% c("Contract farming", "Nucleus estate or outgrower scheme"), 1, 0),
      Cooperative = ifelse(value_chain_type == "Cooperative, collective or other producer driven organisation ", 1, 0)
    )
  
  data_filtered = data_filtered%>%
    mutate(
      Africa = ifelse(continent == "Africa", 1, 0),
      Americas = ifelse(continent == "Americas", 1, 0),
      Asia = ifelse(continent == "Asia", 1, 0),
    )
  
  
  data_filtered = data_filtered %>%
    mutate(
      Inclusionary = ifelse(overall_inclusion == "Inclusionary", 1, 0)
    )
  

  #### FARM PEFORMANCE ####
    
    #Remove inconclusive or NA values. 
    data_farming = data_filtered %>%
      filter(!is.na(value_farming) & value_farming != "Inconclusive" & !is.na(overall_inclusion))
    data_farming= data_farming%>%
      filter(value_chain_type != "Both")
    
    #Create dummy indicators 
    data_farming = data_farming %>%
      mutate(
        High = ifelse(value_farming == "High", 1, 0),
        Low = ifelse(value_farming == "Low", 1, 0),
        Null = ifelse(value_farming == "Null", 1, 0)
      )
  
    # Run the logit model - Low, Grains_Legumes_Rice_Sugarcane, Cooperative all excluded. 
    condfreqmodelfarming = glm(Inclusionary ~ High + Null  + Annual_Crops + Livestock + Other_Products + Contract_Farming_Nucleus_Estate + Americas + Asia, 
                  data = data_farming, family = binomial(link = "logit"))
    
    coef_summary = summary(condfreqmodelfarming)$coefficients
    odds_ratios = exp(coef_summary[, "Estimate"])
    
    # Calculate confidence intervals for the odds ratios
    ci = exp(confint(condfreqmodelfarming))
    
    # Create a data frame for Null and High odds ratios and confidence intervals
    odds_data = data.frame(
      Variable = names(odds_ratios),
      OddsRatio = odds_ratios,
      CI_Lower = ci[,1],
      CI_Upper = ci[,2]
    )
    
    odds_data_farming = odds_data[odds_data$Variable == "High" | odds_data$Variable == "Null" ,]
    odds_data_farming$value = rep("Farm performance outcomes", nrow(odds_data_farming ))
    
    mcfaddenR2_farming = PseudoR2(condfreqmodelfarming, which = "McFadden")
    
    #### Other value outcomes ####
    #Remove inconclusive or NA values. 
    data_other = data_filtered %>%
      filter(!is.na(value_other) & value_other != "Inconclusive" & !is.na(overall_inclusion))
    data_other= data_other%>%
      filter(value_chain_type != "Both")
    
    #Create dummy indicators 
    data_other = data_other %>%
      mutate(
        High = ifelse(value_other == "High", 1, 0),
        Low = ifelse(value_other == "Low", 1, 0),
        Null = ifelse(value_other == "Null", 1, 0)
      )
    
    # Run the logit model - Low, Grains_Legumes_Rice_Sugarcane, Cooperative all excluded. 
    condfreqmodelother = glm(Inclusionary ~ High + Null  + Annual_Crops + Livestock + Other_Products + Contract_Farming_Nucleus_Estate + Americas + Asia, 
                               data = data_other, family = binomial(link = "logit"))
    
    coef_summary = summary(condfreqmodelother)$coefficients
    odds_ratios = exp(coef_summary[, "Estimate"])
    
    # Calculate confidence intervals for the odds ratios
    ci = exp(confint(condfreqmodelother))
    
    # Create a data frame for Null and High odds ratios and confidence intervals
    odds_data = data.frame(
      Variable = names(odds_ratios),
      OddsRatio = odds_ratios,
      CI_Lower = ci[,1],
      CI_Upper = ci[,2]
    )
    
    odds_data_other = odds_data[odds_data$Variable == "High" | odds_data$Variable == "Null" ,]
    odds_data_other$value = rep("Other value outcomes", nrow(odds_data_other ))
    
    mcfaddenR2_other = PseudoR2(condfreqmodelother, which = "McFadden")
    
    ##### PRINT OUTCOMES ####
    
    odds_data = rbind(odds_data_farming,
                      odds_data_other)
    
    odds_data$value = factor(odds_data$value, 
                              levels =  c("Other value outcomes", "Farm performance outcomes"))
    
    # Plot 
    plot = ggplot(odds_data, aes(x = value, y = OddsRatio, color = Variable, group = Variable, ymin = CI_Lower, ymax = CI_Upper)) +
      geom_point(stat = "identity", position = position_dodge(width =0.5), size = 5) +
      geom_errorbar(position = position_dodge(width = 0.5), width = 0.2, linewidth = 1) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      labs(title = NULL, x = NULL) +
      labs(y = "Odds Ratio") +
      theme_minimal() +
      coord_flip() +
      theme(
        axis.text.x = element_text(size = 12),  
        axis.title.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        legend.text = element_text(size = 12),  
        legend.title = element_blank(),
        legend.position = "bottom"  
      ) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
      scale_color_manual(labels = c("High value", "Null value"), values = c("High" = "coral1", "Null" = "#69b3a2")) 
   
    
    print(plot)
    
    return(list(condfreqmodelfarming, condfreqmodelother))
}
