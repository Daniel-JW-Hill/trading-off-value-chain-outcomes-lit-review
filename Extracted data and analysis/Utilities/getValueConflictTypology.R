#This function returns the value combinations assessed in the literature and organises it in a table
#The function can be used 


getValueConflictTypology = function(data) {

column_references = c(which(colnames(data) =="value_farming"),
                      which(colnames(data) =="value_nethhincome"),
                      which(colnames(data) =="value_risks"),
                      which(colnames(data) =="value_consumption"),
                      which(colnames(data) =="value_farmlabour"),
                      which(colnames(data) =="value_inputaccess"),
                      which(colnames(data) =="value_marketinformation") ,
                      which(colnames(data) =="value_foodsecurity"),
                      which(colnames(data) =="value_womenseconomic"),
                      which(colnames(data) =="value_womensempowerment"),
                      which(colnames(data) =="value_children"),
                      which(colnames(data) =="value_employment"),
                      which(colnames(data) =="value_farmenvironment"),
                      which(colnames(data) =="value_broaderenvironment"),
                      which(colnames(data) =="value_landinputmarketst"))

high_high = list()
low_low = list()
null_null = list()
high_low = list()
high_null = list()
low_null = list()

counter_highhigh = 1
counter_lowlow = 1
counter_nullnull = 1
counter_highlow = 1
counter_highnull = 1
counter_lownull = 1

for (r in 1:nrow(data)){
  data_subset = data[r,column_references]
  
  for (i in 1:ncol(data_subset)){
    for (j in 1:ncol(data_subset)){
      if (j > i){
      
        value_1 = data_subset[i]
        value_2 = data_subset[j]
        
        if (value_1 != "Inconclusive" & value_2 != "Inconclusive"){
          value_pair = paste(value_1, value_2, sep = "-")
          if (value_pair == "High-High"){
            colname_1 = colnames(data_subset[i])
            colname_2 = colnames(data_subset[j])
            high_high[[counter_highhigh]] = paste(colname_1, colname_2, sep = "-")
            counter_highhigh = counter_highhigh + 1
            
          } else if (value_pair == "Low-Low"){
            colname_1 = colnames(data_subset[i])
            colname_2 = colnames(data_subset[j])
            low_low[[counter_lowlow]] = paste(colname_1, colname_2, sep = "-")
            counter_lowlow = counter_lowlow + 1
            
          } else if (value_pair == "Null-Null"){
            colname_1 = colnames(data_subset[i])
            colname_2 = colnames(data_subset[j])
            null_null[[counter_nullnull]] = paste(colname_1, colname_2, sep = "-")
            counter_nullnull = counter_nullnull + 1
            
          } else if (value_pair == "High-Low" || value_pair == "Low-High" ){
            colname_1 = colnames(data_subset[i])
            colname_2 = colnames(data_subset[j])
            high_low[[counter_highlow]] = paste(colname_1, colname_2, sep = "-")
            counter_highlow = counter_highlow + 1
            
          } else if (value_pair == "High-Null" || value_pair == "Null-High" ){
            colname_1 = colnames(data_subset[i])
            colname_2 = colnames(data_subset[j])
            high_null[[counter_highnull]] = paste(colname_1, colname_2, sep = "-")
            counter_highnull = counter_highnull + 1
            
          } else if (value_pair == "Low-Null" || value_pair == "Null-Low" ){
            colname_1 = colnames(data_subset[i])
            colname_2 = colnames(data_subset[j])
            low_null[[counter_lownull]] = paste(colname_1, colname_2, sep = "-")
            counter_lownull = counter_lownull + 1
          }
         }
       }  
     }
   }
}

#Unlist for manipulation
high_high = unlist(high_high)
low_low  = unlist(low_low)
null_null = unlist(null_null)
high_low = unlist(high_low)
high_null  = unlist(high_null)
low_null = unlist(low_null)

#Retrieve frequencies
high_high_tab = high_high %>%
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))

null_null_tab = null_null %>%
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))

low_low_tab = low_low %>%
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))


high_low_tab = high_low %>%
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))

high_null_tab = high_null %>%
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))

low_null_tab = low_null %>%
  table()%>%
  as.data.frame() %>%
  arrange(desc(Freq))

# Now organised into one table
value_combinations = unique(c(as.character(high_high_tab[,1], 
                              as.character(null_null_tab[,1]), 
                              as.character(low_low_tab[,1]), 
                              as.character(high_low_tab[,1]), 
                              as.character(high_null_tab[,1]),
                              as.character(low_null_tab[,1]))))

value_combination_frequencies = as.data.frame(cbind(value_combinations, matrix(data = 0, nrow = length(value_combinations), ncol = 6)))

for (r in 1:nrow(high_high_tab)){
  value_combo = as.character(high_high_tab[r,1])
  row_idx = which(value_combination_frequencies$value_combinations  == value_combo)
  value_combination_frequencies[r,2] = high_high_tab$Freq[r]
}
for (r in 1:nrow(null_null_tab)){
  value_combo = as.character(null_null_tab[r,1])
  row_idx = which(value_combination_frequencies$value_combinations  == value_combo)
  value_combination_frequencies[r,3] = null_null_tab$Freq[r]
}
for (r in 1:nrow(low_low_tab)){
  value_combo = as.character(low_low_tab[r,1])
  row_idx = which(value_combination_frequencies$value_combinations  == value_combo)
  value_combination_frequencies[r,4] = low_low_tab$Freq[r]
}
for (r in 1:nrow(high_low_tab)){
  value_combo = as.character(high_low_tab[r,1])
  row_idx = which(value_combination_frequencies$value_combinations  == value_combo)
  value_combination_frequencies[r,5] = high_low_tab$Freq[r]
}
for (r in 1:nrow(high_null_tab)){
  value_combo = as.character(high_null_tab[r,1])
  row_idx = which(value_combination_frequencies$value_combinations  == value_combo)
  value_combination_frequencies[r,6] = high_null_tab$Freq[r]
}
for (r in 1:nrow(low_null_tab)){
  value_combo = as.character(low_null_tab[r,1])
  row_idx = which(value_combination_frequencies$value_combinations  == value_combo)
  value_combination_frequencies[r,7] = low_null_tab$Freq[r]
}

colnames(value_combination_frequencies) = c("Value combination", "High-High", "Null-Null", "Low-Low", "High-Low", "High-Null", "Low-Null")

return(value_combination_frequencies)
 }


