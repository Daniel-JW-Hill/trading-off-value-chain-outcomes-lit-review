
getWithinHeterogeneityTable = function(data){

within_heterogeneity = prop.table(table(data$Within_heterogeneity))*100
n_within = length(data$Within_heterogeneity[!is.na(data$Within_heterogeneity)])

return(list(within_heterogeneity, n_within))
}
