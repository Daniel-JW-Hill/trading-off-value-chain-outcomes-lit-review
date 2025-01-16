#### Clarifying outcomes and reviewing trade-offs for inclusive value chain interventions for smallholder producers. 

# Author - Daniel Hill - July 2023
# dhill41@une.edu.au


# This script analyses the literature review data for value chain inclusion value trade offs
# The data derives from a literature review of contract farming and cooperative value chain analyses since 2003
# Each oberservation represents a value chain analysis, where papers may have more than one case study

# Clear environment
rm(list=ls())

##### Set working directory and access data ---------------------------------------------------
require(here) #This sets the local directory according to where the github repository has been cloned. 
here() # check that 'here' works. Else see the description of using 'here' in 'what they forgot to teach you about R' at https://rstats.wtf/
setwd(here()) 

library(readxl)
data = read_xlsx("Enter working directory for Data_extracted_clean.xlsx here")

#change names in dataframe for easier analysis
colnames(data)[which(colnames(data) == "Sample size of data (total)")] = "sample_size"
colnames(data)[which(colnames(data) == "Target population of study")] = "target_population"
colnames(data)[which(colnames(data) == "Services provided by the value chain")] = "value_chain_interactions"
colnames(data)[which(colnames(data) == "Type of value chain interaction")] = "value_chain_type"
colnames(data)[which(colnames(data) == "Study ID")] = "Study_ID"
colnames(data)[which(colnames(data) == "Study type")] = "Study_type"
colnames(data)[which(colnames(data) == "Type of data")] = "Data_type"
colnames(data)[which(colnames(data) == "Countries operated in")] = "Country"
colnames(data)[which(colnames(data) == "Educ_Exper_inclsuion")] = "Educ_Exper_inclusion"

data$inclusion_adjusted[is.na(data$inclusion_adjusted)] = 0

#Create subsets for contract farming and cooperatives to ease analysis. 
data$value_chain_type[data$value_chain_type == "Contract Farming"] = "Contract farming"
data$value_chain_type[data$value_chain_type == "Outgrowers"] = "Nucleus estate or outgrower scheme"
data$value_chain_type[data$value_chain_type == "Cooperatives"] = "Cooperative, collective or other producer driven organisation"
data$value_chain_type[data$value_chain_type == "Other: cooperative with contracts"] = "Cooperative, collective or other producer driven organisation"
data$value_chain_type[data$value_chain_type == "Other: Cooperatives with and without marketing, and contracts"] = "Cooperative, collective or other producer driven organisation"
data$value_chain_type[data$value_chain_type == "Other: specialty contracts through cooperative, direct marketing"] = "Cooperative, collective or other producer driven organisation"

data_cooperatives = subset(data, data$value_chain_type == 'Cooperative, collective or other producer driven organisation')
data_contracts = subset(data, data$value_chain_type != 'Cooperative, collective or other producer driven organisation') #includes outgrower schemes, but not the observation with both

# Map countries to continents for the probit analysis
library(countrycode)
data$continent = countrycode(sourcevar = data$Country,
                            origin = "country.name",
                            destination = "continent")

##### Create world map  ---------------------------------------------------

source('createWorldMap.R')

#All value chains
Map_all = createWorldMap(data, "All value chains", "lightblue", "navy")

# Repeat for cooperatives
Map_cooperatives = createWorldMap(data_cooperatives, "Cooperatives", "lightblue", "navy")

# Repeat for contracts
Map_contracts = createWorldMap(data_cooperatives, "Contract farming", "lightblue", "navy")

# Notes - can offset numbers manually, and play around with formats and colours. 

##### Get publications by year  ---------------------------------------------------

source("createYearChart.R")

#All value chain interactions
yearchart_all = createYearChart(data, "All value chains", "lightblue")
# Cooperatives
yearchart_cooperatives = createYearChart(data_cooperatives, "Cooperatives", "lightblue")
# Contracts
yearchart_contracts = createYearChart(data_contracts, "Contract farming", "lightblue")

##### Get crop diagrams  ---------------------------------------------------

source("createCropChart.R")

#All value chain interactions
cropchart_all = createCropChart(data, "All value chains", 'Pastel2')
# For cooperatives
cropchart_cooperatives = createCropChart(data_cooperatives, "Cooperatives", 'Pastel2')
# For contracts
cropchart_contracts = createCropChart(data_contracts, "Contracts", 'Pastel2')

##### Get overall inclusion and value outcomes   ---------------------------------------------------

#First group value outcomes as a single value score
source('getOverallValue.R')
data = getOverallValue(data)
data_cooperatives = getOverallValue(data_cooperatives)
data_contracts = getOverallValue(data_contracts)


# Now group overall inclusion outcomes into a single measure
source('getOverallInclusion.R')
data$overall_inclusion = getOverallInclusion(data)
data_cooperatives$overall_inclusion = getOverallInclusion(data_cooperatives)
data_contracts$overall_inclusion = getOverallInclusion(data_contracts)


##### Number of observations for inclusion outcomes  ---------------------------------------------------

source('getInclusionBarChart.R')
Inclusion_bar_charts_all = getInclusionBarChart(data, "Inclusion outcomes across all case studies")

show(Inclusion_bar_charts_all[[2]])

Inclusion_bar_charts_cooperatives = getInclusionBarChart(data_cooperatives, "Inclusion outcomes across cooperative case studies")

show(Inclusion_bar_charts_cooperatives[[2]])

Inclusion_bar_charts_contracts = getInclusionBarChart(data_contracts, "Inclusion outcomes across contract farming case studies")

show(Inclusion_bar_charts_contracts[[2]])

##### Counts of other inclusion dimensions  ---------------------------------------------------
source("organiseOtherInclusion.R")
Other_inclusion_dimensions_all = organiseOtherInclusion(data)
Other_inclusion_dimensions_cooperatives = organiseOtherInclusion(data_cooperatives)
Other_inclusion_dimensions_contracts = organiseOtherInclusion(data_contracts)

#Plot outcomes
source('getOtherInclusionDimensionPlot.R')
plot_other_exclusion_dimensions_all = getOtherInclusionDimensionPlot(Other_inclusion_dimensions_all)
  
##### Reasons for inclusion/exclusion outcomes  ---------------------------------------------------

source("organiseParticipationReasons.R")
participation_reasons_all = organiseParticipationReasons(data)
participation_reasons_cooperatives = organiseParticipationReasons(data_cooperatives)
participation_reasons_contracts = organiseParticipationReasons(data_contracts)

participation_reasons_all = rbind(participation_reasons_all[[1]], participation_reasons_all[[2]])
participation_reasons_cooperatives = rbind(participation_reasons_cooperatives[[1]], participation_reasons_cooperatives[[2]])
participation_reasons_contracts = rbind(participation_reasons_contracts[[1]], participation_reasons_contracts[[2]])

write.csv(participation_reasons_all, "participation_reasons_all.csv")
write.csv(participation_reasons_cooperatives, "participation_reasons_cooperatives.csv")
write.csv(participation_reasons_contracts, "participation_reasons_contract.csv")

# Number of case studies with reasons (to determine percentages)
source("inclusion_reasons_total_count.R")

exclusion_reasons_count_all = inclusion_reasons_total_count(data, type = "exclusion")
exclusion_reasons_count_coop = inclusion_reasons_total_count(data_cooperatives, type = "exclusion")
exclusion_reasons_count_contracts = inclusion_reasons_total_count(data_contracts, type = "exclusion")

inclusion_reasons_count_all = inclusion_reasons_total_count(data, type = "inclusion")
inclusion_reasons_count_coop = inclusion_reasons_total_count(data_cooperatives, type = "inclusion")
inclusion_reasons_count_contracts = inclusion_reasons_total_count(data_contracts, type = "inclusion")


cases_with_reasons_cooperatives = 0
for (i in 1:nrow(data_cooperatives)){
  if (is.na(data_cooperatives[i,'exclusion_reason_1']) & is.na(data_cooperatives[i,'inclusion_reason_1'])){
    next
  } else {
    cases_with_reasons_cooperatives = cases_with_reasons_cooperatives +1 
  }
}
cases_with_reasons_contracts = 0
for (i in 1:nrow(data_contracts)){
  if (is.na(data_contracts[i,'exclusion_reason_1']) & is.na(data_contracts[i,'inclusion_reason_1'])){
    next
  } else {
    cases_with_reasons_contracts = cases_with_reasons_contracts +1 
  }
}

##### Conditional probability tables and chart ---------------------------------------------------

# Get combined conditional frequency chart for all dimensions.

source('getCondFreqAll.R')
value_criteria_list = c("value_farming", "value_hhconsumption", "value_risks", "value_gender")
combined_chart = getCondFreqAll(data, "Combined Chart", value_criteria_list, "overall_inclusion", "Value Title")
print(combined_chart)


##### Conditional probability through a probit model ---------------------------------------------------
source("getCondFreqProbit.R")
condFreqProbitResults = getCondFreqProbit(data)
summary(condFreqProbitResults[[1]])
summary(condFreqProbitResults[[2]])

library(stargazer)
stargazer(condFreqProbitResults[[1]], condFreqProbitResults[[2]], type = "html", out = "Probit results - farm performance.html")

##### Value-inclusion trade off exploration ---------------------------------------------------

#First retrieve the subset of case studies which achieve both value and inclusion outcomes, 
#As defined as overall inclusion and overall value
IV_case_studies = subset(data, (data$overall_inclusion == "Inclusionary" & data$overall_value == "High"))

# write.csv(IV_case_studies, "IV case studies.csv")

##### Within heterogeneity exploration ---------------------------------------------------
#This section explores heterogeneity in value experiences within value chain case studies, and how this relates to inclusion outcomes

# First we get a column equal to:
# All high values
# All low values
# All null values
# Conflicting low and high value outcomes

source('getConflictingValue.R')
data = getConflictingValue(data)
data_cooperatives = getConflictingValue(data_cooperatives)
data_contracts = getConflictingValue(data_contracts)

table_multiple_values =prop.table(table(data$conflicting_value))*100
multiple_values = subset(data, data$multiple_value_dimensions=="Yes")
multiple_values_cooperatives = subset(data_cooperatives, data_cooperatives$multiple_value_dimensions=="Yes")
multiple_values_contracts = subset(data_contracts, data_contracts$multiple_value_dimensions=="Yes")

table_multiple_values =prop.table(table(multiple_values$conflicting_value))*100
# 26% report conflict value outcomes when more than one value is reported. Other outcomes nothing to report
table_multiple_values_cooperatives =prop.table(table(multiple_values_cooperatives$conflicting_value))*100
# 12% for cooperatives
table_multiple_values_contracts =prop.table(table(multiple_values_contracts$conflicting_value))*100
# 26% for contracts

#And if we restrict this to only mutiple value outcomes that are non-monetary (i.e. hh income is excluded)
source('getConflictingValue_WithoutHHincome.R')
data = getConflictingValue_WithoutHHIncome (data)
data_cooperatives = getConflictingValue_WithoutHHIncome (data_cooperatives)
data_contracts = getConflictingValue_WithoutHHIncome (data_contracts)

multiple_values = subset(data, data$multiple_value_dimensions=="Yes")
multiple_values_cooperatives = subset(data_cooperatives, data_cooperatives$multiple_value_dimensions=="Yes")
multiple_values_contracts = subset(data_contracts, data_contracts$multiple_value_dimensions=="Yes")

table_multiple_values =prop.table(table(multiple_values$conflicting_value))*100
# 34% report conflict value outcomes when more than one value is reported. Other outcomes nothing to report
table_multiple_values_cooperatives =prop.table(table(multiple_values_cooperatives$conflicting_value))*100
# 17% for cooperatives
table_multiple_values_contracts =prop.table(table(multiple_values_contracts$conflicting_value))*100
# 34% for contracts

# What are the major types of value-value trade offs? 
# Here we subset case studies with all high values, all low values, and conflicting, and combine all pairs of values presented into a single dataset to aggregate. 

source('getValueConflictTypology.R')

value_conflict_typology = getValueConflictTypology(data)
value_conflict_typology[,2:7] = as.integer(unlist(value_conflict_typology[,2:7]))

#Manually construct table with main insights
#First restrict total with more than 5 observations. If not, group as other. 
value_conflict_typology$total_times_assessed =rowSums(value_conflict_typology[,2:7])

value_conflict_typology_keep = subset(value_conflict_typology, value_conflict_typology$total_times_assessed>4)
value_conflict_typology_other = colSums(subset(value_conflict_typology[2:7], value_conflict_typology$total_times_assessed<5))

#Group table together
value_conflict_typology = rbind(value_conflict_typology_keep , c("Other", value_conflict_typology_other, sum(value_conflict_typology_other)))
write.csv(value_conflict_typology, "value_conflict_typology.csv")

#Repeat process for only "INCLUSIVE" value chains
value_conflict_typology_INCLUSIVE = getValueConflictTypology(subset(data, data$overall_inclusion=="Inclusionary"))
value_conflict_typology_INCLUSIVE[,2:7] = as.integer(unlist(value_conflict_typology_INCLUSIVE[,2:7]))

#Manually construct table with main insights
#First restrict total with more than 5 observations. If not, group as other. 
value_conflict_typology_INCLUSIVE$total_times_assessed =rowSums(value_conflict_typology_INCLUSIVE[,2:7], na.rm = TRUE)

value_conflict_typology_INCLUSIVE_keep = subset(value_conflict_typology_INCLUSIVE, value_conflict_typology_INCLUSIVE$total_times_assessed>1)
value_conflict_typology_INCLUSIVE_other = colSums(subset(value_conflict_typology_INCLUSIVE[2:7], value_conflict_typology_INCLUSIVE$total_times_assessed<2), na.rm = TRUE)

#Group table together
value_conflict_typology_INCLUSIVE = rbind(value_conflict_typology_INCLUSIVE_keep , c("Other", value_conflict_typology_INCLUSIVE_other, sum(value_conflict_typology_INCLUSIVE_other, na.rm = TRUE)))
write.csv(value_conflict_typology_INCLUSIVE, "value_conflict_typology_INCLUSIVE.csv")


#Repeat process for only "EXCLUSIVE" value chains
value_conflict_typology_EXCLUSIVE = getValueConflictTypology(subset(data, data$overall_inclusion=="Exclusionary"))
value_conflict_typology_EXCLUSIVE[,2:7] = as.integer(unlist(value_conflict_typology_EXCLUSIVE[,2:7]))

#Manually construct table with main insights
#First restrict total with more than 5 observations. If not, group as other. 
value_conflict_typology_EXCLUSIVE$total_times_assessed =rowSums(value_conflict_typology_EXCLUSIVE[,2:7], na.rm = TRUE)

value_conflict_typology_EXCLUSIVE_keep = subset(value_conflict_typology_EXCLUSIVE, value_conflict_typology_EXCLUSIVE$total_times_assessed>4)
value_conflict_typology_EXCLUSIVE_other = colSums(subset(value_conflict_typology_EXCLUSIVE[2:7], value_conflict_typology_EXCLUSIVE$total_times_assessed<5), na.rm = TRUE)

#Group table together
value_conflict_typology_EXCLUSIVE = rbind(value_conflict_typology_EXCLUSIVE_keep , c("Other", value_conflict_typology_EXCLUSIVE_other, sum(value_conflict_typology_EXCLUSIVE_other, na.rm = TRUE)))
write.csv(value_conflict_typology_EXCLUSIVE, "value_conflict_typology_EXCLUSIVE.csv")


# Now to consider the value chains that report heterogenous outcomes within the value chain
source('getWithinHeterogeneityTable.R')
within_heterogeneity = getWithinHeterogeneityTable(data)
within_heterogeneity[[1]]
within_heterogeneity[[2]]

within_heterogeneity_cooperatives = getWithinHeterogeneityTable(data_cooperatives)
within_heterogeneity_cooperatives[[1]]
within_heterogeneity_cooperatives[[2]]

within_heterogeneity_contracts = getWithinHeterogeneityTable(data_contracts)
within_heterogeneity_contracts[[1]]
within_heterogeneity_contracts[[2]]


##### End of script ---------------------------------------------------







