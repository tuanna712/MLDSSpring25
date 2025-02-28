library(viridis)
library(arules)
library(TSP)
library(data.table)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)
library(RColorBrewer)

## YOUR working dir goes here...
setwd("/Users/dna-tuananguyen/Downloads/CUB/Classes/2025 Spring/5612 MLDS/CourseProject/Github/MLDSSpring25/notebooks")
file_path = "/Users/dna-tuananguyen/Downloads/CUB/Classes/2025 Spring/5612 MLDS/CourseProject/Github/MLDSSpring25/notebooks/data/force/arm_data_noShale.csv"

LithoData <- read.transactions(file_path,
                           rm.duplicates = FALSE, 
                           format = "basket",  ##if you use "single" also use cols=c(1,2)
                           sep=",",  ## csv file
                           cols=1) ## The dataset HAS row numbers
inspect(LithoData)

##### Use apriori to get the RULES
LithoRulesK = arules::apriori(LithoData, parameter = list(support=.35, 
                                                 confidence=.25, minlen=3))
inspect(LithoRulesK)

## Count:
## count of times the products occur together in the transactions. 

## Plot of which items are most frequent
arules::itemFrequencyPlot(LithoData, topN = 5,
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative", #absolute
                          ylab = "Item Frequency (Relative)")

## Sort rules by a measure such as conf, sup, or lift
SortedRulesK <- sort(LithoRulesK, by="lift", decreasing=TRUE)
inspect(SortedRulesK[1:10])
(summary(SortedRulesK))

## Selecting or targeting specific rules  RHS
LithoRules <- apriori(data=LithoData,parameter = list(supp=.001, conf=.01, minlen=2),
                     appearance = list(default="lhs", rhs="Sandstone"),
                     control=list(verbose=FALSE))
LithoRules <- sort(LithoRules, decreasing=TRUE, by="confidence")
inspect(LithoRules[1:4])

## Selecting rules with LHS specified
UtsiraFmRules <- apriori(data=LithoData,parameter = list(supp=.0001, conf=.001, minlen=2),
                       appearance = list(default="rhs", lhs="UtsiraFm"),
                       control=list(verbose=FALSE))
UtsiraFmRules <- sort(UtsiraFmRules, decreasing=TRUE, by="support")
inspect(UtsiraFmRules[1:4])

## Visualize (tcltk)

subrulesK <- head(sort(SortedRulesK, by="lift"),100)
plot(subrulesK)

plot(subrulesK, method="graph", engine="interactive")
