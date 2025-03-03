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
file_path = "/Users/dna-tuananguyen/Downloads/CUB/Classes/2025 Spring/5612 MLDS/CourseProject/Github/MLDSSpring25/notebooks/data/arm/arm_data_noShale.csv"

LithoData <- read.transactions(file_path,
                           rm.duplicates = FALSE, 
                           format = "basket", 
                           sep=",", 
                           cols=1)
inspect(LithoData)

##### Use Apriori to get the RULES
LithoRulesK = arules::apriori(LithoData, parameter = list(support=.35, confidence=.25, minlen=2))

inspect(LithoRulesK)

## Count of times the products occur together in the transactions. 

## Plot of which items are most frequent
arules::itemFrequencyPlot(LithoData, topN = 10,
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative", #absolute
                          ylab = "Item Frequency (Relative)")

## Sort rules by a measure such as conf, sup, or lift
SortedRulesKSup <- sort(LithoRulesK, by="sup", decreasing=TRUE)
SortedRulesKConf <- sort(LithoRulesK, by="conf", decreasing=TRUE)
SortedRulesKLift <- sort(LithoRulesK, by="lift", decreasing=TRUE)

inspect(SortedRulesKSup[1:15])
inspect(SortedRulesKConf[1:15])
inspect(SortedRulesKLift[1:15])

(summary(SortedRulesKLift))

## Selecting or targeting specific rules  RHS
HighRHOBRules <- apriori(data=LithoData,parameter = list(supp=.001, conf=.01, minlen=2),
                     appearance = list(default="lhs", rhs="HighRHOB"),
                     control=list(verbose=FALSE))
HighRHOBRules <- sort(HighRHOBRules, decreasing=TRUE, by="confidence")
inspect(HighRHOBRules[1:4])

## Selecting rules with LHS specified
UtsiraFmRules <- apriori(data=LithoData,parameter = list(supp=.0001, conf=.001, minlen=2),
                       appearance = list(default="rhs", lhs="UtsiraFm"),
                       control=list(verbose=FALSE))
UtsiraFmRules <- sort(UtsiraFmRules, decreasing=TRUE, by="support")
inspect(UtsiraFmRules[1:4])

## Visualize (tcltk)

subrulesK <- head(sort(SortedRulesKLift, by="lift"),100)
plot(subrulesK)

plot(SortedRulesKLift, method="graph", engine="interactive")
