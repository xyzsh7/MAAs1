##################################
## ASSOCIATION RULES
library(arules)
library(arulesViz)
library(utils)
library(ggplot2)
library(dplyr)
library(psych)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

library(plyr)

Transact.df<-read.csv("F:/study/CRM analysis/Association Rule/Agg2010.csv",stringsAsFactors=FALSE)

Transact.df <- Transact.df[order(Transact.df$panid),]

# When doing the assignment, you'll need to change the aggregation features as well as "product" feature (see slides)
#  With this dataset, you can change between CATEGORY_DESCRIPTION, SUB_CATEGORY_DESCRIPTION and TRANSACTION_TYPE_DESCRIPTION

# Transact_item_ry=Transact.df[Transact.df$L2=='RFG YOGURT',]
Transact_item <- ddply(Transact.df,c("panid","week",'minute'), 
                       function(df)paste(df$L2, 
                                         collapse = ","))

# Get rid of extraneous info (which implicitly treats all trips as independent)

Transact_item$panid <- NULL
Transact_item$week <- NULL
Transact_item$minute <- NULL

# Save processed transactions

write.csv(Transact_item,"F:/study/CRM analysis/Association Rule/Transact_item.csv", quote = FALSE, row.names = TRUE)

# Read processed transactions

Transact_tx = read.transactions(file="F:/study/CRM analysis/Association Rule/Transact_item.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);

# Strip quotes if they exist

Transact_tx@itemInfo$labels <- gsub("\"","",Transact_tx@itemInfo$labels)


# EDA ---------------------------------------------------------------------

#data analysis: absolute freq
itemFrequencyPlot(Transact_tx, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

#data anlysis: realtive frequency
itemFrequencyPlot(Transact_tx, topN=15, type="relative", col="lightcyan2", xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")

# parameter plot ----------------------------------------------------------

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01,0.005)
confidenceLevels <- c(0.6, 0.5, 0.4, 0.35, 0.3, 0.25, 0.2, 0.15, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  a=apriori(Transact_tx, parameter=list(sup=supportLevels[1], 
                                        conf=confidenceLevels[i], target="rules"))
  a<-as(a,"data.frame")
  a_ry=subset(a, grepl(glob2rx("{*} => {*YOGURT*}"), a$rules))
  rules_sup10[i] <- nrow(a_ry)
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  a=apriori(Transact_tx, parameter=list(sup=supportLevels[2], 
                                        conf=confidenceLevels[i], target="rules"))
  a<-as(a,"data.frame")
  a_ry=subset(a, grepl(glob2rx("{*} => {*YOGURT*}"), a$rules))
  rules_sup5[i] <- nrow(a_ry)
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  a=apriori(Transact_tx, parameter=list(sup=supportLevels[3], 
                                        conf=confidenceLevels[i], target="rules"))
  a<-as(a,"data.frame")
  a_ry=subset(a, grepl(glob2rx("{*} => {*YOGURT*}"), a$rules))
  rules_sup1[i] <- nrow(a_ry)
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  a=apriori(Transact_tx, parameter=list(sup=supportLevels[4], 
                                        conf=confidenceLevels[i], target="rules"))
  a<-as(a,"data.frame")
  a_ry=subset(a, grepl(glob2rx("{*} => {*YOGURT*}"), a$rules))
  rules_sup0.5[i] <- nrow(a_ry)
  
}




# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found including yougurt", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found including yougurt", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found including yougurt", 
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found including yougurt", 
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found including yougurt in consequence", 
       title="Apriori algorithm with different support levels and confident levels") +
  theme_bw() +
  theme(legend.title=element_blank())




# Most of these are fairly intuitive but this won't always be the case


# chosen rules ------------------------------------------------------------

# Create rules - Note these two parameters sup = support and conf = confidence can be adjusted to get more or fewer rules
#sup = 0.007 conf = 0.3
Transact_rules <- apriori(Transact_tx,parameter = list(sup = 0.007, conf = 0.3,target="rules"));

saverules<-as(Transact_rules,"data.frame")

saverules
saverules_ry=subset(saverules, grepl(glob2rx("{*} => {*YOGURT*}"), saverules$rules))
saverules_ry=saverules_ry[order(saverules_ry$lift,decreasing=TRUE),]
saverules_ry[1:3,]

