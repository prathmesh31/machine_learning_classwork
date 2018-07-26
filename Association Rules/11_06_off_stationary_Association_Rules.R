#Association rules
#off_stationary data
txn<-read.transactions ("F:\\ML_Class\\Machine Learning\\Association Rules\\off_stationary.csv",
                        rm.duplicates = FALSE,format="single",sep=",",cols=c(1,2))

#forming rules
rules <- apriori(txn, 
                 parameter = list(support=0.05,confidence = 0.7, 
                                  target="rules"))

summary(rules)

inspect(head(sort(rules, by="lift"),10))

#ploting the graph
library(arulesViz)
highLiftRules <- head(sort(rules,by="lift"),3)
plot(highLiftRules, 
     method="graph",control=list(type="items"))

inspect(head(sort(rules, by="lift"),3))

