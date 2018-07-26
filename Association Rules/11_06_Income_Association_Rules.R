#Association rules
library(arules)
data("Income")

#forming rules
rules <- apriori(Income, 
                    parameter = list(support=0.2,confidence = 0.8, 
                                     target="rules"))

summary(rules)

inspect(head(sort(rules, by="lift"),10))

#ploting the graph
library(arulesViz)
highLiftRules <- head(sort(rules,by="lift"),5)
plot(highLiftRules, 
     method="graph",control=list(type="items"))

inspect(head(sort(rules, by="lift"),5))

