#assosication rule
#mushroom dataset
mush <- read.csv ("F:\\ML_Class\\Machine Learning\\Cases\\mushroom\\mushroom.csv")
mushroom <- as(mush,"transactions")
#forming rules
rules <- apriori(mushroom, 
                 parameter = list(support=0.2,confidence = 0.7, 
                                  target="rules"))

summary(rules)

inspect(head(sort(rules, by="lift"),10))

#ploting the graph
library(arulesViz)
highLiftRules <- head(sort(rules,by="lift"),5)
plot(highLiftRules, 
     method="graph",control=list(type="items"))

inspect(head(sort(rules, by="lift"),5))

