library(arules)
trns <- read.csv("F:\\Statistics\\15. Association Rules\\trans.csv")


demog <- as(trns,"transactions")

class(demog)
demog@itemInfo
demog@data
rules <- apriori(demog, 
                 parameter = list(   support=0.15, 
                                     confidence=0.8,
                                     target="rules"))


inspect(head(sort(rules,by="lift"),10))
