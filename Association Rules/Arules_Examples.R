library(arules)

data(Groceries)

class(Groceries)

isS4(Groceries)
slotNames(Groceries)

# Groceries@transactionInfo
# A data frame with vectors of same length as number of transactions

Groceries@data
# A binary incidence matrix that indicates which item labels appear 
# in every transaction

Groceries@itemInfo
# A data frame to store item labels

Groceries@itemInfo[1:15,]

itemsets <- apriori(Groceries, 
                    parameter = list(minlen=1, maxlen=1,
                                     support=0.02, 
                                     target="frequent itemsets"))

summary(itemsets)

inspect(head(sort(itemsets, by="support"),10))


# minlen and maxlen changed

itemsets <- apriori(Groceries, parameter = list(minlen=1, maxlen=2,
                                                support=0.02, target="frequent itemsets"))
inspect(head(sort(itemsets, by="support"),20))


itemsets <- apriori(Groceries, 
                    parameter = list(minlen=2, maxlen=2,
                                                support=0.02, target="frequent itemsets"))
inspect(head(sort(itemsets, by="support"),10))

# Rules Display
rules <- apriori(Groceries, 
                 parameter = list(   support=0.001, 
                                  confidence=0.6,
                                      target="rules"))

inspect(head(sort(rules,by="lift"),10))


inspect(head(sort(rules,by=c("confidence"))))

summary(rules)

# Visualizing Rules

library(arulesViz)

plot(rules)

head(rules@quality)

plot(rules@quality)

head(quality(rules))

confidentRules <- rules[quality(rules)$confidence > 0.9 
                        & quality(rules)$support > 0.001
                        & quality(rules)$lift > 1.5]

inspect(head(sort(confidentRules,by="lift"),5))

# View by Support
#plot(confidentRules, method="matrix",
#     measure = "support")

# View by Confidence
#plot(confidentRules, method="matrix",measure = "confidence")

# View by Confidence
#plot(confidentRules, method="matrix",measure = "confidence",
#     control=list(reorder=TRUE),interactive=TRUE)

# View by Lift Ratio
#plot(confidentRules, method="matrix",measure = "lift",
#     control=list(reorder=TRUE))

# View by Lift Ratio and Conficence
#plot(confidentRules, method="matrix",measure = c("lift","confidence"),
#     control=list(reorder=TRUE),interactive=TRUE)

highLiftRules <- head(sort(confidentRules,by="lift"),5)
plot(highLiftRules, 
     method="graph",control=list(type="items"))


inspect(head(sort(rules,by="lift"),5))


## Subsetting for tropical fruit
## select all rules then %in
rules.sub <- subset(rules, 
                    subset = rhs %in% "tropical fruit" )
inspect(rules.sub)


rules.sub <- subset(rules, subset = rhs %in% "tropical fruit" &
                                    lhs %in% "root vegetables")
inspect(rules.sub)


rules.sub <- subset(rules, subset = rhs %in% "tropical fruit" &
                      lhs %in% c("root vegetables","turkey"))
inspect(rules.sub)

## select only rules then %ain%
rules.sub <- subset(rules, subset = rhs %in% "tropical fruit" &
                      lhs %ain% c("root vegetables","turkey"))
inspect(rules.sub)

