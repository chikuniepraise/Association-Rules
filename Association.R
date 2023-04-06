# Perform Association and Visualize the association on a dataset

# loading the required libraries necessary for Association Rules
require(arules)
require(arulesViz)

#reading the required dataset
store_trans <- read.transactions("Grocery Store.csv", sep=",")
inspect(store_trans[1:3]) #lets inspect the first three rows

summary(store_trans)

# SUPPORT is used in association rule to denote how frequent an item occurs in 
#...the data. in essence, in what percentage/proportion does an item occur

# to look at the SUPPORT, we use itemfrequency

itemFrequency(store_trans[,1]) # looking for the support for the first item.

# if you times it by the number of transaction, we would know how many times, 
# it occurs in the data set.

0.003558719*9835 #this shows that abrasive cleaner occurs 35 times in the dataset.


#looking at the first six items
itemFrequency(store_trans[,1:6]) 


#plotting support using ItemFrequencyPlot.
itemFrequencyPlot(store_trans, support= 0.20)

itemFrequencyPlot(store_trans, support= 0.10)# plotting items that have a minimum of 
#...10% frequency

# We can also see the top 5 items on the dataset
itemFrequencyPlot(store_trans, topN=5)#this shows items with the top5 support

# CONFIDENCE is a measure of the proportion of transactions where the presence
# of an item or set of items results in the presence of another set of item.

# It is like a conditional probability that states that what is the probability
# of an item being purchased with another item

#for example, if i buy item A and B, how likely will i buy item C also?


#High confidence implies that a rule is likely to occur or good predictive power 

#In essence, We want to come up with association rules that has high confidence 
#...and support

# to check for association, we use the apriori function in the arules package

m1 <- apriori(store_trans) # we can see that the min confidence is 0.8 & support =0.1
m1 # it shows 0 set of rules, this is because, this is too high for our data set


#lets lower the confidence level.
m1 <- apriori(store_trans, parameter = list(support=.010, confidence = .50,
                                          minlen=2))
m1
summary(m1)

# To look at these rules, lets inspect m1
inspect(m1)

# lift is basically the confidence over the support
# think of lift as how much an item is likely to be purchased relative to its 
#...general purchase rate.

# therefore, the lift shows that it is almost 1.5 times more likely for "other
#...vegetables" be purchased with whole milk than with other items in the store.

plot(m1, method = "grouped")

#LETS USE THE STORE_TRANS DATASET WITH ANOTHER SET OF SUPPORT & CONFIDENCE

summary(store_trans)

apriori(store_trans, parameter= list(support= 0.002, confidence=0.5)) -> rule_1
rule_1
summary(rule_1)

#lets take a glance of the first five rules or association.
inspect(head(rule_1, 5))

#lets sort them on the basis of lift. so we know the ones with higher frequencies.
inspect(sort(rule_1, by ="lift"))

#these are too many, so lets use the "head" function to see the first-five rules

inspect(head(sort(rule_1, by ="lift"), 5))

# to plot these rules:
plot(rule_1) 
#this shows every rule, but it is not readable.

#lets plot this in a different way, lets group everything.

plot(rule_1, method = "grouped")

# it shows all the items in the lhs and rhs group.
# the size of the bubble represents support and the color represents lift
#therefore, the larger the bubble, the greater the support and the darker the bubble,
#...the greater the lift.
