#install.packages("arules")
#install.packages("arulesViz")
require(arules)


#It is an unsupervised learning technique readily used for transactional data(big data)
#We will try to find interesting relationships in the store.
# in the data, the data is mainly unstructured and therefore, there are many 0s
# because people do not buy the same volume of things in a supermarket.


data(package="arules")# datasets in the arules package
data("Groceries")

help(Groceries)

summary(Groceries)# summary summarizes the data. for element length distribution,
# we can see that, 1 item was purchased 2159 times in a month, to the last, which
# was an item was bought 32 times in a day.

# now you can see that the min=1 and max=32, now we can see the mean and median
# the labels just shows the first three items in our matrix or dataset.

# for sparse matrix, to see the transactions or rows is to use the inspect function.

inspect(Groceries[1:3]) #lets inspect the first three rows
#this means that 1 person bought the first row, another bought the items in the 
#...second row and then another bought the items in the third row.


# SUPPORT is used in association rule to denote how frequent an item occurs in 
#...the data. in essence, in what percentage/proportion does an item occur

# to look at the SUPPORT, we use itemfrequency

itemFrequency(Groceries[,1]) # looking for the support for the first item.
# this shows that frankfurter occurs in 5% proportion

# if you times it by the number of transaction, we would know how many times, 
# it occurs in the dataset.

0.05897306*9835 #this shows that frankfurter occurs 580 times in the dataset.


#looking at the first six items
itemFrequency(Groceries[,1:6]) # looking for the support for the first item.

# sausage        
0.093950178*9835 #this shows that sausage occurs 924 times in the dataset.

# liver loaf                
0.005083884*9835 ##this shows that liver loaf occurs 50 times in the dataset.

# ham
0.026029487*9835 #this shows that ham occurs 256 times in the dataset.

#meat 
0.025826131*9835 #this shows that meat occurs 254 times in the dataset.

#finished products 
0.006507372*9835 #this shows that finished product occurs 64 times in the dataset.


#plotting support using ItemFrequencyPlot.
itemFrequencyPlot(Groceries, support= 0.20)# plotting items that have a minimum
#...of 20% frequency
# this means that only whole milk hasa min of 20% of transactions.

itemFrequencyPlot(Groceries, support= 0.10)# plotting items that have a minimum of 
#...10% frequency

# We can also see the top 5 items on the dataset
itemFrequencyPlot(Groceries, topN=5)#this shows items with the top5 support

# CONFIDENCE is a measure of the proportion of transactions where the presence
# of an item or set of items results in the presence of another set of item.

# It is like a conditional probability that states that what is the probability
# of an item being purchased with another item

#for example, if i buy item A and B, how likely will i buy item C also?


#High confidence implies that a rule is likely to occur or good predictive power 

#In essence, We want to come up with association rules that has high confidence 
#...and support

# to check for association, we use the apriori function in the arules package
help("apriori")
m1 <- apriori(Groceries) # we can see that the min confidence is 0.8 & support =0.1
m1 # it shows 0 set of rules, this is because, this is too high for our data set


#lets lower the confidence level.
#we do not also want rules that are one item
m1 <- apriori(Groceries, parameter = list(support=0.07, confidence = .25,
                                          minlen=2))
m1 # shows there are set of two rules.
summary(m1)
# this shows that the two rules are of two items.

# To look at these rules, lets inspect m1
inspect(m1)

#this shows that customers who bought "other vegetables" were likely to buy
# whole milk and vice versa.
# they both had a support of 0.07.

#looking at the confidence level:
#{other vegetables} => {whole milk}  38%
#{whole milk} => {other vegetables}  29%

# lift is basically the confidence over the support
# think of lift as how much an item is likely to be purchased relative to its 
#...general purchase rate.

# therefore, the lift shows that it is almost 1.5 times more likely for "other
#...vegetables" be purchased with whole milk than with other items in the store.

plot(m1, method = "grouped")


#LETS USE THE GROCERIES STORES DATASET WITH ANOTHER SET OF SUPPORT & CONFIDENCE

summary(Groceries)

apriori(Groceries, parameter= list(support= 0.002, confidence=0.5)) -> rule_1
rule_1
summary(rule_1)

#lets take a glance of the first five rules or association.
inspect(head(rule_1, 5))

#lets sort them on the basis of lift. so we know the ones with higher frequencies.
inspect(sort(rule_1, by ="lift"))

#these are too many, so lets use the "head" function to see the first-five rules

inspect(head(sort(rule_1, by ="lift"), 5))

# to plot these rules:
library(arulesViz)
plot(rule_1) 
#this shows every rule, but it is not readable.

#lets plot this in a different way, lets group everything.

plot(rule_1, method = "grouped")

# it shows all the items in the lhs and rhs group.
# the size of the bubble represents support and the color represents lift
#therefore, the larger the bubble, the greater the support and the darker the bubble,
#...the greater the lift.


#remember there are many rules or association, now, lets set a rule with minlen of 5


apriori(Groceries, parameter= list(support= 0.002, confidence=0.5, minlen = 5)) -> rule_2
rule_2
summary(rule_2)


inspect(rule_2)

#inspecting the first 4 rules
inspect(head(rule_2, 4))

#here, the minlen is 5, the products in lhs is 4 plus rhs makes it 5.

# Plotting in group

plot(rule_2, method = "grouped") #this shows that size represents the support and the color
#...represents the lift.

# one item that has high support and high lift is citrus fruit and root vegetables.
# this shows that when a customer purchases items in lhs, there is a high probability that
# they purchase items in the rhs as well.



