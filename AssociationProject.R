setwd("C:/Users/hp/Desktop/All auaf courses/spring 2024/data mining/dataMining")
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
m=read.csv("Book1.csv") 
View(m)
q=as.matrix(m)  
q=as(q, "transactions")
View(q) 
inspect(q)
summary(q) 

itemFrequency(q) # here we can see that bread is the most common item
itemFrequency(q[,4])
itemFrequencyPlot(q)
itemFrequencyPlot(q, support=0.6)#Conf(X-->Y)=Pr(Y|X) :: the prob of Y Given X =Pr(X,Y)/Pr(X)=sup(X,Y)/sup(X)
####confidence Use the default
m1=apriori(q)
m1
inspect(m1)
q.rules=apriori(q, parameter = list(supp=0.1, conf=0.7, minlen=3))
#if bread goes with milk the chance of buying milk decrease
#bread and jam is the best product to be placed in one shelf
#minlen=is the number of items

#summary(q.rules)
inspect(q.rules)
plot(q.rules, method = "grouped")

