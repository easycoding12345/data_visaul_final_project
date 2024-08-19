data = read.csv("C:/Users/hp/Desktop/All auaf courses/ITC255/myData.csv")
View(dat
names(data)
#H0= No association between gender and Height 
#H1= Association between gender and height 
#Alpha = 0.05
t.test(data$Hieght..in.cm.~data$Gender.M.F.)
#p-value = 1.407e-07<0.05
#Decision = Reject H0 in favor of H1: There is strong statistical evidence that the variables are associated in the population.
#Here, I want to check the association between weight and height ######H0= No linear association between weight and Height #####H1= Linear Association between weight and height #####Alpha = 0.05
cor.test(data$Weight..in.Kg., data$Hieght..in.cm.) cor(data$Weight..in.Kg., data$Hieght..in.cm.)
# pvalue = 0.000002498 < 0.05
# correlation = 0.7105689
#Decision = Reject H0 in favor of H1: There is strong statistical evidence that the variables have strong linear association in the population.

