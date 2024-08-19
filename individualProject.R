#Data suggest that on average 3 kids are absent from
# a kindergarten group every day. The average is 6 one 
# day before/after a holiday.  We choose a day (not holiday)
#  at random. Compute the chance of any number of missing from 0 to 10. 
#  What if the day chosen is either before or after a holiday.
help("dpois")
#calculating the probability of missing children on regular days
#lambda = 3
# k = x (0 to 10)
x= seq(0,10)
x
y = dpois(x, 3)
y
#calculating the probability of missing children on one day before/after holidays
#lambda = 6
# k = x (0 to 10)

x1= x= seq(0,10)
x1
y1 = dpois(x, 6)
y1

library(ggplot2)
dt = as.data.frame(cbind(x, y))
dt2 = as.data.frame(cbind(x1, y1))
grph = ggplot()+
       geom_line(data = dt, aes(x=x, y=y,linetype ="dashed", color = "black"))+
       geom_point(data = dt, aes(x=x, y=y, color="red"))+
       geom_line(data = dt2, aes(x=x1, y=y1,linetype="dashed", color = "red"))+
       geom_point(data = dt2, aes(x=x1, y=y1, color="black"))+
       scale_color_manual(name="variables", labels=c("regular day", "before/after holidays"), values = c("black", "red"))+
       labs(x="absent kids", y="the probability of absent kids")

grph


#cdf
x2= x= seq(0,10)
x2
y2 = ppois(x, 3, lower.tail = TRUE)
y2
# after/before holidays
x3= x= seq(0,10)
x3
y3 = ppois(x, 6, lower.tail = TRUE)
y3

dt3= as.data.frame(cbind(x2, y2))
dt3

dt4= as.data.frame(cbind(x3, y3))
dt4

grph2 = ggplot()+
  geom_line(data = dt3, aes(x=x2, y=y2,linetype ="dashed", color = "black"))+
  geom_point(data = dt3, aes(x=x2, y=y2, color="red"))+
  geom_line(data = dt4, aes(x=x3, y=y3,linetype="dashed", color = "red"))+
  geom_point(data = dt4, aes(x=x3, y=y3, color="black"))+
  scale_color_manual(name="variables", labels=c("regular day", "before/after holidays"), values = c("black", "red"))+
  labs(x="absent kids", y="the probability of absent kids")

grph2
