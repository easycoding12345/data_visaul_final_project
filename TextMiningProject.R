#required packages
setwd("C:/Users/hp/Desktop/All auaf courses/spring 2024/data mining/dataMining/textMiningProject")
install.packages("arules")
install.packages("arulesViz")
install.packages("rJava")
install.packages("tidyverse")
stats::filter(x, filter_value)
stats::lag(x, lag_value)
install.packages("conflicted")
library(conflicted)
conflict_prefer("filter", "stats")
conflict_prefer("lag", "stats")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(tm)
library(SnowballC)
library(dplyr)
library(qdap)
Sys.setenv(JAVA_HOME = "C:/Users/hp/Desktop/java/jdk-22.0.1") # Update the path according to your JDK installation
install.packages("rJava", type = "source", INSTALL_opts = '--no-multiarch')
# Install rJava again
install.packages("rJava", type = "source", INSTALL_opts = '--no-multiarch')


#*******************************************************************
#*******************************************************************
##upload the dataset 
iphone_14_proMax=read.csv("AmazonReviewsIphone14ProMax.csv", stringsAsFactors = FALSE)
View(iphone_14_proMax)
str(iphone_14_proMax)

frequency of the words in the Review column 
fr=freq_terms(iphone_14_proMax)
fr
plot(fr)

##Creat documents
iphone_14_proMax_scr=Corpus(VectorSource(iphone_14_proMax$Review))
iphone_14_proMax_scr[[2]]$content

#1. trun all words to lower case
iphone_14_proMax_scr= iphone_14_proMax_scr %>%
  tm_map(tolower)
iphone_14_proMax_scr[[2]]$content

#2. Remove punctuations
iphone_14_proMax_scr = iphone_14_proMax_scr %>%
  tm_map(removePunctuation)
iphone_14_proMax_scr[[2]]$content

#3. removing the stopwords 
stopwords("english")
iphone_14_proMax_scr =iphone_14_proMax_scr %>%
  tm_map(removeWords, c("iphone", "review","get", "phone", "just","14 pro Max", stopwords("english")))
iphone_14_proMax_scr[[2]]$content

#4. Remove numbers
iphone_14_proMax_scr = iphone_14_proMax_scr %>%
  tm_map(removeNumbers)
iphone_14_proMax_scr[[2]]$content

#5. stripping the white space
iphone_14_proMax_scr= iphone_14_proMax_scr %>%
  tm_map(stripWhitespace)
iphone_14_proMax_scr[[2]]$content

#6. Stemming
iphone_14_proMax_scr = iphone_14_proMax_scr %>%
  tm_map(stemDocument)

iphone_14_proMax_scr[[2]]$content
fr1=freq_terms(iphone_14_proMax_scr, 10)
plot(fr1)
View(iphone_14_proMax_scr)

#Feature Extraction 
iphone_14_proMax_freq=DocumentTermMatrix(iphone_14_proMax_scr)

dim(iphone_14_proMax_freq)    #49 rows and 463 variables

inspect(iphone_14_proMax_freq)

inspect(iphone_14_proMax_freq)[1:4, 1:3]   #document 1:4 columns 1:3

findFreqTerms(iphone_14_proMax_freq)
l=findFreqTerms(iphone_14_proMax_freq, lowfreq = 8)
length(l)
##we have many features with too many zeros, high sparsity
iphone_14_proMax_sparse=removeSparseTerms(iphone_14_proMax_freq, 0.90)  
##keep only the terms that appears in 10% or more of the feedback/documents/columns
dim(iphone_14_proMax_sparse)

inspect(iphone_14_proMax_sparse)
##convert it to dataframe 
iphone_14_proMax_review=as.data.frame(as.matrix(iphone_14_proMax_sparse))
View(iphone_14_proMax_review)
dim(iphone_14_proMax_review)

###visualize the freq of terms

iphone_14_proMax_names=colnames(iphone_14_proMax_review)


iphone_14_proMax_freq=c()

for (i in 1:37){
  iphone_14_proMax_freq[i]=sum(iphone_14_proMax_review[,i])
}
iphone_14_proMax_freq
#barplot( iphone_14_proMax_freq,
        #col=rainbow(37), 
        #names.arg = iphone_14_proMax_names)

iphone_data <- data.frame(names = iphone_14_proMax_names, freq = iphone_14_proMax_freq)

# Plot using ggplot2
ggplot(iphone_data, aes(x = names, y = freq, fill = names)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow(37)) +  # Setting colors
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  # Rotating x-axis labels
  labs(title = "Frequency of iPhone 14 Pro Max Features", x = "Features", y = "Frequency")
####Sentiment analysis 
#create your sentiment variable
View(iphone_14_proMax)
rate=iphone_14_proMax$Rating
View(iphone_14_proMax_review)
iphone_14_proMax_review = iphone_14_proMax_review %>%
  mutate(y=ifelse(rate>4, "Positive","Negative"))
View(iphone_14_proMax_review)

##which terms derive positive rating
#decision tree
install.packages(c("rpart", "rpart.plot"))
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
iphone_14_proMax_sent=rpart(y~.,
                data = iphone_14_proMax_review,
                method = "class")
prp(iphone_14_proMax_sent)
rpart.plot(iphone_14_proMax_sent, extra = 106)


#model

sb=iphone_14_proMax_review %>%
  select(batteri)
##sb = iphone_14_proMax_review$batteri
View(sb)
table(sb)
##you may use this to make prediction model

                                            




