---
title: "Sykes - Mushroom data"
author: "Venkata sai"
date: "November 2, 2016"
output: word_document
---


list of all librarries used

library(vcd)
library(caret)
library(klaR)
library(e1071)
library(caTools)
library(ggplot2)
libraray(rpart)
library(rpart.plot)
```{r}
mushroom <- read.csv("E:/Sykes/mushroom.csv")
# Exploratory data analysis
summary(mushroom)
View(mushroom)
str(mushroom)
table(mushroom$PE)
# The data set has 4208 edible mushrooms and 3916 poisonous mushrooms
str(mushroom$stalk.color.below.ring)
#To find out missing values 
colSums(is.na(mushroom))

#The below command resulted that stalk root variable has 2480 missing values
sapply(mushroom, function(p) table(p=='?') )

# Missing value treatment.
percentb<-nrow(subset(mushroom,mushroom$stalk.root=='b'))/nrow(mushroom)
percentb
#   the most frequent value  'bulbous' - 'b' which is 3776 out of 8124 which is 46%. hence Replacing the missing value in stalk.root with b
mushroom$stalk.root[mushroom$stalk.root == '?']<-'b'

#converting stalk root as a factor again
mushroom$stalk.root<-factor(mushroom$stalk.root)

#Reducing dimensionality to make the model a simple one.
#The simple the better
#The variable veil type is partial - p for all the mushrooms
# This variable doesnt explain any variance hence removing the variable
mushroom$veil.type<-NULL
#Most of the veil color are w= 7924, n,o,96 each y 8 and hence in the intension to reduce dimensionality
mushroom$veil.color<-NULL

#Response rate 
# understanding the response rate to make some decisions about variables
sapply(mushroom,function(x) prop.table(table(x,mushroom$PE),1)*100)

#Frequency
sapply(mushroom,function(x) (table(x)/nrow(mushroom)*100))

#A better way to collapse too many categories is to make use of both response rate and frequency and then bin them accordingly

#Collapsing categories of variables with too many categories ,based on response rate of poisonous mushroom and frequency of each category
sapply(mushroom,function(x)cbind((prop.table(table(x,mushroom$PE),1)*100)[,2],(table(x)/nrow(mushroom)*100)))

# The following Analysis takes the output of the response rate and frequency in  making various decisions about variables.


#Interpreting the results of above command gives us that the variable spore.print.color has same response rate and frequency for k,n and u,y,b,o categories
#The results are quite convincing because black-k and brown-n spore clolored mushroom are similar atleast with respect to spores.Replacing with new levels by combining similar categories.

mushroom$spore.print.color[mushroom$spore.print.color == 'k']<-'n'
mushroom$spore.print.color[mushroom$spore.print.color %in% c('r','o','b','y')]<-'u'
mushroom$spore.print.color<-factor(mushroom$spore.print.color)

#interpretation 2

#The proportions of response rate and frequency of stalk color above ring and below ring are same. lets us reduce the levelsof these of factors and also the dimensionality by removing one of the variables

#binning b,c,y together as b and e,g,o as o. as both of them have almost equal response rates 

mushroom$stalk.color.below.ring[mushroom$stalk.color.above.ring %in% c('c','y')]<-'b'
mushroom$stalk.color.below.ring[mushroom$stalk.color.above.ring %in% c('g','e')]<-'o'
mushroom$stalk.color.below.ring<-factor(mushroom$stalk.color.below.ring)

#similar is the case with stal
mushroom$cap.color[mushroom$cap.color %in% c('r')]<-'u'
mushroom$cap.color[mushroom$cap.color %in% c('g')]<-'n'
mushroom$cap.color<-factor(mushroom$cap.color)

#Removing stalk color above ring as stalk color above ring and below ring are same for significant portion which is evident form the below command
table(mushroom$stalk.color.above.ring,mushroom$stalk.color.below.ring)
#interpreting the result of this command reveals that most of the values other than the diagonal elements are zeros.Hence the decision.
mushroom$stalk.color.above.ring<-NULL

#interpretation3
#similarly gill color based on response rate and frequency
#gill.color
#b,r<-r
#e,o<-o
#g,h<-g
mushroom$gill.color[mushroom$gill.color %in% c('r')]<-'b'
mushroom$gill.color[mushroom$gill.color %in% c('e')]<-'o'
mushroom$gill.color[mushroom$gill.color %in% c('h')]<-'g'
mushroom$gill.color<-factor(mushroom$gill.color)


#odor

mushroom$odor[mushroom$odor %in% c('s','y','m')]<-'c'
mushroom$odor[mushroom$odor %in% c('a')]<-'l'
mushroom$odor<-factor(mushroom$odor)
str(mushroom$odor)


library(ggplot2)

#Visualizing data to understand trends and associations.
odorcounts<-as.data.frame(table(mushroom$odor,mushroom$PE))
names(odorcounts)<-c("odor","PE","Response")

ggplot(data = odorcounts, aes(x = PE, y = Response, fill = odor)) + geom_bar(stat="identity") +
ggtitle("Odor and Poisonous/Edible")

#The above plot determines how odor plays an important role in determing the status of mushroom.
# THe plot tell us that if a mushroom has a category of f,c  it may be poisonous and edible if its n.
# We need a statistical proof to accept this. Let us perform a chi sq test later to see the statistical significance

#Capcolor and Poisonous/Edible
capcolorcounts<-as.data.frame(table(mushroom$cap.color,mushroom$PE))
names(capcolorcounts)<-c("capcolor","PE","Response")

ggplot(data = capcolorcounts, aes(x = PE, y = Response, fill = capcolor)) + 
geom_bar(stat="identity")+
ggtitle("Capcolor and Poisonous/Edible")
#Cap color doesnt significantly seperate poisonous and edible mushroom as evident from the above ggplot


# Performinh chi square test of independence to understand the associal of all variables in determining whether the mushroom is a poisonous (p) or edible (e)
sapply(mushroom, function(x) chisq.test(table(x,mushroom$PE)))


'''The p values of all the tests are very small and we reject the null hypothesis that variables are independent in determining PE of a mushroom and tell us that all the predictors are important in determining the edible and poisonous of a mushroom'''


#Since we found out that odor plays an important role from visualisations let us understand in detail by doing a mosiac plot to understand correlarions

chisq.test((table(mushroom$odor,mushroom$PE)))
# Chi sq test says that there is association between 
mosaicplot(table(mushroom$PE,mushroom$odor) ,shade = TRUE, type = c("pearson", "deviance", "FT"), main = "odor and poisonous/edible")

#Mosaic plot indicates that there is a high strength of association between odor and PE variable with n as edible and f,c as poisonous mushrooms.
#Now we have the statistical proof to say that odor determines poisonous of a mushroom

library(vcd)
library(caret)
library(klaR)
library(e1071)
library(caTools)

# Split into train and test data with 70% train and 30 % test
split<- sample.split(mushroom$PE,SplitRatio = 0.7)

mushroomtrain<-subset(mushroom,split==TRUE)

mushroomtest<- subset(mushroom,split == FALSE)

# BASE case

str(mushroomtrain$stalk.color.below.ring)

table(mushroomtrain$PE)
table(mushroomtest$PE)
1262/nrow(mushroomtest)
#baseline model predicts all the mushrooms as edible
#baseline model accuracy : 51.7%
# Suppose if a balanced set is taken to train then the base case acuuracy would be 50%m because the probability if edible mushroom is 0.5 and the probability of poisonous mushroom is 0.5.

#NaiveBayes
#This model is chosen because it considers all the properties as independent in determining the PE of a mushroom.
modelnb <- NaiveBayes(PE ~ . , data=mushroomtrain)
summary(modelnb)
str(modelnb)
pred=predict(modelnb,newdata=mushroomtest,type="response")
confusionMatrix(pred$class,mushroomtest$PE)
'''
The accuracy of the model is 95%. However, the main target is to mininise the cases where actual status is poisonous and predicted is edible. The cost of making this error is more because if you classify a poisonous mushroom as edible and if some one consumes ,there is a risk of life.
In Naive bayes there are 85 such cases.
'''

#Let us try and build a decision tree to see how it performs

#decision tree
library(rpart)
library(rpart.plot)
#A decision tree with some important variables from chi square test of independence has been built to see how it performs.
# various subsets of variables have been performed and the best are shown here.
mushrpart<-rpart(PE ~ odor+spore.print.color+stalk.color.below.ring, data = mushroomtrain, method= "class", control = rpart.control(cp = 0.0001))

predictrpart<- predict(mushrpart,newdata = mushroomtest, type="class")
#Visualising the tree
prp(mushrpart)
confusionMatrix(predictrpart,mushroomtest$PE)

# The confusion matrix reveals that there are only 3 cases where we classify a poisonous mushroom as edible mushroom

#Let us build a model with all the variables to see how it performs

mushrpart2<-rpart(PE ~ ., data = mushroomtrain, method= "class", control = rpart.control(cp = 0.0001))

predictrpart2<- predict(mushrpart2,newdata = mushroomtest, type="class")
summary(predictrpart)
prp(mushrpart)
confusionMatrix(predictrpart2,mushroomtest$PE)


#This model give a 0 cases where we classify a poisonous mushroom as edible mushroom and hence beats all the other models.
prp(mushrpart)

The above visual shows that odod plays a significant role in determining which we initially hypotheisized by visualisation and later statistically proved by chi sq and mmosiac plot. Such is the power of exploratory data analysis and inital data exploration.

# Now there would be health problems caused by poisonous mushrooms

```



