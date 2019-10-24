library(arules)
library(arulesViz)
data("AdultUCI")
adultDf<-AdultUCI
View(adultDf)

#Data Exploration
str(adultDf)
summary(adultDf)

#adultDf<-read.csv('imp_discretized.csv')
##Data Cleaning
#adultDf<-na.omit(adultDf)
#Education and education number provide the same information,we can remove any one of them.
adultDf<-adultDf[,-which(names(adultDf) %in% "education-num")]
#Missing values 
sapply(adultDf,function(x) sum(is.na(x)))
# Missing values by rows
which(apply(adultDf,1,function(x) sum(is.na(x)))> 0 )
#Missing values by columns
which(apply(adultDf,2,function(x) sum(is.na(x)))> 0)

#removing missing values
adultDf<-adultDf[!(is.na(adultDf$income)),]

#impute missing values
library(DMwR)
adultDf<-knnImputation(adultDf)
detach(package:DMwR)
#dicretization
adultDf$age_grp <- discretize(adultDf$age, method = "frequency", breaks = 3, 
                              labels = c("low", "medium", "high"), order = T)


adultDf$fnlwgt<-discretize(adultDf$fnlwgt,method = "frequency",breaks=3,labels = c("small","medium","large"),order=T)
adultDf$`hours-per-week`<-discretize(adultDf$`hours-per-week`,method = "cluster",breaks=4,labels=c("low","medium","high","very-high"),order=T)

adultDf$capital_gain<-cut(adultDf$`capital-gain`,c(-Inf,0,10000,99999),labels = c("Zero","Below_10000","Above_10000"))
adultDf$capital_loss<-cut(adultDf$`capital-loss`,c(-Inf,0,2000,Inf),labels=c("Zero","Below_2000","Above_2000"))

#to check with dicretized properly for capital gain and loss
table(which(adultDf$`capital-gain`==0)==which(adultDf$capital_gain=='Zero'))
table(which(adultDf$`capital-loss`==0)==which(adultDf$capital_loss=='Zero'))

#write.csv(adultDf,"imp_dicretized.csv")

#collapsing variables with many factor levels
list_countries<-levels(adultDf$`native-country`)
levels(adultDf$`native-country`)<-list("United Stated"=list_countries[39],"Other"=list_countries[-39])

list_occupation<-levels(adultDf$occupation)
levels(adultDf$occupation)<-list("White Collar"=list_occupation[c(1,4,10,13)],"Blue Collar"=list_occupation[c(3,5,6,7,9,11,12,14)],"Armed Forces"=list_occupation[2],"Other Servive"=list_occupation[8])

list_marital_status=levels(adultDf$`marital-status`)
levels(adultDf$`marital-status`)<-list("Married"=list_marital_status[c(2,3,4)],"Unmarried"=list_marital_status[c(1,5,6,7)])

list_education<-levels(adultDf$education)
levels(adultDf$education)<-list("12th and Below"=list_education[c(1,2,3,4,5,6,7,8)],"High School Grad"=list_education[9],"Other Training"=list_education[c(10,11,12)],"University"=list_education[c(13,14,15,16)])

list_workclass<-levels(adultDf$workclass)
levels(adultDf$workclass)<-list("Government"=list_workclass[c(1,2,7)],"Self Employed"=list_workclass[c(5,6)],"Private"=list_workclass[4],"Never Worked or Without Pay"=list_workclass[c(3,8)])

list_relationship<-levels(adultDf$relationship)
levels(adultDf$relationship)<-list("In Family"=list_relationship[c(1,6)],"Not in Family"=list_relationship[c(2,5)],"Own Child"=list_relationship[4],"Other-relative"=list_relationship[3])
#Data Exploration

library(tidyverse)
library(ggplot2)
#race Income
race_income<-adultDf%>%group_by(race,income)%>%summarise(count=n())
ggplot(race_income,aes(x=income,y=count))+geom_bar(stat='identity',aes(fill=race))
#sex income
sex_income<-adultDf%>%group_by(sex,income)%>%summarise(count=n())                                            
ggplot(sex_income,aes(x=income,y=count))+geom_bar(stat='identity',aes(fill=sex))
#marital status income
colnames(adultDf)[which(colnames(adultDf) %in% "marital-status")]<-"maritalstatus"
marital_income<-adultDf%>%group_by(maritalstatus,income)%>%summarise(count=n())
ggplot(marital_income,aes(x=income,y=count,fill=maritalstatus))+geom_bar(stat='identity')
#age income
age_income<-adultDf%>%group_by(age_grp,income)%>%summarise(count=n())
ggplot(age_income,aes(x=income,y=count,fill=age_grp))+geom_bar(stat='identity')
# hours per week worked,income
hours_income<-adultDf%>%group_by(`hours-per-week`,income)%>%summarise(count=n())
ggplot(hours_income,aes(x=income,y=count,fill=`hours-per-week`))+geom_bar(stat='identity')
# Education by income
education_income<-adultDf%>%group_by(education,income)%>%summarise(count=n())
ggplot(education_income,aes(x=income,y=count,fill=education))+geom_bar(stat='identity')

#Occupation Income
occupation_income<-adultDf%>%group_by(occupation,income)%>%summarise(count=n())
ggplot(occupation_income,aes(x=income,y=count,fill=occupation))+geom_bar(stat='identity')

#Native Country Income
native_income<-adultDf%>%group_by(`native-country`,income)%>%summarise(count=n())
ggplot(native_income,aes(x=income,y=count,fill=`native-country`))+geom_bar(stat='identity')

#capital_gain_income
capital_gain_income<-adultDf%>%group_by(capital_gain,income)%>%summarise(count=n())
ggplot(capital_gain_income,aes(x=income,y=count,fill=capital_gain))+geom_bar(stat='identity')

#relationship_income
relationship_income<-adultDf%>%group_by(relationship,income)%>%summarise(count=n())
ggplot(relationship_income,aes(x=income,y=count,fill=relationship))+geom_bar(stat='identity')

##Association Rules Mining

#Convert into transactional dataset
fact_var<-sapply(adultDf, is.factor)
adult_trans<-as(adultDf[,fact_var],"transactions")
inspect(head(adult_trans,3))

#function to subset rules and remove redundancy
subset_rules<-function(Rules){
  #Removing redundant rules
  sub_rules <- which(colSums(is.subset(Rules,Rules)) > 1)
  Rules <- sort(Rules[-sub_rules], by = "lift", descreasing = T)
  return(Rules)
  Rules<-subset(Rules,subset=(lift >1))
}


#Rules with default setting
rules<-apriori(adult_trans, parameter = list(support = 0.4, confidence = 0.9, minlen = 3))
subset_rules(rules)
inspect(head(rules,5))


#frequent items
frequent_items <- eclat(adult_trans, parameter = list(support = 0.01, minlen = 2))
inspect(head(frequent_items))

#item freq plot
itemFrequencyPlot(adult_trans, topN = 10, type = "absolute", main = "Item frequency")

##Rules to predict income
#Rules to predict large income

income_large_rules<-apriori(adult_trans,parameter = list(support = 0.01, confidence = 0.3),appearance = list(default="lhs",rhs="income=large"))
subset_rules(income_large_rules)
inspect(head(income_large_rules,10))

#Rules to predict small income

income_small_rules<-apriori(adult_trans,parameter = list(support = 0.1, confidence = 0.6,minlen=3),appearance = list(default="lhs",rhs="income=small"))
subset_rules(income_small_rules)
inspect(head(income_small_rules,10))


#plot rules
plot(rules, measure = c("support", "lift"), shading = "confidence")

#plot rules fo income income
plot(income_large_rules, measure = c("support", "lift"), shading = "confidence")

#plot rules for small income
plot(income_small_rules,measure=c("support","lift"),shading="confidence")
