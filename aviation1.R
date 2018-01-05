rm(list = ls())

#############################################################
##                Importing Data                          ##
#############################################################
install.packages("readr")
library(readr)
AviationDataEnd2016UP <- read_csv("C:/Users/hibab/Google Drive/4sigma/semestre 2/data mining/projet/AviationDataEnd2016UP.csv")
summary(AviationDataEnd2016UP)
View(AviationDataEnd2016UP)

#############################################################
##                 Preparing Data                          ##
#############################################################

##backup initial data
aviationInitial<-AviationDataEnd2016UP
AviationDataEnd2016UP<-aviationInitial
library(dplyr)
## colonne quali vs colonne quanti
Aviation.quant<-select_if(AviationDataEnd2016UP, is.numeric)
Aviation.quali<-select_if(AviationDataEnd2016UP, is.character)

##deleting rows with special character
AviationDataEnd2016UP <- AviationDataEnd2016UP[-grep("\\?",AviationDataEnd2016UP$Airport.Name),]
AviationDataEnd2016UP <- AviationDataEnd2016UP[-grep("\\?",AviationDataEnd2016UP$Location),]

##converting upperCase to lowerCase
AviationDataEnd2016UP$Investigation.Type<-as.factor(tolower(AviationDataEnd2016UP$Investigation.Type))
AviationDataEnd2016UP$Accident.Number<-as.factor(tolower(AviationDataEnd2016UP$Accident.Number))
AviationDataEnd2016UP$Location<-as.factor(tolower(AviationDataEnd2016UP$Location))
AviationDataEnd2016UP$Country<-as.factor(tolower(AviationDataEnd2016UP$Country))
AviationDataEnd2016UP$Airport.Name<-as.factor(tolower(AviationDataEnd2016UP$Airport.Name))
AviationDataEnd2016UP$Airport.Code<-as.factor(tolower(AviationDataEnd2016UP$Airport.Code))
AviationDataEnd2016UP$Injury.Severity<-as.factor(tolower(AviationDataEnd2016UP$Injury.Severity))
AviationDataEnd2016UP$Aircraft.Damage<-as.factor(tolower(AviationDataEnd2016UP$Aircraft.Damage))
AviationDataEnd2016UP$Aircraft.Category<-as.factor(tolower(AviationDataEnd2016UP$Aircraft.Category))
AviationDataEnd2016UP$Registration.Number<-as.factor(tolower(AviationDataEnd2016UP$Registration.Number))
AviationDataEnd2016UP$Make<-as.factor(tolower(AviationDataEnd2016UP$Make))
AviationDataEnd2016UP$Model<-as.factor(tolower(AviationDataEnd2016UP$Model))
AviationDataEnd2016UP$Amateur.Built<-as.factor(tolower(AviationDataEnd2016UP$Amateur.Built))
AviationDataEnd2016UP$FAR.Description<-as.factor(tolower(AviationDataEnd2016UP$FAR.Description))
AviationDataEnd2016UP$Engine.Type<-as.factor(tolower(AviationDataEnd2016UP$Engine.Type))
AviationDataEnd2016UP$Schedule<-as.factor(tolower(AviationDataEnd2016UP$Schedule))
AviationDataEnd2016UP$Purpose.of.Flight<-as.factor(tolower(AviationDataEnd2016UP$Purpose.of.Flight))
AviationDataEnd2016UP$Air.Carrier<-as.factor(tolower(AviationDataEnd2016UP$Air.Carrier))
AviationDataEnd2016UP$Weather.Condition<-as.factor(tolower(AviationDataEnd2016UP$Weather.Condition))
AviationDataEnd2016UP$Broad.Phase.of.Flight<-as.factor(tolower(AviationDataEnd2016UP$Broad.Phase.of.Flight))
AviationDataEnd2016UP$Report.Status<-as.factor(tolower(AviationDataEnd2016UP$Report.Status))

aviationInitial<-AviationDataEnd2016UP
View(AviationDataEnd2016UP)

View(aviationInitial)


##check if we have emty columns
sapply(AviationDataEnd2016UP, function(x)all(is.na(x)))
isTRUE(sapply(AviationDataEnd2016UP, function(x)all(is.na(x))))
####we have no empty column but there are many columns that are not needed in our study.


###columns that we dont need
AviationDataEnd2016UP$Accident.Number<-NULL
AviationDataEnd2016UP$Registration.Number<-NULL
AviationDataEnd2016UP$Event.Id<-NULL
AviationDataEnd2016UP$FAR.Description<-NULL
AviationDataEnd2016UP$Publication.Date<-NULL

unique (AviationDataEnd2016UP$Schedule)

###making all empty values as NA

##### non available data are presented as NA, N/N, UNk, unk
##### so we will make them unique as NA.


##to clean data error occured because of the event date.
##that's why we deleted the event date and cleaned dataset.
AviationDataEnd2016UP$Event.Date<-NULL
AviationDataEnd2016UP[AviationDataEnd2016UP=="N/A"] <- NA
AviationDataEnd2016UP[AviationDataEnd2016UP=="unk"] <- NA
AviationDataEnd2016UP[AviationDataEnd2016UP=="unknown"] <- NA


##then we bind it back, from the backup aviationInitial
AviationDataEnd2016UP<-cbind(AviationDataEnd2016UP, Event.Date= aviationInitial$Event.Date)


# ###change NA in dates by 1900-01-01
# AviationDataEnd2016UP$Event.Date[is.na(AviationDataEnd2016UP$Event.Date)]<-"1900-01-01"
# AviationDataEnd2016UP$Publication.Date[is.na(AviationDataEnd2016UP$Publication.Date)]<-"1900-01-01"



##############################################################################
## preparing the class to predict:  Fatality (Fatal, non fatal Or incident)###
##############################################################################

##here we want to know whether the accident is fatal or not without counting the deaths in the fatal case
AviationDataEnd2016UP$Injury.Severity<-gsub("fatal","fatalx",AviationDataEnd2016UP$Injury.Severity)
library(tidyr)
AviationDataEnd2016UP<-separate(AviationDataEnd2016UP,Injury.Severity,c("Injury.Severity","Fatal"),sep="x") ##here we used tidyr package
AviationDataEnd2016UP$Fatal<-gsub("\\(", "", AviationDataEnd2016UP$Fatal)
AviationDataEnd2016UP$Fatal<-gsub("\\)", "", AviationDataEnd2016UP$Fatal)
##what we've done there, is we put an x after every fatal, just to mark the end of the word, so we can move the number of deathsinto another column that we called fatal
##then we replaced the parentheses existing in the column fatal with empty character.
?gsub
AviationDataEnd2016UP$Fatal<-NULL
View(AviationDataEnd2016UP)
unique(AviationDataEnd2016UP$Injury.Severity)
###  "non-fatal"   "fatal"       "incident"    "unavailable"


##############################################################################
## preparing the class to predict:  Location        ##347 location         ###
##############################################################################

##here we want to know whether the accident is fatal or not without counting the deaths in the fatal case
AviationDataEnd2016UP<-separate(AviationDataEnd2016UP,Location,c("Location","State or cntry"),sep=",") ##here we used tidyr package

View(AviationDataEnd2016UP)
unique(AviationDataEnd2016UP$`State or cntry`) ##347 location





#####export data to backup
backupAviationPrepared <- AviationDataEnd2016UP
write.table(AviationDataEnd2016UP,file = "C:/Users/hibab/Google Drive/4sigma/semestre 2/data mining/projet/final/code R/AviationNew.txt")
write.csv(AviationDataEnd2016UP,file = "C:/Users/hibab/Google Drive/4sigma/semestre 2/data mining/projet/final/code R/AviationNew.csv")










###############################################################
##              some statistics
##############################################################

##only accidents
accident<-AviationDataEnd2016UP %>%  filter(AviationDataEnd2016UP$Investigation.Type== "accident" )%>% droplevels()
accident
nrow(accident)
##only incidents
incident<-  AviationDataEnd2016UP %>%  filter(AviationDataEnd2016UP$Investigation.Type== "incident" )%>% droplevels()
incident
##76,118  accidents (96%) vs  3,165 incidents (4%)
3165/79293
View(accident)
View(incident)


## number of events occured by country
eventByCountry<-aggregate(data.frame(count = AviationDataEnd2016UP$Country), list(value = AviationDataEnd2016UP$Country), length)
##order
x<-eventByCountry[with(eventByCountry,order(count, decreasing = TRUE)),]
x<-as.data.frame(eventByCountry)

####
#   United States 74734
#         Canada   256
#         Brazil   220
# United Kingdom   216
#         Mexico   210
#      Australia   196
74734/79293*100
##=94.25% from usa


###event by country rate
x$RateEventByCountry<-round(x$count*100/79293,digits=2)
x$RateEventByCountry <- paste(x$RateEventByCountry, "%", sep="")
View(x)

countryeventcount<-x[,1:2]
##another way to do the ordered country by event
orderedcountryeventcount <- countryeventcount$value [order (countryeventcount$count, decreasing = TRUE)]
orderedcountryeventcount
##top 5 countries by event ocuracy
Top5Countries=subset(countryeventcount,value %in% orderedcountryeventcount[1:5])
Top5Countries
hist(Top5Countries)
###ordered locations by event ocuracy
eventByLocation<-aggregate(data.frame(count = AviationDataEnd2016UP$`State or cntry`), list(value = AviationDataEnd2016UP$`State or cntry`), length)
y<-eventByLocation[with(eventByLocation,order(count, decreasing = TRUE)),]
View(y)
##top5 locations by even ocuracy
orderedLocationeventcount <- y$value [order (y$count, decreasing = TRUE)]

Top5Location=subset(y,value %in% orderedLocationeventcount[1:5])
Top5Location

#      value count
# 43     CA  8177
# 306    TX  5265
# 91     FL  5226
# 3      AK  5171
# 17     AZ  2554

############Visualizations

##to unify the accidents rate within coutries
##we opted to classify the accidents accuracy by countries like this:
##so the country with level 1 is the one having accidents between 0 and 10
x$AccidentsRateByCountry <- as.numeric(cut(x$count, c(0, 10, 100, 200,300,400, 10000,100000)))
##the cut function  divides the range of x into intervals and codes the values in x according to which interval they fall.
##the we called the package Rworldmap
install.packages("rworldmap")
library(rworldmap)

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map

spdfu <- joinCountryData2Map(x, joinCode="NAME", nameJoinColumn="value")

mapCountryData(spdfu, nameColumnToPlot="AccidentsRateByCountry", catMethod="fixedWidth",colourPalette =  c("#F1EEF6", "#D4B9DA",  "#DF65B0","#F60073", "#DD1C77","#592D67","#5B00F9"), addLegend = TRUE)

library(ggplot2)
library(gplots)
AV.theme<-theme(
  axis.text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size = 14),
  panel.grid.major = element_line(color = "black"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey"),
  legend.position = "right",
  legend.justification = "top", 
  legend.background = element_blank(),
  panel.border = element_rect(color = "royalblue4", fill = NA, size = 2))

##events by country
Plot.topcountry.bar<-ggplot(data = Top5Countries,
                           aes(x=value,
                               y=count))+
  geom_bar(width= 0.5,stat = "identity",fill="light blue")+
  
  
  ggtitle("events  per country")+
  labs(x="country",
       y="number of events ")+
  theme(axis.text.x=element_text(size= 8, angle=90,hjust = 0.5))
Plot.topcountry.bar

##events by location
Plot.topLocation.bar<-ggplot(data = Top5Location,
                            aes(x=value,
                                y=count))+
  geom_bar(width= 0.5,stat = "identity",fill="light blue")+
  
  
  ggtitle("events  per location")+
  labs(x="location",
       y="number of events ")+
  theme(axis.text.x=element_text(size= 8, angle=90,hjust = 0.5))
Plot.topLocation.bar
##############################################################################
##                              Time Series Study                          ###
##############################################################################


Aviation <- AviationDataEnd2016UP
# the use of colclasses helps importation faster and makes dates that are in the form %Y-%m-%d or Y/%m/%d will import correctly. 
View(Aviation)
#suppressMessages(attach(Aviation))


##now we will divide the event date into more columns, year, month..
Aviation$yearmonth<- format(as.Date(Aviation$Event.Date), "%Y-%m") #year month
Aviation$year<- format(as.Date(Aviation$Event.Date), "%Y") #year 
Aviation$monthletter<- format(as.Date(Aviation$Event.Date),  format = "%B") # month
Aviation$monthnbr<- format(as.Date(Aviation$Event.Date),  format = "%m") # month


####settings of the visualization
library(ggplot2)
library(gplots)
AV.theme<-theme(
  axis.text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size = 14),
  panel.grid.major = element_line(color = "black"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey"),
  legend.position = "right",
  legend.justification = "top", 
  legend.background = element_blank(),
  panel.border = element_rect(color = "royalblue4", fill = NA, size = 2))



##now we're going to analyse the fatal injuries by year
library(dplyr)
aviation.FatalInjuries<-Aviation[which(Aviation$Total.Fatal.Injuries>0),]
##aviation.FatalInjuries are events where there's minimum 1 fatal injury

# 
# #the 'magrittr' package which allows functional arguments to be passed to functions in a pipes style fashion (David Smith ).
# #This stylistic option has several advantages:
#   
# #1. Reduced requirements of nested parenthesizes
# #2. Order of functional operations now read from left to right
# #3. Organizational style of the code may be improved
# 
# #The library uses a new operator %>% which basically tells R to take the value of that which is to the left and pass it to the right as an argument.


fatalInjuriesbyyear <- summarise(group_by(aviation.FatalInjuries,year),Num.Acc.per.Year=n())%>% arrange(desc(Num.Acc.per.Year))
fatalInjuriesbyyear
##here we listed events with fatal injuries and groupby year, and we count number of accidents in each year
## and then we ordered them by number of accidents by year


#The Number Of Observations In The Current Group.


###some visualizations
plot(fatalInjuriesbyyear)### we see that the number of acc per year are decreasing by years



###points
Plot.Acc.Years.points<-ggplot(data = fatalInjuriesbyyear,
                              aes(x=as.numeric(year),
                                  y=Num.Acc.per.Year))+
  geom_point(stat = "identity",fill="light blue")+
  
  
  ggtitle("Fatalities per Year")+
  labs(x="Year",
       y="Fatalities ")+
  theme(axis.text.x=element_text(size= 8, angle=90,hjust = 0.5))


###bar plot

Plot.Acc.Years.bar<-ggplot(data = fatalInjuriesbyyear,
                           aes(x=as.numeric(year),
                               y=Num.Acc.per.Year))+
  geom_bar(width= 0.5,stat = "identity",fill="light blue")+
  
  
  ggtitle("Fatalities per Year")+
  labs(x="Year",
       y="Fatalities ")+
  theme(axis.text.x=element_text(size= 8, angle=90,hjust = 0.5))
Plot.Acc.Years.bar
###now by month
###bar plot
fatalInjuriesbymonths <- summarise(group_by(aviation.FatalInjuries,monthnbr),Num.Acc.per.month=n())%>% arrange(desc(Num.Acc.per.month))
plot(fatalInjuriesbymonths)
Plot.Acc.month<-ggplot(data = fatalInjuriesbymonths,
                       aes(x=monthnbr,
                           y=Num.Acc.per.month))+
  geom_bar(width= 0.5,stat = "identity",fill="red")+
  ggtitle("Fatalities per month")+
  labs(x="month",
       y="Fatalities ")+
  theme(axis.text.x=element_text(size= 8, angle=90,hjust = 0.5))

###points

Plot.Acc.month.point<-ggplot(data = fatalInjuriesbymonths,
                             aes(x=monthnbr,
                                 y=Num.Acc.per.month))+
  geom_point(stat = "identity",fill="red")+
  ggtitle("Fatalities per month")+
  labs(x="month",
       y="Fatalities ")+
  theme(axis.text.x=element_text(size= 8, angle=90,hjust = 0.5))
Plot.Acc.month.point
####fatality by month shows seasonality. fatality increases in summer and decreases in cold months. 


#####??? trying to visualize them all in 1 plot
source("http://peterhaschke.com/Code/multiplot.R")
multiplot(Plot.Acc.month, Plot.Acc.Years.bar, Plot.Acc.month.point, Plot.Acc.Years.points, cols=2)

 ################################################################################
################################################################################
#                                PREDICTION                                   #
################################################################################
 ################################################################################


str(AviationDataEnd2016UP)
AviationDataEnd2016UP$state<-as.factor(AviationDataEnd2016UP$`State or cntry`)

View(AviationDataEnd2016UP)
#Weather.Condition,Investigation.Type, Broad.Phase.of.Flight, Purpose.of.Flight, schedule, Engine.Type, Amateur.Built, Aircraft.Category, Aircraft.Damage

NewData<-data.frame(Investigation.Type =AviationDataEnd2016UP$Investigation.Type,Event.Date =AviationDataEnd2016UP$Event.Date,state= AviationDataEnd2016UP$state ,Country= AviationDataEnd2016UP$Country,Weather.Condition= AviationDataEnd2016UP$Weather.Condition,Broad.Phase.of.Flight= AviationDataEnd2016UP$Broad.Phase.of.Flight, Purpose.of.Flight=  AviationDataEnd2016UP$Purpose.of.Flight, Engine.Type= AviationDataEnd2016UP$Engine.Type, Amateur.Built= AviationDataEnd2016UP$Amateur.Built,Aircraft.Category = AviationDataEnd2016UP$Aircraft.Category, Aircraft.Damage= AviationDataEnd2016UP$Aircraft.Damage, Injury.Severity =AviationDataEnd2016UP$Injury.Severity)
View(NewData)
NewData1<-NewData
## dans apriori il n'a pas accepter le type date, dnc g converti as factor, puis je l'ai ajouté à newdata.
NewData1$Event.Date<-as.factor(NewData1$Event.Date)
View(NewData1)
NewData$Event.Date<-NULL
NewData<-data.frame(NewData, Event.Date = NewData1$Event.Date)
NewData <- subset(x= NewData, subset = (!is.na(NewData$Investigation.Type)&!is.na(NewData$Event.Date)&!is.na(NewData$Country)&!is.na(NewData$state)& !is.na(NewData$Weather.Condition)& !is.na(NewData$Broad.Phase.of.Flight)&!is.na(NewData$Purpose.of.Flight)&!is.na(NewData$Engine.Type)&!is.na(NewData$Amateur.Built)&!is.na(NewData$Aircraft.Category)&!is.na(NewData$Aircraft.Damage)&!is.na(NewData$Injury.Severity)))
View(NewData)







#####export NewData to backup
backupAviationPrepared2 <- AviationDataEnd2016UP
write.table(NewData,file = "C:/Users/hibab/Google Drive/4sigma/semestre 2/data mining/projet/final/code R/NewData.txt")
write.csv(NewData,file = "C:/Users/hibab/Google Drive/4sigma/semestre 2/data mining/projet/final/code R/NewData.csv")



library(readr)
NewData <- read_csv("C:/Users/hibab/Google Drive/4sigma/semestre 2/data mining/projet/final/code R/NewData.csv")

## sample of 75% of data
smp_size <- floor(0.75 * nrow(NewData))
##set seed to have always the same sample
set.seed(123) 
train_ind <- sample(seq_len(nrow(NewData)), size = smp_size)
#training data
train <- NewData[train_ind, ]
#test data
test <- NewData[-train_ind, ]



View(AviationDataEnd2016UP)
################# SVM
library(kernlab)


model.svm=ksvm(Injury.Severity~. , data = train)
predict.svm=predict(model.svm, test)
xtab<-table(predict.svm,test$Injury.Severity)

# predict.svm   fatal incident non-fatal unavailable
# fatal         257        0       110           1
# incident        0       43         0           0
# non-fatal     304        0      3217           1
# unavailable     0        0         0           0

(257+43+3217)/(257+110+1+43+304+3217+1)
##0.89%
install.packages("caret")
library(caret) 
confusionMatrix(xtab)



#############KNN


library(kknn)
model.kknn <- kknn(formula=Injury.Severity~., train= train, test=test, distance = 1, kernel = "triangular", na.action = na.fail())


predict.knn<-predict(model.kknn,train, type="raw" )
predict.knn
table.knn<-table(predict.knn,test$Injury.Severity)
confusionMatrix(table.knn)
# Accuracy : 0.8818  

##############arbre
library(rpart)

arbre = rpart(Injury.Severity~.,data=train,minsplit=50,minbucket=15,cp=0)
arbre
plot(arbre)
text(arbre)
prediction = predict(arbre,test,type = "class")
evaluate = table(prediction,test$Injury.Severity)
evaluate
confusionMatrix(evaluate)




#########Regle d'association 
library(arules)
library(arulesViz)

##conversion as transactions.
train1<-as(train, "transactions")
test1<-as(test,"transactions")


#méthode des relges d'association => apropori
#intput : les données + supp min et conf min 
#Resultat: les regles les plus fréqentes
#sur grocerie, on applique  en fixant supportMin 0.007 et confience Min a 0.25 et minlength = cardinalité = 2 produits
#


##confiance 0.8 , minlen=10 toutes les colonnes 
rules<-apriori(train1, parameter = list(supp = 0.002, conf = 0.8,minlen=10,target ="rules"))

#Afficher les régles=> on appiquant les m&thode des regles d'association , on a 363 régles
rules

Fatalrules = subset(rules, rhs %in% "Injury.Severity=fatal")
inspect(sort(Fatalrules, by = "lift"))
Fatalrules

#rules ayant 2 cardinalités on a 127 rélges ou il y a 2 items
#137 rélges avec 2 itéms et 214 régles avec 3 items

summary(rules)
#Affichage des rélges + support + confience + lift
inspect(rules)
inspect(Fatalrules)
#Affichage odrer by lift : les 4 1ere regles ayant le plus grand lift
inspect(head(sort(rules, decreasing=TRUE,by="lift"),10))
#inspect(sort(rules,by="lift"),10)


""

#visualisation DES Régels sous forme graphique

#Les points = les regles par rapport support et confience
#lift :plus la régle est foncé plus la régle est + interessante

plot(Fatalrules)

#présentre les regles sous forme de cerle, taille du cerle => support et la couleur c le lift
#Le plus important dans une relges c le lift

plot(Fatalrules, method="matrix", measure = "lift")
plot(Fatalrules, method="paracoord", control=list(reorder=TRUE)) 
plot (rules,method="graph",shading="confidence")
