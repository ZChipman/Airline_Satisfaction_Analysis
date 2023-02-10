#specify the packages of interest
packages=c("maps","zipcode","mapproj","ggmap","ggplot2","readxl")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

library(readr)
readCSV<-function(location) #read file
{
  # read in data, replace blanks with NAs, trim white space
  x <- read_csv(location, na = "NA", trim_ws = TRUE)
  return(x)
}


library(kernlab)
library(e1071)
library(ggplot2)
library(gridExtra)
library(caret)

#Create Raw Dataframe
SatSurveyRaw <- readCSV("Desktop/IST687/Code/Satisfaction Survey(2).csv")


#Create data frame for clean up
SatSurvey<-SatSurveyRaw


# Remove cases with misrepresented satisfaction ratings


#write a function for: 0 entry is maintained, <1 is set to 1 to categorized to yes/no ie 1/0

#No. of other Loyalty Cards:  add a column with (0=none) and (1=loyalty member)

SatSurvey$LoyaltyBin[SatSurvey$"No. of other Loyalty Cards" == 0]=0
SatSurvey$LoyaltyBin[SatSurvey$"No. of other Loyalty Cards" > 0]=1
#Shopping Amount at Airport:  add a column with (0=non-shopper) and (1=shopper)
SatSurvey$ShopperBin[SatSurvey$"Shopping Amount at Airport" == 0]=0
SatSurvey$ShopperBin[SatSurvey$"Shopping Amount at Airport" > 0]=1

#Eating and Drinking at Airport:  add a column with (0=non-diner) and (1=diner)
SatSurvey$DinerBin[SatSurvey$"Eating and Drinking at Airport" == 0]=0
SatSurvey$DinerBin[SatSurvey$"Eating and Drinking at Airport" > 0]=1
# Imputes values over 100  
veclength <- length(SatSurvey$`% of Flight with other Airlines`)
for (i in 1:veclength){
  if (SatSurvey$`% of Flight with other Airlines`[i] > 100){
    SatSurvey$`% of Flight with other Airlines`[i] <- 100
  }
  
}

#-------Clean the data within a function-------

CleanData <- function(inputDF) # function to clean satisfaction survey data set
{
  d <- inputDF
  
  # transform column names to new names without spaces
  names(d)<-make.names(names(d),unique = TRUE)
  names(d)[8]<-paste("Percent.of.Flight.with.Other.Airlines")
  
  # Remove cases with misrepresented satisfaction ratings (i.e., NOT 1:5 integers)
  d <- d[!is.na(d$Satisfaction), ]
  d <- d[d$Satisfaction==1 | d$Satisfaction==2 | d$Satisfaction==3 | d$Satisfaction==4 | d$Satisfaction==5, ]
  
  #Code down flight with other airlines values over 100%, down to 100%
  d$Percent.of.Flight.with.Other.Airlines[d$Percent.of.Flight.with.Other.Airlines>100] <- 100
  
  #remove records with no flight time AND does not say their flight was canceled
  #we feel these records aren't intuitive and therefore not useful, perhaps captured incorrectly 
  d <- d[!(is.na(d$Arrival.Delay.in.Minutes) & is.na(d$Flight.time.in.minutes) & d$Flight.cancelled=="No"),]
  
  #add a unique identifier
  d$Unique <- c(1:129543)
  
  return(d)
}

df <- CleanData(SatSurvey)

#-------Let's view the clean data file specs-------

#display the specs of the columns
str(df)

#-------Creating variables for analysis-------

library(ggplot2)
#No. of other Loyalty Cards:  add a column with (0=none) and (1=loyalty member)
length(df$No..of.other.Loyalty.Cards[df$No..of.other.Loyalty.Cards=='NA']) # check for NAs
df$LoyaltyCardCat <- ifelse(df$No..of.other.Loyalty.Cards>0, "Member", "Non-Member")
g <- ggplot(df, aes(x=LoyaltyCardCat))
g <- g + geom_bar(color='black', fill='gray')
g

#Shopping Amount at Airport:  add a column with (0=non-shopper) and (1=shopper)
length(df$Shopping.Amount.at.Airport[df$Shopping.Amount.at.Airport=='NA']) # check for NAs
df$ShoppingCat <- ifelse(df$Shopping.Amount.at.Airport>0, "Shopper", "Non-Shopper")
g <- ggplot(df, aes(x=ShoppingCat))
g <- g + geom_bar(color='black', fill='gray')
g

#Eating and Drinking at Airport:  add a column with (0=non-diner) and (1=diner)
length(df$Eating.and.Drinking.at.Airport[df$Eating.and.Drinking.at.Airport=='NA']) # check for NAs
df$DinerCat <- ifelse(df$Eating.and.Drinking.at.Airport>0, "Diner", "Non-diner")
g <- ggplot(df, aes(x=DinerCat))
g <- g + geom_bar(color='black', fill='gray')
g # we see there isn't many non-diners for analysis

#Create satisfaction variable coded (4-5, 3, 1-2)
df$Satisfaction.Coded <- ifelse(df$Satisfaction==5, "4-5", ifelse(df$Satisfaction==4, "4-5", ifelse(df$Satisfaction==3, "3", ifelse(df$Satisfaction==2, "1-2",ifelse(df$Satisfaction==1, "1-2", 'NA'))))) 

#-------Satisfaction by Airline Visualized as Counts-------
g <- ggplot(df, aes(x=Airline.Name))
g <- g + geom_histogram(stat="count", color="black", aes(fill=Satisfaction.Coded))
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Satisfaction by Airline") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("Count of Records") + labs(fill="Satisfaction Grouped")
g <- g + scale_fill_manual(values = c("darkred", "gray", "dodgerblue4"))
g

#-------Satisfaction by Airline Visualized as Percentages-------
g <- ggplot(df, aes(fill=factor(Satisfaction), y=Unique, x=Airline.Name))
g <- g + geom_bar(position="fill", stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Satisfaction by Airline") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("") + labs(fill="Satisfaction")
g <- g + scale_fill_manual(values = c("darkred", "indianred2", "gray", "dodgerblue1", "dodgerblue4"))
g

#-------Satisfaction by Airline Visualized as Y=Average-------
g <- ggplot(df, aes(x=Airline.Name, y=Satisfaction))
g <- g + geom_bar(stat="summary", fun="mean", color="black",fill="dodgerblue4")
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Average Satisfaction by Airline") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
# g <- g + geom_text(aes(label=Satisfaction)) -> this doesn't work??
g

#-------Satisfaction by Loyalty Membership (Split Business vs. Personal) with y=Average-------
g <- ggplot(df, aes(x=LoyaltyCardCat, y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + ggtitle("Average Satisfaction by Loyalty Membership & Travel Type") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Loyalty Membership") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
# [MGS] Not sure how to add data labels
g

#display the specs of the columns
spec(SatSurvey)

#Display in table
View(SatSurvey)

# Chart with satisfaction by airline
#-------Satisfaction by Airline Visualized as Counts-------
g <- ggplot(df, aes(x=Airline.Name))
g <- g + geom_histogram(stat="count", color="black", aes(fill=Satisfaction.Coded))
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Satisfaction by Airline") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("Count of Records") + labs(fill="Satisfaction Grouped")
g <- g + scale_fill_manual(values = c("darkred", "gray", "dodgerblue4"))
g

#-------Satisfaction by Departure Delay (Split Business vs. Personal) with y=Average-------
length(df$Departure.Delay.in.Minutes[df$Departure.Delay.in.Minutes=='NA'])

df$Departure.Binned <- ifelse(df$Departure.Delay.in.Minutes >= 60, "60+", 
                                ifelse(df$Departure.Delay.in.Minutes < 60 & df$Departure.Delay.in.Minutes >= 10, "10-59", 
                                       ifelse(df$Departure.Delay.in.Minutes < 10 & df$Departure.Delay.in.Minutes >= 1, "1-9",
                                              "0")))

df$Departure.Binned[is.na(df$Departure.Binned)] <- "Flight Cancelled"                                         

g <- ggplot(df, aes(x=Departure.Binned, y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + ggtitle("Average Satisfaction by Departure Delay in Minutes") + theme(plot.title=element_text(hjust=0.25))
g <- g + xlab("Approximate Departure Delay (Minutes)") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g


#-------Satisfaction by Arrival Delay (Split Business vs. Personal) with y=Average-------
length(df$Arrival.Delay.in.Minutes[df$Arrival.Delay.in.Minutes=='NA'])
df$Arrival.Binned <- ifelse(df$Arrival.Delay.in.Minutes >= 60, "60+", 
                              ifelse(df$Arrival.Delay.in.Minutes < 60 & df$Arrival.Delay.in.Minutes >= 10, "10-59", 
                                     ifelse(df$Arrival.Delay.in.Minutes < 10 & df$Arrival.Delay.in.Minutes >= 1, "1-9", 
                                            "0")))

df$Arrival.Binned[is.na(df$Arrival.Binned)] <- "Flight Cancelled"   

g <- ggplot(df, aes(x=Arrival.Binned, y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + ggtitle("Average Satisfaction by Arrival Delay in Minutes") + theme(plot.title=element_text(hjust=0.25))
g <- g + xlab("Approximate Arrival Delay (Minutes)") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g



# Show the U.S. map, 
# Thus code was modified from the Sample Project Report
OriginState <- state.name 
area <- state.area
center <- state.center
state_1 <- data.frame(OriginState, area, center)


UniqueOS <- sort(table(df$Origin.State)) 
UniqueOS <- data.frame(UniqueOS) 
colnames(UniqueOS) <- c("OriginState","count") 

Origin_State <- merge(UniqueOS, state_1, by = "OriginState") 
Origin_State$OriginState <- tolower(Origin_State$OriginState) 

library(ggplot2) 
library(ggmap)
us <- map_data("state")

Origin_State_Area <- ggplot(Origin_State, aes(map_id = OriginState)) 
Origin_State_Area <- Origin_State_Area + 
  geom_map(map = us, aes(fill = Origin_State$area))
Origin_State_Area <- Origin_State_Area + 
  expand_limits(x = Origin_State$x, y = Origin_State$y)
Origin_State_Area <- Origin_State_Area + coord_map() + ggtitle("Area") 

OS <- ggplot(Origin_State, aes(map_id = OriginState))
OS <- OS + geom_map(map = us, aes(fill = Origin_State$count)) 
OS <- OS + expand_limits(x = Origin_State$x, y = Origin_State$y) 
OS <- OS + coord_map() + ggtitle("Origin State")
OS

DestinationState <- state.name 
area <- state.area
center <- state.center
state_2 <- data.frame(DestinationState, area, center)

UniqueDS <- sort(table(df$Destination.State)) 
UniqueDS <- data.frame(UniqueDS)
colnames(UniqueDS) <- c("DestinationState","count")


Destination_State <- merge(UniqueDS, state_2, by = "DestinationState") 
Destination_State$DestinationState <- 
  tolower(Destination_State$DestinationState) 

Destination_State_Area <- ggplot(Destination_State, aes(map_id = DestinationState)) 
Destination_State_Area <- Destination_State_Area + 
  geom_map(map = us, aes(fill = Destination_State$area))
Destination_State_Area <- Destination_State_Area + 
  expand_limits(x = Destination_State$x, y = Destination_State$y)
Destination_State_Area <- Destination_State_Area + coord_map() + ggtitle("Area")

DS <- ggplot(Destination_State, aes(map_id = DestinationState))
DS <- DS + geom_map(map = us, aes(fill = Destination_State$count))
DS <- DS + expand_limits(x = Destination_State$x, y = Destination_State$y) 
DS <- DS + coord_map() + ggtitle("Destination State")
DS

Uniquedf <- df[df$Origin.State!="Alaska" & df$Origin.State!="Hawaii",]
Uniquedf <- Uniquedf[Uniquedf$Orgin.City != "Guam, TT",]
UniqueOC <- sort(table(Uniquedf$Orgin.City))
UniqueOC <- data.frame(UniqueOC) 
colnames(UniqueOC) <- c("OriginCity","count") 
View(UniqueOC)

UniqueDC <- sort(table(df$Destination.City)) 
UniqueDC <- data.frame(UniqueDC) 
colnames(UniqueDC) <- c("DestinationCity","count") 
View(UniqueDC)





#-------Clean the data within a function-------



CleanData <- function() # function to clean satisfaction survey data set
  
{
  
  d <- SatSurveyRaw
  
  
  
  # transform column names to new names without spaces
  
  names(d)<-make.names(names(d),unique = TRUE)
  
  names(d)[8]<-paste("Percent.of.Flight.with.Other.Airlines")
  
  
  
  # Remove cases with misrepresented satisfaction ratings (i.e., NOT 1:5 integers)
  
  d <- d[!is.na(d$Satisfaction), ]
  
  d <- d[d$Satisfaction==1 | d$Satisfaction==2 | d$Satisfaction==3 | d$Satisfaction==4 | d$Satisfaction==5, ]
  
  
  
  #Code down flight with other airlines values over 100%, down to 100%
  
  d$Percent.of.Flight.with.Other.Airlines[d$Percent.of.Flight.with.Other.Airlines>100] <- 100
  
  
  
  #remove records with no flight time AND does not say their flight was canceled
  
  #we feel these records aren't intuitive and therefore not useful, perhaps captured incorrectly
  
  d <- d[!(is.na(d$Arrival.Delay.in.Minutes) & is.na(d$Flight.time.in.minutes) & d$Flight.cancelled=="No"),]
  
  
  
  #add a unique identifier
  
  d$Unique <- c(1:129543)
  
  
  
  #convert flight.date to date format
  
  d$Flight.date <- as.Date(d$Flight.date , format = "%m/%d/%Y")
  
  
  
  return(d)
  
}



df <- CleanData()

####------------Create new variables----------

#variable creation

length(df$No..of.other.Loyalty.Cards[df$No..of.other.Loyalty.Cards=='NA']) # check for NAs

df$LoyaltyCardCat <- ifelse(df$No..of.other.Loyalty.Cards>0, "Member", "Non-Member")

length(df$Shopping.Amount.at.Airport[df$Shopping.Amount.at.Airport=='NA']) # check for NAs

df$ShoppingCat <- ifelse(df$Shopping.Amount.at.Airport>0, "Shopper", "Non-Shopper")

length(df$Eating.and.Drinking.at.Airport[df$Eating.and.Drinking.at.Airport=='NA']) # check for NAs

df$DinerCat <- ifelse(df$Eating.and.Drinking.at.Airport>0, "Diner", "Non-Diner")

#Create satisfaction variable coded (4-5, 3, 1-2)

df$Satisfaction.Coded <- ifelse(df$Satisfaction==5, "4-5", ifelse(df$Satisfaction==4, "4-5", ifelse(df$Satisfaction==3, "3", ifelse(df$Satisfaction==2, "1-2",ifelse(df$Satisfaction==1, "1-2", 'NA')))))





#-------convert to binary----------



#No. of other Loyalty Cards:  add a column with (0=none) and (1=loyalty member)



df$LoyaltyBin<- ifelse(df$No..of.other.Loyalty.Cards>0, 1, 0)

#df$LoyaltyBin<-as.factor(df$LoyaltyBin)

#Shopping Amount at Airport:  add a column with (0=non-shopper) and (1=shopper)

df$ShopperBin<- ifelse(df$Shopping.Amount.at.Airport>0, 1, 0)

#df$ShopperBin<-as.factor(df$ShopperBin)

#Eating and Drinking at Airport:  add a column with (0=non-diner) and (1=diner)

df$DinerBin<- ifelse(df$Eating.and.Drinking.at.Airport>0, 1, 0)

#df$DinerBin<-as.factor(df$DinerBin)

#gender bin male=1

df$GenderBin<- ifelse(df$Gender=="Male", 1, 0)

#df$GenderBin<-as.factor(df$GenderBin)

#canceled bin

df$Flight.canceledBin<- ifelse(df$Flight.cancelled=="yes", 1, 0)

#df$Flight.canceledBin<-as.factor(df$Flight.canceledBin)

#convert Travel type

df$Type.of.Travel.Business<- ifelse(df$Type.of.Travel=="Business travel", 1, 0)

#df$Type.of.Travel.Business<-as.factor(df$Type.of.Travel.Business)

df$Type.of.Travel.Personal<- ifelse(df$Type.of.Travel=="Personal Travel", 1, 0)

#df$Type.of.Travel.Personal<-as.factor(df$Type.of.Travel.Personal)

#Convert Status

df$Airline.Status.Blue<- ifelse(df$Airline.Status=="Blue", 1, 0)

#df$Airline.Status.Blue<-as.factor(df$Airline.Status.Blue)

df$Airline.Status.Silver<- ifelse(df$Airline.Status=="Silver", 1, 0)

#df$Airline.Status.Silver<-as.factor(df$Airline.Status.Silver)

df$Airline.Status.Gold<- ifelse(df$Airline.Status=="Gold", 1, 0)

#df$Airline.Status.Gold<-as.factor(df$Airline.Status.Gold)



#Convert Class

df$Class.Eco<- ifelse(df$Class=="Eco", 1, 0)

#df$Class.Eco<-as.factor(df$Class.Eco)

df$Class.EcoPlus<- ifelse(df$Class=="Eco Plus", 1, 0)

#df$Class.EcoPlus<-as.factor(df$Class.EcoPlus)



#------------Create Linear Model Dataframe-------------

dfLMModel<-data.frame(df$Satisfaction,df$Age,df$Price.Sensitivity,df$No.of.Flights.p.a.,
                      
                      df$No..of.other.Loyalty.Cards,df$Shopping.Amount.at.Airport,df$Eating.and.Drinking.at.Airport,
                      
                      df$Departure.Delay.in.Minutes,df$Arrival.Delay.in.Minutes,df$Flight.canceledBin,
                      
                      df$Flight.time.in.minutes,df$Flight.Distance,df$Airline.Status.Blue,df$Airline.Status.Gold,
                      
                      df$Airline.Status.Silver,df$Type.of.Travel.Business,df$Type.of.Travel.Personal,
                      
                      df$Class.Eco,df$Class.EcoPlus,df$GenderBin)

str(dfLMModel)

dfLMModel <- na.omit(dfLMModel) #Remove NAs from linear model df, we want data across these variables



linearModelAll <- lm(formula=df.Satisfaction~., data=dfLMModel)

summary(linearModelAll)

m1=lm(formula = df.Satisfaction~.,data=dfLMModel)
step(m1, data=dfLMModel, direction="backward")

m2=lm(formula = df.Satisfaction ~ df.Age + df.Price.Sensitivity + df.No.of.Flights.p.a. + df.Shopping.Amount.at.Airport + df.Departure.Delay.in.Minutes + 
        df.Arrival.Delay.in.Minutes + df.Flight.time.in.minutes + 
        df.Flight.Distance + df.Airline.Status.Blue + df.Airline.Status.Gold + 
        df.Airline.Status.Silver + df.Type.of.Travel.Business + df.Type.of.Travel.Personal + 
        df.Class.Eco + df.Class.EcoPlus + df.GenderBin, data=dfLMModel)
summary(m2)

m3 <- lm(formula = df.Satisfaction ~ df.Age + df.Price.Sensitivity + df.No.of.Flights.p.a. + 
           df.Shopping.Amount.at.Airport + df.Eating.and.Drinking.at.Airport + 
           df.Departure.Delay.in.Minutes + df.Arrival.Delay.in.Minutes + 
           df.Flight.time.in.minutes + df.Flight.Distance + df.Airline.Status.Blue + 
           df.Airline.Status.Gold + df.Airline.Status.Silver + df.Type.of.Travel.Business + 
           df.Type.of.Travel.Personal + df.Class.Eco + df.Class.EcoPlus + 
           df.GenderBin, data = dfLMModel)
summary(m3)

m4 <- lm(formula = df.Satisfaction ~ df.Age + df.Price.Sensitivity + 
           df.No.of.Flights.p.a. + df.Shopping.Amount.at.Airport + df.Eating.and.Drinking.at.Airport + 
           df.Departure.Delay.in.Minutes + df.Arrival.Delay.in.Minutes + 
           df.Flight.time.in.minutes + df.Flight.Distance + df.Airline.Status.Blue + 
           df.Airline.Status.Gold + df.Airline.Status.Silver + df.Type.of.Travel.Business + 
           df.Type.of.Travel.Personal + df.Class.Eco + df.Class.EcoPlus + 
           df.GenderBin, data = dfLMModel)
summary(m4)

#create train/test dfs

nrow.df<-nrow(dfLMModel) #total observations

cutPoint<-floor(nrow.df/3*2)#2/3 of the count

rand<-sample(1:nrow.df)#randomize rows

df.train<-dfLMModel[rand[1:cutPoint],]#create train dataset

dim(df.train)

df.test<-dfLMModel[rand[(cutPoint+1):nrow.df],]#create test dataset

dim(df.test)

####root mean squared error

# ksvm practice
#ksvmOutput <- ksvm(df.Satisfaction~., data=df.train, kernel="rbfdot", kpar="automatic",
                   #C=10, cross=10, probmodel=TRUE)
#head(ksvmOutput)

#ksvmPred <- predict(ksvmOutput, testData, type="votes")
#kcompTable <- data.frame(testData[,1], ksvmPred[,1])
#colnames(kcompTable) <- c("test", "Pred")
#sqrt(mean((kcompTable$test-kcompTable$Pred)^2))

rmse <- function(error)
  
{
  
  sqrt(mean(error^2))
  
}



lm.model<-lm(df.Satisfaction~.,data=df.train)

pred.lm<-predict(lm.model,df.test)

df.test$error1 <- df.test$df.Satisfaction - pred.lm

head(df.test)


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)


