# Specify the packages of interest
packages=c("maps","zipcode","mapproj","ggmap","ggplot2","readxl")

# Use this function to check if each package is on the local machine
# If a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Verify they are loaded
search()

library(readr)
library(kernlab)
library(e1071)
library(ggplot2)
library(gridExtra)
library(caret)
library(readxl)

#-------Read in the RAW data-------

library(readxl)

location <- "Airline_Satisfaction_Survey.xlsx"

SatSurveyRaw <- read_excel(location, na = "NA")


# Create data frame for clean up
# SatSurvey<-SatSurveyRaw

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

#-------Let's view the clean data file specs-------

#display the specs of the columns
str(df)

#------------Create new variables----------
#variable creation
length(df$No..of.other.Loyalty.Cards[df$No..of.other.Loyalty.Cards=='NA']) # check for NAs df$LoyaltyCardCat <- ifelse(df$No..of.other.Loyalty.Cards>0, "Member", "Non-Member") length(df$Shopping.Amount.at.Airport[df$Shopping.Amount.at.Airport=='NA']) # check for NAs df$ShoppingCat <- ifelse(df$Shopping.Amount.at.Airport>0, "Shopper", "Non-Shopper") length(df$Eating.and.Drinking.at.Airport[df$Eating.and.Drinking.at.Airport=='NA']) # check for NAs df$DinerCat <- ifelse(df$Eating.and.Drinking.at.Airport>0, "Diner", "Non-Diner")
#Create satisfaction variable coded (4-5, 3, 1-2)
df$Satisfaction.Coded <- ifelse(df$Satisfaction==5, "4-5", ifelse(df$Satisfaction==4, "4-5", ifelse(df$Satisfaction==3, "3", ifelse(df$Satisfaction==2, "1-2",ifelse(df$Satisfaction==1, "1-2", 'NA')))))

#-------Creating variables for analysis-------

library(ggplot2)
#No. of other Loyalty Cards:  add a column with (0=none) and (1=loyalty member)
length(df$No..of.other.Loyalty.Cards[df$No..of.other.Loyalty.Cards=='NA']) # check for NAs
df$LoyaltyCardCat <- ifelse(df$No..of.other.Loyalty.Cards>0, "Member", "Non-Member")
g <- ggplot(df, aes(x=LoyaltyCardCat))
g <- g + geom_bar(color='black', fill='gray')
g # visualize result

#Shopping Amount at Airport:  add a column with (0=non-shopper) and (1=shopper)
length(df$Shopping.Amount.at.Airport[df$Shopping.Amount.at.Airport=='NA']) # check for NAs
df$ShoppingCat <- ifelse(df$Shopping.Amount.at.Airport>0, "Shopper", "Non-Shopper")
g <- ggplot(df, aes(x=ShoppingCat))
g <- g + geom_bar(color='black', fill='gray')
g # visualize result

#Eating and Drinking at Airport:  add a column with (0=non-diner) and (1=diner)
length(df$Eating.and.Drinking.at.Airport[df$Eating.and.Drinking.at.Airport=='NA']) # check for NAs
df$DinerCat <- ifelse(df$Eating.and.Drinking.at.Airport>0, "Diner", "Non-Diner")
g <- ggplot(df, aes(x=DinerCat))
g <- g + geom_bar(color='black', fill='gray')
g # visualize result

table(df$DinerCat) # we see there aren't many non-diners for analysis
table(df$DinerCat) / length(df$DinerCat) # in fact, less than 5% are non-diners

#-------Additional Feature Profile------

# LOYALTY CARD
TempDf <- data.frame(table(df$LoyaltyCardCat) / length(df$LoyaltyCardCat))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Loyalty Membership")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

# SHOPPER
TempDf <- data.frame(table(df$ShoppingCat) / length(df$ShoppingCat))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Airport Shopping")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

# DINER
TempDf <- data.frame(table(df$DinerCat) / length(df$DinerCat))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Airport Dining")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

#-------Demographic Profile-------
#GENDER
TempDf <- data.frame(table(df$Gender) / length(df$Gender))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", color="white", width=0.35)
g <- g + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Gender")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.5, vjust=-1, color = "black", size=20),
                                 legend.text = element_text(size=15),
                                 legend.position = "bottom")
g

#AGE
g <- ggplot(df, aes(x=Age))
g <- g + geom_histogram(color="white", fill="dodgerblue4", binwidth=5)
g <- g + labs(title = "Age Distribution") + theme(plot.title = element_text(hjust=0.5, vjust=2, size=20))
g

#-------Usage Profile-------
#AIRLINE STATUS
TempDf <- data.frame(table(df$Airline.Status) / length(df$Airline.Status))
TempDf$Var1 <- factor(TempDf$Var1, levels=c("Blue", "Silver", "Gold", "Platinum"))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", color="white", width=0.35)
g <- g + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("blue4", "grey75","darkgoldenrod2", "gray35")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Airline Status")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.5, vjust=-1, color = "black", size=20),
                                 legend.text = element_text(size=15),
                                 legend.position = "right")
g

#NUMBER OF FLIGHTS
g <- ggplot(df, aes(x=No.of.Flights.p.a.))
g <- g + geom_histogram(color="white", fill="dodgerblue4", binwidth=5)
g <- g + labs(title = "Number of Flights") + theme(plot.title = element_text(hjust=0.5, vjust=2, size=20))
g

#TYPE OF TRAVELER
TempDf <- data.frame(table(df$Type.of.Travel) / length(df$Type.of.Travel))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("indianred3", "chartreuse3", "royalblue2")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Type of Travel")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

#CLASS
TempDf <- data.frame(table(df$Class) / length(df$Class))
TempDf$Var1 <- factor(TempDf$Var1, levels=c("Eco", "Eco Plus", "Business"))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", color="white", width=0.35)
g <- g + geom_text(aes(label=paste0(round(Freq*100), "%")), position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue1", "dodgerblue4","black")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Class")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.5, vjust=-1, color = "black", size=20),
                                 legend.text = element_text(size=15),
                                 legend.position = "left")
g


#Create satisfaction variable coded (4-5, 3, 1-2)
df$Satisfaction.Coded <- ifelse(df$Satisfaction==5, "4-5", ifelse(df$Satisfaction==4, "4-5", ifelse(df$Satisfaction==3, "3", ifelse(df$Satisfaction==2, "1-2",ifelse(df$Satisfaction==1, "1-2", 'NA'))))) 

#Satisfaction descriptives
summary(df$Satisfaction)

#Let's check sample sizes by airline
stack(tapply(df$Unique, df$Airline.Name, length))

#View mean satisfaction by airline

round.mean <- function(x)
{
  y <- round(mean(x),digits=2)
  return(y)
}

stack(tapply(df$Satisfaction, df$Airline.Name, round.mean))

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
TempDf <- data.frame(tapply(df$Satisfaction, df$Airline.Name, mean))
TempDf$Airline.Name <- row.names(TempDf)
names(TempDf)[1] <- "Value"
g <- ggplot(TempDf, aes(x=reorder(Airline.Name, -Value), y=Value))
g <- g + geom_bar(stat="identity", color="black",fill="dodgerblue4")
g <- g + theme(axis.text.x = element_text(angle = 90, size=12))
g <- g + ggtitle("Average Satisfaction by Airline Sorted") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g <- g + geom_text(aes(label=round(Value,2)), position=position_dodge(width=0.9), vjust=-0.25)
g

#-------Airline Average Arrival Delay Comparison-------
g <- ggplot(df, aes(x=Airline.Name, y=Arrival.Delay.in.Minutes))
g <- g + geom_bar(stat="summary", fun="mean", color="black", fill="dodgerblue4")
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + xlab("Airline Name") + ylab("Arrival Delay (minutes)")
g

#-------Satisfaction by Loyalty Membership (Split Business vs. Personal) with y=Average-------
g <- ggplot(df, aes(x=LoyaltyCardCat, y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + ggtitle("Average Satisfaction by Loyalty Membership & Travel Type") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Loyalty Membership") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g <- g + geom_point(stat="summary", fun="mean", size=3)
g

#-------Satisfaction by Age & Traveler Type-------
g <- ggplot(df, aes(x=Age, y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_point(stat="summary", fun="mean", size=1.75)
g <- g + ggtitle("Average Satisfaction by Age & Travel Type") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Age of Customer") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g

#-------Satisfaction by Airline Status & Traveler Type-------
g <- ggplot(df, aes(x=factor(Airline.Status, level = c("Blue", "Silver", "Gold", "Platinum")), y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + geom_point(stat="summary", fun="mean", size=3)
g <- g + ggtitle("Average Satisfaction by Airline Status & Travel Type") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Status") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g

#-------Satisfaction by Gender & Traveler Type-------
g <- ggplot(df, aes(x=Gender, y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + geom_point(stat="summary", fun="mean", size=3)
g <- g + ggtitle("Average Satisfaction by Gender & Travel Type") + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Gender") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g