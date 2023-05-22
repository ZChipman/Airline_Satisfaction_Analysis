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
  
  # Remove cases with misrepresented satisfaction ratings 
  # (i.e., NOT 1:5 integers)
  d <- d[!is.na(d$Satisfaction), ]
  d <- d[d$Satisfaction==1 | d$Satisfaction==2 | d$Satisfaction==3 | 
           d$Satisfaction==4 | d$Satisfaction==5, ]
  
  #Code down flight with other airlines values over 100%, down to 100%
  d$Percent.of.Flight.with.Other.Airlines[d$Percent.of.Flight.with.Other.Airlines>100] <- 100
  
  # Remove records with no flight time AND does not say their flight was 
  # canceled, we feel these records aren't intuitive and therefore not useful, 
  # perhaps captured incorrectly 
  d <- d[!(is.na(d$Arrival.Delay.in.Minutes) & is.na(d$Flight.time.in.minutes) 
           & d$Flight.cancelled=="No"),]
  
  # Add a unique identifier
  d$Unique <- c(1:129543)
  
  # Convert flight.date to date format
  d$Flight.date <- as.Date(d$Flight.date , format = "%m/%d/%Y")
  
  return(d)
}

df <- CleanData()

#-------Let's view the clean data file specs-------

# Display the specs of the columns
str(df)

#------------Create new variables----------
# Variable creation
# Check for NAs 
length(df$No..of.other.Loyalty.Cards[df$No..of.other.Loyalty.Cards=='NA'])  
df$LoyaltyCardCat <- ifelse(df$No..of.other.Loyalty.Cards>0, "Member", 
                            "Non-Member") 
length(df$Shopping.Amount.at.Airport[df$Shopping.Amount.at.Airport=='NA']) 
df$ShoppingCat <- ifelse(df$Shopping.Amount.at.Airport>0, "Shopper", 
                         "Non-Shopper") 
length(df$Eating.and.Drinking.at.Airport[df$Eating.and.Drinking.at.Airport=='NA'])  
df$DinerCat <- ifelse(df$Eating.and.Drinking.at.Airport>0, "Diner", "Non-Diner")
# Create satisfaction variable coded (4-5, 3, 1-2)
df$Satisfaction.Coded <- ifelse(df$Satisfaction==5, "4-5", 
                                ifelse(df$Satisfaction==4, "4-5", 
                                       ifelse(df$Satisfaction==3, "3", 
                                              ifelse(df$Satisfaction==2, "1-2", 
                                                     ifelse(df$Satisfaction==1, 
                                                            "1-2", 'NA')))))

#-------Convert to Binary----------

# No. of other Loyalty Cards: add a column with (0=none) and (1=loyalty member)
df$LoyaltyBin <- ifelse(df$No..of.other.Loyalty.Cards>0, 1, 0) 
df$LoyaltyBin <- as.factor(df$LoyaltyBin)
# Shopping Amount at Airport: add a column with (0=non-shopper) and (1=shopper) 
df$ShopperBin <- ifelse(df$Shopping.Amount.at.Airport>0, 1, 0) 
df$ShopperBin <- as.factor(df$ShopperBin)
# Eating and Drinking at Airport: add a column with (0=non-diner) and (1=diner) 
df$DinerBin <- ifelse(df$Eating.and.Drinking.at.Airport>0, 1, 0) 
df$DinerBin <- as.factor(df$DinerBin)
# Gender bin male=1
df$GenderBin <- ifelse(df$Gender=="Male", 1, 0) 
df$GenderBin <- as.factor(df$GenderBin)
# Canceled bin
df$Flight.canceledBin <- ifelse(df$Flight.cancelled=="yes", 1, 0) 
df$Flight.canceledBin <- as.factor(df$Flight.canceledBin)

# Convert Travel Type
df$Type.of.Travel.Business <- ifelse(df$Type.of.Travel=="Business travel", 1, 0)
df$Type.of.Travel.Business <- as.factor(df$Type.of.Travel.Business)
df$Type.of.Travel.Personal <- ifelse(df$Type.of.Travel=="Personal Travel", 1, 0)
df$Type.of.Travel.Personal <- as.factor(df$Type.of.Travel.Personal)

# Convert Status
df$Airline.Status.Blue<- ifelse(df$Airline.Status=="Blue", 1, 0)
df$Airline.Status.Blue<-as.factor(df$Airline.Status.Blue)
df$Airline.Status.Silver<- ifelse(df$Airline.Status=="Silver", 1, 0)
df$Airline.Status.Silver<-as.factor(df$Airline.Status.Silver)
df$Airline.Status.Gold<- ifelse(df$Airline.Status=="Gold", 1, 0)
df$Airline.Status.Gold<-as.factor(df$Airline.Status.Gold)

# Convert Class
df$Class.Eco<- ifelse(df$Class=="Eco", 1, 0)
df$Class.Eco<-as.factor(df$Class.Eco)
df$Class.EcoPlus<- ifelse(df$Class=="Eco Plus", 1, 0)
df$Class.EcoPlus<-as.factor(df$Class.EcoPlus)


class(df$Flight.canceledBin)
#---------------------------------------------------- 
# Modeling 
#----------------------------------------------------


#------------Create Linear Model Dataframe-------------

dfLMModel <- data.frame(df$Satisfaction,df$Age,df$Price.Sensitivity,
                      df$No.of.Flights.p.a.,df$No..of.other.Loyalty.Cards,
                      df$Shopping.Amount.at.Airport,
                      df$Eating.and.Drinking.at.Airport,
                      df$Departure.Delay.in.Minutes,df$Arrival.Delay.in.Minutes,
                      df$Flight.time.in.minutes,df$Flight.Distance,
                      df$Airline.Status.Blue,df$Airline.Status.Gold,
                      df$Airline.Status.Silver,df$Type.of.Travel.Business,
                      df$Type.of.Travel.Personal,
                      df$Class.Eco,df$Class.EcoPlus,df$GenderBin)

str(dfLMModel)

model.all<-lm(df.Satisfaction~.,data=dfLMModel)
summary(model.all)

# Note: change variable names

# First model iteration
# Remove loyalty cards (not significant)
iter.1 <- dfLMModel[,-5]
str(iter.1)
model.iter1 <- lm(df.Satisfaction~., data=iter.1)
summary(model.iter1)

# Second model iteration
# Remove Eating and Drinking (only significant at 90%)
iter.2 <- iter.1[,-6]
str(iter.2)
model.iter2 <- lm(df.Satisfaction~., data=iter.2)
summary(model.iter2)

# Iteration 3
# Remove departure delay due to colinearity with arrival delay
iter.3 <- iter.2[,-6]
str(iter.3)
model.iter3 <- lm(df.Satisfaction~., data=iter.3)
summary(model.iter3)

# Export to file for easier copy/paste
# sink("iter3.txt")
# print(summary(model.iter3))
# sink()

iter.3_coef <- summary(model.iter3)$coefficients
iter.3_coef

# Sets random seed (not in original project)
set.seed(14)

# Create Train/Test df for Iteration 3
nrow.df <- nrow(iter.3) # Total observations
cutPoint <- floor(nrow.df/3*2) # 2/3 split
rand <- sample(1:nrow.df) # randomize rows
df.train <- iter.3[rand[1:cutPoint],] # Create train data set
dim(df.train)
df.test <- iter.3[rand[(cutPoint+1):nrow.df],] # Create test data set
dim(df.test)

# Root Mean Squared Error
# Note: fix function
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# LM Model
lm.model <- lm(df.Satisfaction~., data=df.train)
summary(lm.model)
# Export to file for easier copy/paste
# sink("lm.txt")
# print(summary(lm.model))
# sink 
pred.lm <- predict(lm.model, df.test)
df.test$error1 <- df.test$df.Satisfaction - pred.lm
head(df.test)
rmse(df.test$error1)

# SVM
svm.model<-svm(df.Satisfaction~.,data=df.train)
summary(svm.model)
# Export to file for easier copy/paste
# sink("svm.txt")
# print(summary(svm.model))
# sink() # Returns output to the console
pred.svm <- predict(svm.model, df.test)
df.test$error2 <- df.test$df.Satisfaction - pred.svm
head(df.test)
rmse(df.test$error2)

# kSVM
ksvm.model<-ksvm(df.Satisfaction~.,data=df.train)
summary(ksvm.model)
# Export to file for easier copy/paste
# sink("ksvm.txt")
# print(summary(ksvm.model))
# sink() # Returns output to the console
pred.ksvm <- predict(svm.model, df.test)
df.test$error3 <- df.test$df.Satisfaction - pred.ksvm
head(df.test)
rmse(df.test$error3)

# Recode variable for Satisfaction
iter.4 <- iter.3
iter.4$SatifactionClass <- ifelse(iter.4$df.Satisfaction==5, "1", 
                           ifelse(iter.4$df.Satisfaction==4, "1",
                           ifelse(iter.4$df.Satisfaction==3, "0",
                           iflese(iter.4$df.Satisfaction==2, "0",
                           ifelse(iter.4$df.Satisfaction==1, "0",
                           "NA")))))
str(iter.4)
iter.4<-iter.4[,-1]
iter.4$SatisfactionClass <- as.factor(iter.4$SatisfactionClass)