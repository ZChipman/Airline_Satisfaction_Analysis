# Specify the packages of interest
packages=c("maps","zipcode","mapproj","ggmap","ggplot2","readxl","lmtest")

# install.packages("MASS", dependencies=TRUE)
# install.packages("car", dependencies=TRUE)

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
library(lmtest)
# library(MASS)
# library(car)

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

str(df$Satisfaction.Coded)
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


# # Factorizing Variables
# df$Satisfaction <- as.integer(df$Satisfaction)
# df$Price.Sensitivity <- ordered(df$Price.Sensitivity)
# df$Gender <- as.factor(df$Gender)
# df$Type.of.Travel <- as.factor(df$Type.of.Travel)
# df$Airline.Status <- as.factor(df$Airline.Status)
# df$Class <- as.factor(df$Class)
# 
# dfLMModel <- data.frame(df$Satisfaction,df$Age,df$No.of.Flights.p.a.,
#                         df$Shopping.Amount.at.Airport,
#                         df$Arrival.Delay.in.Minutes,
#                         df$Flight.time.in.minutes,df$Flight.Distance,
#                         df$Airline.Status, df$Type.of.Travel, df$Class,
#                         df$Gender)


# Remove NAs from linear model
# We want data across these variables
dfLMModel <- na.omit(dfLMModel)
str(dfLMModel)

model.all<-lm(df.Satisfaction~.,data=dfLMModel)
summary(model.all)

nrow.df <- nrow(dfLMModel) # Total observations
print(nrow.df)
cutPoint <- floor(nrow.df/3*2) # 2/3 split
rand <- sample(1:nrow.df) # randomize rows
df.train <- dfLMModel[rand[1:cutPoint],] # Create train data set
dim(df.train)
df.test <- dfLMModel[rand[(cutPoint+1):nrow.df],] # Create test data set
dim(df.test)

# Create decision tree using train data 
# tree <- rpart(df.Satisfaction~., data = df.train, cp=.02)
# rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

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

# install.packages("rpart")
# install.packages("rpart.plot")
# library(rpart)
# library(rpart.plot)

# tree <- rpart(df.Satisfaction~., data = df.train, cp=.02)
# rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)


# # Checking for non-linearity within the data
# resettest(df.Satisfaction~., power=2:3, type='regressor', data=df.train)
# 
# boxTidwell(df.Satisfaction~df.Flight.time.in.minutes, data=df.train, tol=0.001, max.iter=25)
# 
# df.new <- df.train
# 
# # Variable Transformations
# df.new$df.Flight.time.in.minutes <- (df.train$df.Flight.time.in.minutes)^(-2)
# 
# # Check new model
# lm.new <- lm(df.Satisfaction~., data=df.new)
# summary(lm.new)

# LM Model
lm.model <- lm(df.Satisfaction~., data=df.train)
summary(lm.model)
# Export to file for easier copy/paste
# sink("lm.txt")
# print(summary(lm.model))
# sink 
pred.lm <- predict(lm.model, df.test)
# print(pred.lm)
df.test$error1 <- df.test$df.Satisfaction - pred.lm
head(df.test)
rmse(df.test$error1)
# sqrt(mean((df.test$error1)^2))


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
iter.4$SatisfactionClass <- ifelse(iter.4$df.Satisfaction==5, "1", 
                           ifelse(iter.4$df.Satisfaction==4, "1",
                           ifelse(iter.4$df.Satisfaction==3, "0",
                           ifelse(iter.4$df.Satisfaction==2, "0",
                           ifelse(iter.4$df.Satisfaction==1, "0",
                           "NA")))))
str(iter.4)
iter.4<-iter.4[,-1]
iter.4$SatisfactionClass <- as.factor(iter.4$SatisfactionClass)

# Create train/test dt for Iter.4
nrow.df <- nrow(iter.4) # Total observations
cutPoint <- floor(nrow.df/3*2) # 2/3 of the count
rand <- sample(1:nrow.df) # Randomize rows
df.train2 <- iter.4[rand[1:cutPoint],] # Create train data set
dim(df.train2)
df.test2 <- iter.4[rand[(cutPoint+1):nrow.df],] # Create test data set 
dim(df.test2)


length(df.test$SatisfactionClass)



# SVM
svm.model.class <- svm(SatisfactionClass~., data = df.train2)
# Export file for easier copy/paste
# sink("svmclass.txt")
print(summary(svm.model.class))
# sink()
# Review predictions 
df.test2$predictSatSVM <- predict(svm.model.class, df.test2)
str(df.test2)
results <- table(df.test2$SatisfactionClass, df.test2$predictSatSVM)
print(results)
percentCorrect <- (results[1,1] + results[2,2] / results[1,1] + results[1,2] + 
                     results[2,1] + results[2,2]) * 100
cat("Percent Correct: ", round(percentCorrect), "\n")

# kSVM
ksvm.model.class <- ksvm(SatisfactionClass~., data = df.train2)
# Export file for easier copy/paste
# sink("ksvmclass.txt")
print(summary(ksvm.model.class))
# sink()
# Review predictions 
df.test2$predictSatkSVM <- predict(ksvm.model.class, df.test2)
str(df.test2)
results <- table(df.test$SatisfactionClass, df.test2$predictSatkSVM)
print(results)
percentCorrect <- (results[1,1] + results[2,2] / results[1,1] + results[1,2] + 
                     results[2,1] + results[2,2]) * 100
cat("Percent Correct: ", round(percentCorrect), "\n")

#-------Creating Variables for Analysis-------

# No. of other Loyalty Cards:  add a column with (0=none) and (1=loyalty member)
# Check for NAs
length(df$No..of.other.Loyalty.Cards[df$No..of.other.Loyalty.Cards=='NA']) 
df$LoyaltyCardCat <- ifelse(df$No..of.other.Loyalty.Cards>0, "Member", 
                            "Non-Member")
g <- ggplot(df, aes(x=LoyaltyCardCat))
g <- g + geom_bar(color='black', fill='gray')
g

# Shopping Amount at Airport:  add a column with (0=non-shopper) and (1=shopper)
# Check for NAs
length(df$Shopping.Amount.at.Airport[df$Shopping.Amount.at.Airport=='NA']) 
df$ShoppingCat <- ifelse(df$Shopping.Amount.at.Airport>0, "Shopper", 
                         "Non-Shopper")
g <- ggplot(df, aes(x=ShoppingCat))
g <- g + geom_bar(color='black', fill='gray')
g

# Eating and Drinking at Airport:  add a column with (0=non-diner) and (1=diner)
# Check for NAs
length(df$Eating.and.Drinking.at.Airport[df$Eating.and.Drinking.at.Airport=='NA']) 
df$DinerCat <- ifelse(df$Eating.and.Drinking.at.Airport>0, "Diner", "Non-diner")
g <- ggplot(df, aes(x=DinerCat))
g <- g + geom_bar(color='black', fill='gray')
g 

table(df$DinerCat) # We see there isn't many non-diners for analysis
table(df$DinerCat) / length(df$DinerCat) # In fact, less than 5% are non-diners

#-------Additional Feature Profile------

# LOYALTY CARD
TempDf <- data.frame(table(df$LoyaltyCardCat) / length(df$LoyaltyCardCat))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + 
  geom_text(aes(label=paste0(round(Freq*100), "%")), 
            position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Loyalty Membership")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, 
                                 vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

# SHOPPER
TempDf <- data.frame(table(df$ShoppingCat) / length(df$ShoppingCat))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), 
                                            "%")), 
                                            position=position_stack(vjust=0.5), 
                                            color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Airport Shopping")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, 
                                 vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

# DINER
TempDf <- data.frame(table(df$DinerCat) / length(df$DinerCat))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), 
                                     "%")), position=position_stack(vjust=0.5), 
                                     color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Airport Dining")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, 
                                 vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

#-------Demographic Profile-------
#GENDER
TempDf <- data.frame(table(df$Gender) / length(df$Gender))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", color="white", width=0.35)
g <- g + geom_text(aes(label=paste0(round(Freq*100), "%")), 
                   position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue4", "gray26")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Gender")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.5, 
                                 vjust=-1, color = "black", size=20),
                                 legend.text = element_text(size=15),
                                 legend.position = "bottom")
g

#AGE
g <- ggplot(df, aes(x=Age))
g <- g + geom_histogram(color="white", fill="dodgerblue4", binwidth=5)
g <- g + labs(title = "Age Distribution") + 
  theme(plot.title = element_text(hjust=0.5, vjust=2, size=20))
g

#-------Usage Profile-------
#AIRLINE STATUS
TempDf <- data.frame(table(df$Airline.Status) / length(df$Airline.Status))
TempDf$Var1 <- factor(TempDf$Var1, levels=c("Blue", "Silver", "Gold", 
                                            "Platinum"))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", color="white", width=0.35)
g <- g + geom_text(aes(label=paste0(round(Freq*100), "%")), 
                   position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("blue4", "grey75","darkgoldenrod2", 
                                    "gray35")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Airline Status")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.5, 
                                 vjust=-1, color = "black", size=20),
                                 legend.text = element_text(size=15),
                                 legend.position = "right")
g

#NUMBER OF FLIGHTS
g <- ggplot(df, aes(x=No.of.Flights.p.a.))
g <- g + geom_histogram(color="white", fill="dodgerblue4", binwidth=5)
g <- g + labs(title = "Number of Flights") + 
  theme(plot.title = element_text(hjust=0.5, vjust=2, size=20))
g

#TYPE OF TRAVELER
TempDf <- data.frame(table(df$Type.of.Travel) / length(df$Type.of.Travel))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", width=1, color="white")
g <- g + coord_polar("y", start=0) + geom_text(aes(label=paste0(round(Freq*100), 
                                     "%")), position=position_stack(vjust=0.5), 
                                     color="white", size=5)
g <- g + scale_fill_manual(values=c("indianred3", "chartreuse3", "royalblue2")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Type of Travel")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.52, 
                                 vjust=-5, color = "black", size=20),
                                 legend.text = element_text(size=15))
g

#CLASS
TempDf <- data.frame(table(df$Class) / length(df$Class))
TempDf$Var1 <- factor(TempDf$Var1, levels=c("Eco", "Eco Plus", "Business"))
TempDf
g <- ggplot(TempDf, aes(x="", y=Freq, fill=Var1))
g <- g + geom_bar(stat="identity", color="white", width=0.35)
g <- g + geom_text(aes(label=paste0(round(Freq*100), "%")), 
                   position=position_stack(vjust=0.5), color="white", size=5)
g <- g + scale_fill_manual(values=c("dodgerblue1", "dodgerblue4","black")) 
g <- g + labs(x = NULL, y = NULL, fill = NULL, title = "Class")
g <- g + theme_classic() + theme(axis.line = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 plot.title = element_text(hjust = 0.5, 
                                 vjust=-1, color = "black", size=20),
                                 legend.text = element_text(size=15),
                                 legend.position = "left")

#Create satisfaction variable coded (4-5, 3, 1-2)
df$Satisfaction.Coded <- ifelse(df$Satisfaction==5, "4-5", 
                         ifelse(df$Satisfaction==4, "4-5", 
                         ifelse(df$Satisfaction==3, "3", 
                         ifelse(df$Satisfaction==2, 
                         "1-2",ifelse(df$Satisfaction==1,
                         "1-2", 'NA'))))) 

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
g <- g + geom_histogram(stat="count", color="black", 
                        aes(fill=Satisfaction.Coded))
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Satisfaction by Airline") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("Count of Records") + 
  labs(fill="Satisfaction Grouped")
g <- g + scale_fill_manual(values = c("darkred", "gray", "dodgerblue4"))
g

#-------Satisfaction by Airline Visualized as Percentages-------
g <- ggplot(df, aes(fill=factor(Satisfaction), y=Unique, x=Airline.Name))
g <- g + geom_bar(position="fill", stat="identity") 
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Satisfaction by Airline") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("") + labs(fill="Satisfaction")
g <- g + scale_fill_manual(values = c("darkred", "indianred2", "gray", 
                                      "dodgerblue1", "dodgerblue4"))
g

#-------Satisfaction by Airline Visualized as Y=Average-------
TempDf <- data.frame(tapply(df$Satisfaction, df$Airline.Name, mean))
TempDf$Airline.Name <- row.names(TempDf)
names(TempDf)[1] <- "Value"
g <- ggplot(TempDf, aes(x=reorder(Airline.Name, -Value), y=Value))
g <- g + geom_bar(stat="identity", color="black",fill="dodgerblue4")
g <- g + theme(axis.text.x = element_text(angle = 90, size=12))
g <- g + ggtitle("Average Satisfaction by Airline Sorted") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Name") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g <- g + geom_text(aes(label=round(Value,2)), 
                   position=position_dodge(width=0.9), vjust=-0.25)
g


#-------Airline Average Arrival Delay Comparison-------
g <- ggplot(df, aes(x=Airline.Name, y=Arrival.Delay.in.Minutes))
g <- g + geom_bar(stat="summary", fun="mean", color="black", fill="dodgerblue4")
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + xlab("Airline Name") + ylab("Arrival Delay (minutes)")
g

#-------Satisfaction by Loyalty Membership (Split Business vs. Personal) with y=Average-------
g <- ggplot(df, aes(x=LoyaltyCardCat, y=Satisfaction, group=Type.of.Travel, 
                    color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + ggtitle("Average Satisfaction by Loyalty Membership & Travel Type") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Loyalty Membership") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g <- g + geom_point(stat="summary", fun="mean", size=3)
g

#-------Satisfaction by Age & Traveler Type-------
g <- ggplot(df, aes(x=Age, y=Satisfaction, group=Type.of.Travel, 
                    color=Type.of.Travel))
g <- g + geom_point(stat="summary", fun="mean", size=1.75)
g <- g + ggtitle("Average Satisfaction by Age & Travel Type") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Age of Customer") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g

#-------Satisfaction by Airline Status & Traveler Type-------
g <- ggplot(df, aes(x=factor(Airline.Status, level = c("Blue", "Silver", "Gold", 
                                                       "Platinum")), 
                    y=Satisfaction, group=Type.of.Travel, color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + geom_point(stat="summary", fun="mean", size=3)
g <- g + ggtitle("Average Satisfaction by Airline Status & Travel Type") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Airline Status") + ylab("Average Satisfaction")
g <- g + coord_cartesian(ylim = c (1, 5))
g

#-------Satisfaction by Gender & Traveler Type-------
g <- ggplot(df, aes(x=Gender, y=Satisfaction, group=Type.of.Travel, 
                    color=Type.of.Travel))
g <- g + geom_line(stat="summary", fun="mean", size=1.5)
g <- g + geom_point(stat="summary", fun="mean", size=3)
g <- g + ggtitle("Average Satisfaction by Gender & Travel Type") + 
  theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Gender") + ylab("Average Satisfaction")
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

Destination_State_Area <- ggplot(Destination_State, 
                                 aes(map_id = DestinationState)) 
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

# Too much to geocode we plan to comment out
# UniqueOC <- sort(table(df$Orgin.City)) 
# UniqueOC <- data.frame(UniqueOC) 
# colnames(UniqueOC) <- c("OriginCity","count") 
# View(UniqueOC)

# UniqueDC <- sort(table(df$Destination.City)) 
# UniqueDC <- data.frame(UniqueDC) 
# colnames(UniqueDC) <- c("DestinationCity","count") 
# View(UniqueDC)

# MS Additions....

# Average Departure Delay by Origin State Data Frame
dfNoNA <- na.omit(df) #Remove NAs from df
TempDf <- data.frame(tapply(dfNoNA$Departure.Delay.in.Minutes, 
                            dfNoNA$Origin.State, mean))
TempDf$Origin.State <- row.names(TempDf)
names(TempDf)[1] <- "Departure.Delay.in.Minutes"
TempDf <- TempDf[-39,] #Remove Puerto Rico
TempDf <- TempDf[-44,] #Remove U.S Pacific Trust Territories
TempDf[order(-TempDf$Departure.Delay.in.Minutes),]
# View(TempDf)
Origin_State$Mean.Departure.Delay <- TempDf$Departure.Delay.in.Minutes
# View(Origin.State)

OS <- ggplot(Origin_State, aes(map_id = OriginState))
OS <- OS + geom_map(map = us, aes(fill = Origin_State$count)) 
OS <- OS + expand_limits(x = Origin_State$x, y = Origin_State$y) 
OS <- OS + coord_map() + ggtitle("Origin State w/ Average Departure Delays")
OS <- OS + geom_point(data=Origin_State, aes(x=Origin_State$x, y=Origin_State$y), 
                      size=Origin_State$Mean.Departure.Delay, color="#800000b5")
OS

# Average Arrival Delay by Destination State Data Frame
TempDf <- data.frame(tapply(dfNoNA$Arrival.Delay.in.Minutes, 
                            dfNoNA$Destination.State, mean))
TempDf$Destination.State <- row.names(TempDf)
names(TempDf)[1] <- "Arrival.Delay.in.Minutes"
TempDf <- TempDf[-39,] #Remove Puerto Rico
TempDf <- TempDf[-44,] #Remove U.S Pacific Trus Territories
TempDf[order(-TempDf$Arrival.Delay.in.Minutes),]
# View(TempDf)
Destination_State$Mean.Arrival.Delay <- TempDf$Arrival.Delay.in.Minutes
# View(Destination_State)

DS <- ggplot(Destination_State, aes(map_id = DestinationState))
DS <- DS + geom_map(map = us, aes(fill = Destination_State$count))
DS <- DS + expand_limits(x = Destination_State$x, y = Destination_State$y) 
DS <- DS + coord_map() + ggtitle("Destination State w/ Average Arrival Delays")
DS <- DS + geom_point(data=Destination_State, aes(x=Destination_State$x, 
                                                  y=Destination_State$y), 
                      size=Destination_State$Mean.Arrival.Delay, 
                      color="#800000b5")
DS
