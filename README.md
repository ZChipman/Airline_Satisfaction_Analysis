# Airline_Satisfaction_Analysis

## Background and Business Questions:

This started as the final project for my Introduction to Data Science class at Syracuse University.
Our team was tasked to review a survey of 129,543 responses taken from January to March of 2014 to determine the factors that most impact customer satisfaction across 14 different fictitious airlines. The primary objective was to identify areas for industry-level improvement, which will have the greatest impact on improving long-term satisfaction. The original project was a collaboration between me and two classmates: Andrew Klassen and Mark Scholz. Any improvements to the code will be a solo effort.

Below are the questions we sought to answer as we felt they were relevant to our research:

What is the profile of an average airline customer?

How do each of the airlines compare to each other on satisfaction?

Are personal travelers more or less satisfied when compared to business travelers? How about when customers use their rewards? Is the satisfaction of these customer segments driven by different factors? 

Which attributes are key drivers of airline customer satisfaction? How can we use this information to improve satisfaction ratings over time?

How impactful are departure and arrival delays in reduced customer satisfaction? Which is more impactful? At approximately how long does a delay have to be (in minutes) for satisfaction to fall dramatically.

Is airline status impactful in driving satisfaction? Should airlines continue investment in this area?


## Files:

Airline_Satisfaction_Report: The final project report presented during the IST657 class. It is in a PDF format and no special software should be needed to view it. The first part of the report is our findings and analysis and the second part if the R code used in the project.

Airline_Satisfaction_Code.R They raw code used to create the analysis featured in the report. 

Airline_Satisfaction_Survey.xslx: The Excel sheet containing the data used in this project.

Attributes.docx: Explains the meaning of each column in the Excel file.

## Technical Requirements:

Any application running R (like R Studio) should be able to replicate this project. If there are any instances of deprecated code when using more recent versions of R, feel free to let me know.

## Development:

### Data Cleaning and Transformation:

Our team performed the following steps to clean and transform the data for analysis:

1.	Replace blanks with NAs. 

2.	Transform column names.

3.	Remove 9 cases with misrepresented satisfaction ratings.

4.	Impute the values of the “Percent of Flight with other Airlines” column that were greater than 100%.

5.	Remove records that do not have a flight time or arrival delay metric AND do not say their flight was cancelled.

6.	Added an index, or what we called an “Unique Identifier,” column to the dataframe.

7.	Added an initial set of NEW variables for analysis. 

8.	Converted categorical variables to binary, and satisfaction to binary (4-5) vs. (1-3) for our classification models. 

### Visualizations:

Below are the most noteworthy images from our analysis, more can be found in the written report.

#### Origin/Destination State Frequency & Average Departure/Arrival Delays:

One of our data questions asked how impactful departure and arrival delays were to customer satisfaction. To help answer this question, we wanted to pinpoint the locations that have the highest average delays. On the maps below, delays are marked by red circles, with the larger circles being states with the highest average delays.

CA & TX have the most flights originating, followed by FL. VT, DE, and WV have significant room for improvement on departure delays. The Northeast is more likely to have delays, perhaps impacted by weather conditions.  

![image](https://github.com/ZChipman/Airline_Satisfaction_Analysis/assets/87530934/a439585a-9403-4d6b-8e04-3d194d99d4b5)

![image](https://github.com/ZChipman/Airline_Satisfaction_Analysis/assets/87530934/8f99204c-11a5-477c-8faa-01ec8cb51fa2)

#### Average Satisfaction by Airline:

This visualization shows the average satisfaction across airlines sorted by performance confirms significant differences are not present.  Therefore, the analysis that follows is on the airline industry, keeping these airlines aggregated. 

![image](https://github.com/ZChipman/Airline_Satisfaction_Analysis/assets/87530934/396c2050-3a74-4e84-aaa9-98289050bfd9)

#### Satisfaction by Traveler Type:

Mileage Tickets and Business Travel have proportionally greater satisfaction; with those using Mileage Tickets having the lowest proportion of dissatisfied customers. This chart also shows that the largest customer base across all airlines is Business Travelers. 

![image](https://github.com/ZChipman/Airline_Satisfaction_Analysis/assets/87530934/6b3c4c7f-64b7-4444-9dac-a2b7db4d4400)

#### Satisfaction by Airline Status:

Airline performance on satisfaction appears to peak at “Silver” status. Personal Platinum status customers are the least satisfied.  Are Gold and Platinum tiers offering perks that appeal to these types of customers?  Are all 4 status types necessary?  Personal Platinum status customers are the LEAST satisfied.

![image](https://github.com/ZChipman/Airline_Satisfaction_Analysis/assets/87530934/6e5b708a-b070-4738-9610-d796d8e19645)

### Models:

While our best predictive model is our SVM (with a prediction accuracy of 79%), our final model is the Linear Model with the variables displayed in the below table. This model is clearest for us to evaluate as the coefficients are easy to interpret. We found whether a traveler is a personal traveler has the largest impact on the passenger’s satisfaction and that impact is negative. Airline Status is the next highest, with gender, class, and prices sensitivity also being notable variables. Surprisingly, delays seem to be less impactful than we initially thought, with the caveat that as arrival delays get longer (40 minutes +), they become a more important negative variable. 

![image](https://github.com/ZChipman/Airline_Satisfaction_Analysis/assets/87530934/b14f063c-0105-4487-9c24-a21f584084ac)

## Interpretation of Results:

1. Satisfaction is driven by factors outside of which airlines these customers use. All airlines show room for improvement, with ~50% of customers satisfied (rating of 4 or 5) across the industry.
2. Airlines should consider focused efforts on improving the experience among personal travelers by conducting a deep dive analysis, speaking to customers directly using a marketing research initiative. 
3. Continue to examine why Silver status members are more satisfied than Gold+ members and try to reduce delays in the Northeast. 
