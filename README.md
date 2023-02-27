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


