#CPSC 375
#4/25/22
#Project 1 Vaccination Rate Modeling

#Loading the required libraries
library(ggplot2)
library(tidyverse)
library(modelr)
library(csv)
library(readr)

#1) Data preparation/wrangling to get all the data into one table that can be used for linear modeling
#1a reading the data files using read_csv()
vaccine_doses <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
#hospital_beds <- read_csv("CPSC 375 Work Environment/CSV/Project/data.csv")
#demographics <- read_csv("CPSC 375 Work Environment/CSV/Project/demographics (1).csv")
hospital_beds <- read_csv("Vaccination-Rate-Modeling/CSV/data.csv")
demographics <- read_csv("Vaccination-Rate-Modeling/CSV/demographics.csv")

#Views of the data sets before tidying
vaccine_doses %>% view("Vaccine Doses")
hospital_beds %>% view("Hospital Beds")
demographics %>% view("Demographics")

#1b Removing unneeded rows (e.g., countries like Brazil and India report Province_State-level data that is not needed as we are studying only country-level rates) and columns. 
#A filter to keep only the rows without a Province State to focus on county level data only
vaccine_doses <- vaccine_doses %>% filter(is.na(Province_State))
#Getting rid of irrelevant variables/columns from vaccine doses data
vaccine_doses <- subset(vaccine_doses, select= -c(Province_State, FIPS, Admin2, Lat, Long_, Combined_Key, iso2, UID, code3, iso3))

#1c tidying tables, as needed. For example, the vaccinations data is not tidy.
#pivot_longer to tidy (lengthen) data by giving date the string date variables and vaccinations the values (Number of Shots)
vaccine_doses.tidy <- vaccine_doses %>% pivot_longer(cols = starts_with("20"), names_to = "Date", values_to = "Vaccinations")
#Filtering out rows with vaccinations equal to 0
vaccine_doses.tidy <- vaccine_doses.tidy %>% filter(Vaccinations > 0) %>% group_by(Country_Region) 
#pivot_wider to tidy (shorten) data by removing an irrelevant column, splitting series code types into separate columns, and giving each series code its corresponding value
demographics.tidy <- demographics %>% select(-`Series Name`) %>% pivot_wider(names_from = "Series Code", values_from = YR2015)
#hospital_beds is already tidy as is

#Country Names Not Matching Fix
#Changing some country names so that they match across data sets
demographics.tidy <- demographics.tidy %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Korea, Rep.", "South Korea")) %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Iran, Islamic Rep.", "Iran"))
hospital_beds <- hospital_beds %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran")) %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea")) %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
vaccine_doses.tidy <- vaccine_doses.tidy %>% mutate(Country_Region = replace(Country_Region, Country_Region == "Korea, South", "South Korea"))

#1d Calculate the vaccination rate: vaccinations/population
#Creating a new vaccination rate column
vaccine_doses.tidy <- vaccine_doses.tidy %>% mutate(vaccination_rate= Vaccinations/Population) 

#1e Since the most important factor affecting vaccination rate is the number of days since vaccination began (vaccination rate always increases), calculate a variable that is: number of days since first non-zero vaccination number. This variable will be important for modeling.
#A function to return the difference between the current date and the first date 
daysSince <- function(x) { return (x-x[1])}
#Obtaining the day since the first vaccination for every row
vaccine_doses.tidy <- vaccine_doses.tidy %>% mutate(daysSinceStart=as.integer(daysSince(as.Date(Date)))) 
#Removing Date since it is not needed for linear modeling
vaccine_doses.tidy <- subset(vaccine_doses.tidy, select= -c(Date))
#Removing rows where days since first vaccination is 0
vaccine_doses.tidy <- subset(vaccine_doses.tidy, daysSinceStart!=0)

#1f Discard data that is not needed. For example, only the number of hospital beds from the most recent year is necessary.
#Getting hospital bed value from the most recent available year
hospital_beds <- hospital_beds %>% group_by(Country) %>% top_n(1, Year)
#Removing an unneeded variable and shortening a variable name
hospital_beds <- subset(hospital_beds, select= -c(Year))
hospital_beds <- hospital_beds %>% rename(beds=`Hospital beds (per 10 000 population)`)

#1g You can ignore sex-related differences in demographics in this project, so add the male/female population numbers together (already done in HW #5).
#adding male/female population numbers together
demographics.tidy.merged <- demographics.tidy %>% mutate(SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA) %>% mutate(SP.POP.1564.IN=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN) %>% mutate(SP.POP.0014.IN=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN) %>% mutate(SP.DYN.AMRT=SP.DYN.AMRT.MA+SP.DYN.AMRT.FE) %>% mutate(SP.POP.TOTL.IN=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN) %>% mutate(SP.POP.65UP.IN=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% select(-contains(".FE")) %>% select(-contains(".MA"))
demographics.tidy.merged.final <- demographics.tidy.merged
#keeping only the rows required for linear modeling
demographics.tidy.merged.final <- subset(demographics.tidy.merged.final, select= c("Country Name","SP.DYN.LE00.IN","SP.URB.TOTL","SP.DYN.AMRT","SP.POP.0014.IN","SP.POP.1564.IN","SP.POP.65UP.IN","SP.POP.80UP"))

#The three tidied and finalized data sets prior to joining
hospital_beds %>% view("HB Final")
vaccine_doses.tidy %>% view("VD Final")
demographics.tidy.merged.final %>% view("Demo Final")

#1h Merge all tables (Hint: Join using the country name)
#Merging all data sets
vaccine_doses_joined <- vaccine_doses.tidy %>% inner_join(hospital_beds, by= c(Country_Region = "Country")) %>% inner_join(demographics.tidy.merged.final, by = c(Country_Region="Country Name"))
#Renaming a variable
vaccine_doses_joined <- vaccine_doses_joined %>% rename(Country="Country_Region")
#Joined table
vaccine_doses_joined %>% view("Joined Table")

#2) Linear modeling the Covid vaccination rate, scatter plot, and bar graph

#2a
#Modeling the vaccination rate based on beds per capita, days since first vaccination, and country population
M1 <- lm(data=vaccine_doses_joined, formula=vaccination_rate~beds+daysSinceStart+Population)
summary(M1) #R-Squared: 0.6391

#Modeling the vaccination rate based on beds per capita, days since first vaccination, and the life expectancy at birth
M2 <- lm(data=vaccine_doses_joined, formula=vaccination_rate~beds+daysSinceStart+SP.DYN.LE00.IN)
summary(M2) #R-Squared: 0.7573

#Transforming multiple demographic variables by dividing the demographic size by the country population
vaccine_doses_joined_tranformed <- vaccine_doses_joined %>% mutate(urb_pop_prop=SP.URB.TOTL/Population) %>% mutate(pop_prop_0014=SP.POP.0014.IN/Population) %>% mutate(pop_prop_1564=SP.POP.1564.IN/Population) %>% mutate(pop_prop_65UP=(SP.POP.65UP.IN+SP.POP.80UP)/Population) 

#Modeling the vaccination rate by chaining together the beds per capita, days since first vaccination, country population, and life expectancy
M3 <- lm(data=vaccine_doses_joined_tranformed, formula=vaccination_rate~beds+daysSinceStart+Population+SP.DYN.LE00.IN)
summary(M3) #R-Squared: 0.7575

#Modeling the vaccination rate based on beds per capita, days since first vaccination, and the adult mortality rate
M4 <- lm(data=vaccine_doses_joined_tranformed, formula=vaccination_rate~beds+daysSinceStart+SP.DYN.AMRT)
summary(M4) #R-Squared: 0.728

#Modeling the vaccination rate based on beds per capita, days since first vaccination, and the proportion of the urban population
M5 <- lm(data=vaccine_doses_joined_tranformed, formula=vaccination_rate~beds+daysSinceStart+urb_pop_prop)
summary(M5) #R-Squared: 0.7083

#Modeling the vaccination rate based on beds per capita, days since first vaccination, and the proportion of people under 14
M6 <- lm(data=vaccine_doses_joined_tranformed, formula=vaccination_rate~beds+daysSinceStart+pop_prop_0014)
summary(M6) #R-Squared: 0.7376

#Modeling the vaccination rate based on beds per capita, days since first vaccination, and the proportion of people between 15 and 64
M7 <- lm(data=vaccine_doses_joined_tranformed, formula=vaccination_rate~beds+daysSinceStart+pop_prop_1564)
summary(M7) #R-Squared: 0.7088

#Modeling the vaccination rate based on beds per capita, days since first vaccination, and the proportion people over 65
M8 <- lm(data=vaccine_doses_joined_tranformed, formula=vaccination_rate~beds+daysSinceStart+pop_prop_65UP)
summary(M8) #R-Squared: 0.664

#2b Creating the scatter plot of only the most recent vaccination rate for every country and the number of days since first vaccination

#Obtaining the most recent vaccination for each country
most_recent <- vaccine_doses_joined %>% group_by(Country) %>% top_n(1, daysSinceStart)
#Removing NA's from most_recent
most_recent <- most_recent %>% drop_na(vaccination_rate)
#Scatter plot of days since first vaccination versus vaccination rate 
ggplot(data=most_recent) + geom_point(mapping = aes(y=vaccination_rate, x=daysSinceStart)) 

#2c Summary Bar Graph for the R-Squared values of each model

#Constructing a data frame to hold the model names, and R-Squared values
models.df <- data.frame(Models = c("M1","M2","M3","M4","M5","M6","M7","M8"), R_Squared = c(0.6391,0.7573,0.7575,0.728,0.7083,0.7376,0.7088,0.664))
#Summarizing each model's r-squared value
ggplot(models.df, aes(Models, R_Squared)) + geom_col()

