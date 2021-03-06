library(tidyverse)

## Download the file and load into memory
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")
surveys <- read.csv('data/portal_data_joined.csv')

## View the contents of the file and the measured variables
## for each observation
surveys
head(surveys)
view(surveys)
?read.csv
str(surveys)


## Inspect the different parts of the survey data frame

# Different sizes of the csv file
dim(surveys)
nrow(surveys)
ncol(surveys)

# Idea of the content of csv file
head(surveys)
tail(surveys)

# Names for the variables and observations, resp., in the csv file
names(surveys)
rownames(surveys)

# Structure information for overall file and each variable
str(surveys)
# More detailed statistical information about each variable (col)
summary(surveys)

## Challenge Question 1

str(read.csv("data/portal_data_joined.csv", stringsAsFactors = TRUE))

# 1. surveys is a data.frame class object.
# 2. There are 34786 rows and 13 columns in surveys.csv.
# 3. There are 40 unique species in surveys.csv and 48 unique species IDs.
#    This can be checked using the following function - 
length(unique(surveys$species))
# Note that if you try to use the str(surveys) function, it won't
# list all the factors for each variable since surveys didn't read 
# the file using stringsAsFactors = TRUE (False by default) 

## Indexing and creating subsets of a data frame

# First element in first column. Note that index numbering starts
# at 1, not 0 like in Python (why?) and the header column is excluded
surveys[1,1]
# First element in the sixth column
surveys[1,6]
# Entire first column in the data frame
surveys[,1]
# First three elements of the seventh column
surveys[1:3,7]
# Third row (observation) of the file
surveys[3,]
# First 6 rows of the file, just like head(surveys)
head_surveys <- surveys[1:6,]
head_surveys

# Testing using colon function to create patterned vectors 
1:10
10:1
# First number is the start number, second is the end number 
# There is likely a way to add steps to this basic function
# to create different patterns

# Excluding data using negative numbers
# All but the first column
surveys[,-1]
# Only the first six rows, like head(surveys)
surveys[-(7:nrow(surveys)),]

# Calling specific variables via column names
surveys["species_id"] # This is a data.frame object.
surveys[, 'species_id'] # This is just a vector
surveys[['species_id']] # This is also just a vector
# Perhaps because the data frame consists of only one column?
surveys$species_id      # This is also a vector 

## Challenge Question 2

# 1. Data.frame object with only row 200's data from surveys
surveys_200 <- surveys[200,]
surveys_200
# 2. Pull out only the last row's data from surveys 
surveys[34786,]
# Compare with the end of surveys via tail:
tail(surveys)
# Use nrow function to pull out the last row
surveys_tail <- surveys[nrow(surveys),]
surveys_tail
# 3. Extract the data from the middle row of surveys (assuming half)
surveys_middle <- surveys[nrow(surveys)/2,]
surveys_middle
# 4. Use nrow() with - to reproduce head(surveys)
surveys_head <- surveys[-(7:nrow(surveys)),]
surveys_head
head(surveys)

## Factors
# Factors are special class used to track each unique variable
# in a vector or data frame

# Create a vector with two different factors
sex <- factor(c('male', 'female', 'female', 'male'))
# See the levels and number of levels for this vector
# Note that female is first (1) since it is alphabetically first
levels(sex)
nlevels(sex)

# Ordering the levels in a vector
sex   # Original order
sex <- factor(sex, levels = c('male', 'female'))
sex   # Reordered levels while the order in the vector stays same

# Converting factors
as.character(sex)   # Converts factor to a character vector
# Note that all vectors shown are now in quotes, indicating strings

# Converting factors to numerical values for manipulation
# as.numeric doesn't work since it returns each input's 
# corresponding factor value
as.numeric(sex)
# To actually convert to numbers for math operations, can use levels()
# for comparison, 
year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(year_fct)    #Returns just factor numbers for inputs
as.numeric(as.character(year_fct))    #Clumsy but correct
as.numeric(levels(year_fct))[year_fct]  #Recommended

# Renaming factors
# Can use plot() to see bar graph of number of each factors in dataset
plot(as.factor(surveys$sex))

# Pull out the data for the sex of all individuals in survey to manipulate
sex <-factor(surveys$sex)  # This overwrites the previous sex vector
head(sex)
levels(sex)
#Change the first factor to undetermined
levels(sex)[1] <- "undetermined"
levels(sex)
head(sex)

## Challenge Question 3

# 1. Rename F and M to female and male, respectively
levels(sex)[2:3] <- c('female', 'male')
levels(sex)
# 2. Make the same plot as before with undetermined after male
sex <- factor(sex, levels = c('female', 'male', 'undetermined'))
plot(sex)

# Using stringsAsFactors = FALSE
# Compare the differences between surveys when using stringAsFactors
# set to True vs False
surveys <- read.csv("data/portal_data_joined.csv", stringsAsFactors = TRUE)
str(surveys)
surveys <- read.csv("data/portal_data_joined.csv", stringsAsFactors = FALSE)
str(surveys)
# As shown, when True,  the character variables have the number 
# of factors and list the factors as their numeric representations
# convert the column "plot_type" into a factor
surveys$plot_type   # Right now it is not a factor since stringsAsFactors = FALSE
surveys$plot_type <- factor(surveys$plot_type)
surveys$plot_type   # Now it is a factor since we used factor()

# Challenge Question 4
# 1. Find the mistakes in the following hand-made data frame -
# Original (with errors) -
animal_data <- data.frame(
  animal = c(dog, cat, sea cucumber, sea urchin),
  feel = c("furry", "squishy", "spiny"),
  weight = c(45, 8 1.1, 0.8)
  )
# Problems: 1 - objects (in 'animal' variable) must be in quotations.
#               Added quotations around to create the characters properly
#           2 - 'feel' variable is missing an observation.
#               Added "fluffy" to correspond to cat (second position)
#           3 - Missing comma in the 'weight' variable between 8 and 1.1.
#               Added the comma

# Fixed data frame - 
animal_data <- data.frame(
  animal = c("dog", "cat", "sea cucumber", "sea urchin"),
  feel = c("furry", "fluffy", "squishy", "spiny"),
  weight = c(45, 8, 1.1, 0.8)
  )
animal_data # Now is created properly

# 2. Predict the class for each variable in the following hand-made data frame -
country_climate <- data.frame(
  country = c("Canada", "Panama", "South Africa", "Australia"),
  climate = c("cold", "hot", "temperate", "hot/temperate"),
  temperature = c(10, 30, 18, "15"),
  northern_hemisphere = c(TRUE, TRUE, FALSE, "FALSE"),
  has_kangaroo = c(FALSE, FALSE, FALSE, 1)
  )
# Note that bc I am using R v4.0.2, stringAsFactors = False by default
# (it has been True by default in the past)
# country is character (quotations around all the strings makes character class)
# climate is character
# temperature is a character (mix of numbers and character makes character overall)
# northern_hemisphere is a character (mix of Boolean and character makes character overall)
# has_kangaroo is a number (the Booleans are converted to numbers)

# Checking predictions -
str(country_climate)

# In my case, if stringAsFactors had been True, then all characters would 
# instead be factors.
# To ensure all columns had the expected data types, I would change the following -
# 1 - Remove the quotations around 15 in temperature to make it number
# 2 - Remove the quotations around the last "FALSE" in northern_hemisphere
#     to make it logic
# 3 - Replace the 1 with TRUE in has_kangaroo to make it logic
# Data frame with changes - 
country_climate <- data.frame(
  country = c("Canada", "Panama", "South Africa", "Australia"),
  climate = c("cold", "hot", "temperate", "hot/temperate"),
  temperature = c(10, 30, 18, 15),
  northern_hemisphere = c(TRUE, TRUE, FALSE, FALSE),
  has_kangaroo = c(FALSE, FALSE, FALSE, TRUE)
)
str(country_climate)


## Formatting Dates

# Notice the different variables for day, month, and year
str(surveys)
# Import the lubridate package
library("lubridate")
# Create a simple date vector, best in form of YYYY-MM-DD
my_date <- ymd("2015-01-01")
str(my_date)
# Now create the same Date vector by manually pasting each part
my_date <- ymd(paste("2015", "1", "1", sep = "-"))
str(my_date)

# Now apply this function to surveys using the paste() function
# for the appropriate variables in the data frame
paste(surveys$year, surveys$month, surveys$day, sep = "-")
ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
# We got an error that 129 failed to parse.

# Add the results to the surveys data frame by adding a new "date" variable
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
# Got the same error that 129 failed to parse

str(surveys)  # New date variable of Date (with "format") class 
summary(surveys) # Notice that 129 values are NA, which are the ones that failed to parse

# Investigate which ones failed to parse and figure out why
# Do so by picking out the year, month, and day variables for the observations with NA as the date
missing_dates <-surveys[is.na(surveys$date), c("year", "month", "day")]
head(missing_dates)
# The missing dates shown in the head are all dates that don't actually exist 
# according to the Gregorian calender (September and April only have 30 days)
# If these data were in my analyses, I would likely keep the date as "NA"
# since the actual day is unknown, If I needed the date for analysis,
# I wouldn't use these data, but if not, then I feel like the rest of 
# the information for that observation is fine to use.