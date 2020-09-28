# Import tidyverse
library('tidyverse')

## Joining tables

# Download, save, and view the JHU COVID reports for 13 March and 13 September 2020, and
# format both data sets to calculate the total number of confirmed cases for all US states/territories
Confirmed_State_3_13 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-13-2020.csv")) %>%
  rename(Country_Region = "Country/Region", Province_State = "Province/State") %>% # Need to rename for ggplot
  filter(Country_Region == 'US') %>% 
  group_by(Province_State, Country_Region) %>% 
  summarize(Confirmed = sum(Confirmed))
str(Confirmed_State_3_13) # 53 entries, for 50 states, plus DC and two cruise ships

Confirmed_State_9_13 <-   read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-13-2020.csv")) %>% 
  filter (Country_Region == "US") %>% 
  group_by(Province_State, Country_Region) %>% 
  summarise(Confirmed = sum(Confirmed)) 
str(Confirmed_State_9_13) # 58 entries, same as above plus 4 territories and "Recovered"

# To see these differences, us setdiff() function from dplyr -
setdiff(Confirmed_State_9_13$Province_State, Confirmed_State_3_13$Province_State)

# We don't need the Recovered data, so filter it out -
Confirmed_State_9_13 <- filter(Confirmed_State_9_13, Province_State != "Recovered")
str(Confirmed_State_9_13)

# Merge the data sets using the September report since all states and territories in this 
# set are found in the March set. Use the full_join function to include all observations -
Confirmed_State_3_13_9_13_joined <- full_join(Confirmed_State_3_13, Confirmed_State_9_13, by = c("Province_State"))
head(Confirmed_State_3_13_9_13_joined)
# Note that because it was joined by Province_State (and not the default), the 
# Country_Region and Confirmed variables are duplicated, with the value from the
# left (March) set appended with ".x" and the right (September) set appended with ".y"

# Look at the entries for Guam, Northern Mariana Islands, Puerto Rico, and the Virgin
# Islands for the March report - 
tail(Confirmed_State_3_13_9_13_joined, 5) # Note that "5" give the last 5 entries
which(is.na(Confirmed_State_3_13_9_13_joined))
# Gives the indexes of the entries that are NA, where the entries are numbered along the 
# lengths of the columns (i.e., first column is 1-57, next is 58-114, etc.)

# Turn the NAs into 0s for graphing, delete the Country_Region columns, and rename Confirmed - 
Confirmed_State_3_13_9_13_joined <- full_join(Confirmed_State_3_13, Confirmed_State_9_13,
                                              by = c("Province_State")) %>% 
  rename(Confirmed_3_13_2020 = "Confirmed.x", Confirmed_9_13_2020 = "Confirmed.y") %>% 
  select(-Country_Region.x, -Country_Region.y) %>% # Selects all except the Country_Region entries
  replace_na(list(Confirmed_3_13_2020 = 0)) # Turns this column into a list, then replaces NA with 0
head(Confirmed_State_3_13_9_13_joined)
# Check for NAs -
which(is.na(Confirmed_State_3_13_9_13_joined)) # None

## Switching between wide and long table formats 

# Note that a wide table format has a column for each variable and a row for each observation. 
# The categorical data are grouped for each observation for easier reading and interpretation
# (like the above data set).
# Long/narrow table format has a column for a variable type and a single column for 
# the values of that combination of variables. Every row is an observation for a particular 
# combination of an observation and categorical variable.

# gather() and spread() were used in the Data Carpentry tutorials to switch between wide and 
# long formats, respectively. However, they are being depreciated and replaced with 
# pivot_wider() and pivot_longer(), respectively

# Convert the joined table to long format - 
Confirmed_State_3_13_9_13_joined_long <- Confirmed_State_3_13_9_13_joined %>% 
  pivot_longer(-c(Province_State), names_to = "Date", values_to = "Confirmed")
# names_to argument is for the keys for the categorical variable (all but state names) while
# values_to argument is for the values of those keys (Confirmed cases for each date).

# Now, plot the data in a scatterplot. Adjsut the figure size in Rmd using fig.width and 
# fig.height in the R chunk declaration ({r, fig.width = 5, fig.height = 10})
ggplot(Confirmed_State_3_13_9_13_joined_long, aes(x = Confirmed, y = Province_State)) +
  geom_point(aes(color = Date))



## Working with the time series data

# Start by loading the most recent time series data for the confirmed cases from the Github repository.
# This can be done every time or downloaded and kept on your computer, then loaded into R.
# To download every time - 
time_series_confirmed <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region") # Still need to rename
# To save the data to the computer as a sort of snapshot - 
download.file(url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
              destfile = "data/time_series_covid19_confirmed_global.csv") # Saves to data folder
time_series_confirmed <- read_csv("data/time_series_covid19_confirmed_global.csv") %>% 
  rename(Province_State = "Province/State", Country_Region = "Country/Region")
head(time_series_confirmed) # check for appropriate importation
# This is in (very) wide format, so convert to long format -
time_series_confirmed_long <- time_series_confirmed %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long), 
               names_to = "Date", values_to = "Confirmed")
# Keeps the  Province, Country, Lat, and Long columns the same (as variables) and converts the 
# date columns into a single variable "Date" with the value "Confirmed".
head(time_series_confirmed_long) # Look at data

# It would be convenient to look at the confirmed cases and deaths over time. Create 
# another table with deaths using the same process and join the two tables - 
# Download, read to R, and rename - 
download.file(url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", 
              destfile = "data/time_series_covid19_deaths_global.csv")
time_series_deaths <- read_csv("data/time_series_covid19_deaths_global.csv")%>%
  rename(Province_State = "Province/State", Country_Region = "Country/Region")
# Convert to long format like above - 
time_series_deaths_long <- time_series_deaths %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long),
               names_to = "Date", values_to = "Deaths")
head(time_series_deaths_long)

# Joining the time series tables

# To join, we need a common column with a unique name (the key). Currently, no column does, 
# but we can make one by merging data in the Province_State, Country_Region, and Date columns 
# using unite() - 
time_series_confirmed_long <- time_series_confirmed_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = '.', remove = FALSE)
head(time_series_confirmed_long)
# For unite(), first argument is new column header name and the rest of the list are 
# the columns to unite. sep is the delimiter and remove is a TRUE/FALSE to remove the 
# united columns from the input data in the output data frame. Note that because remove
# was set to false, the Province_State, Country_Region, and Date columns are still in the
# long series tibble. Additionally, note that the sep and remove arguments must be 
# explicitly named arguments named after the list of columns to unite

# Do the same for the deaths table except remove the redundant columns - 
time_series_deaths_long <- time_series_deaths_long %>% 
  unite(Key, Province_State, Country_Region, Date, sep = '.') %>% # remove is implicitly TRUE
  select(Key, Deaths) # Select only these two columns of data
head(time_series_deaths_long)

# Now use full_join() to join both tables by matching values. If a value doesn't match, NA
# will be used for the missing values. select() can then be used to remove Key since we 
# will no longer need it.
time_series_long_joined <- full_join(time_series_confirmed_long, time_series_deaths_long,
                                     by = c("Key")) %>% 
  select(-Key)
head(time_series_long_joined)
# Note that only one table (confirmed) kept all column variables so that they would be kept
# in this full joined table

# Check that no Confirmed cases or Deaths have NA as a value (both tables were joined 
# correctly and have full data) - 
which(is.na(time_series_long_joined$Confirmed)) # None
which(is.na(time_series_long_joined$Deaths)) # None

# Since all data are present, reformat the Date using lubridate 
library(lubridate)
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)
# mdy parses the Date column to reformat the date into YYYY-MM-DD

# We may want to plot both Confirmed and Deaths on the same graph since they are related
# and both count data. One way to do so is to pivot long again - 
time_series_long_joined_counts <- time_series_long_joined %>% 
  pivot_longer(-c(Province_State, Country_Region, Lat, Long, Date), 
               names_to = "Report_Type", values_to = "Counts")
head(time_series_long_joined_counts)

## Making graphs from time series data

# To make the graph of the confirmed cases over time, must first summarize the Country data 
# to sum all data for countries with Province_State data (like the US). For just the 
# US data over time - 
time_series_long_joined %>% 
  group_by(Country_Region, Date) %>% 
  summarize_at(c("Confirmed", "Deaths"), sum) %>% # summarize_at() summarizes the listed columns with the given function
  filter(Country_Region == "US") %>% # Filter for only the US states/provinces
  ggplot(aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  ggtitle("US COVID-19 Deaths")

# Now, look at the US data in comparison with a few other countries (I added United 
# Kingdom and Germany out of curiosity) -
time_series_long_joined %>% 
  group_by(Country_Region, Date) %>% 
  summarize_at(c("Confirmed", "Deaths"), sum) %>% 
  filter(Country_Region %in% c("China", "Japan", "Korea, South", "Italy",
                               "Spain", "US",  "United Kingdom", "Germany")) %>% # Filter only the listed countries
  ggplot(aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  ggtitle("COVID-19 Deaths in Select Countries") +
  facet_wrap(~Country_Region, ncol=2, scales="free_y") # free_y allows all graphs to have their own y scale
# ~Country_Region facets the data by Country_Region of time_series_long_joined (like time_series_long_joined$Country_Region)

# Now, put the data on one graph - 
time_series_long_joined %>% 
  group_by(Country_Region, Date) %>% 
  summarize_at(c("Confirmed", "Deaths"), sum) %>% 
  filter(Country_Region %in% c("China", "Japan", "Korea, South", "Italy",
                               "Spain", "US",  "United Kingdom", "Germany")) %>%
  ggplot(aes(x = Date, y = Deaths, color = Country_Region)) +
  geom_point() +
  geom_line() +
  ggtitle("COVID-19 Deaths")

# Now, use the data frame with both Confirmed and Deaths data as counts to plot both 
# the number of Confirmed cases and Deaths in the US on one plot. Use log2 scale since
# Deaths are relatively low compared to Confirmed 
time_series_long_joined_counts %>% 
  group_by(Country_Region, Report_Type, Date) %>% 
  summarize(Counts = sum(Counts)) %>% 
  filter(Country_Region == "US") %>% 
  ggplot(aes(x = Date, y = log2(Counts), fill = Report_Type, color = Report_Type)) +
  geom_point() +
  geom_line() +
  ggtitle("US COVID-19 Cases")


## Exercises

# 2. Join tables like above for the 6/13/2020 and 9/13/2020 data, then make a bar graph 
# of the data 

# Download and parse the data - 
Confirmed_State_June_13 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-13-2020.csv")) %>%
  filter(Country_Region == "US", Province_State != "Recovered") %>% # Filter out all non-US data and Recovered data
  group_by(Province_State, Country_Region) %>% # Country_Region is kept to keep "US"
  summarize(Confirmed = sum(Confirmed))
head(Confirmed_State_June_13)

# Join the tibbles for the June and September data -
Confirmed_State_June_Sept_joined <- full_join(Confirmed_State_9_13, Confirmed_State_June_13,
                                              by = c("Province_State", "Country_Region")) %>% 
  rename(Confirmed_13_June_2020 = "Confirmed.x", Confirmed_13_September_2020 = "Confirmed.y") %>% 
  select(-Country_Region)
head(Confirmed_State_June_Sept_joined) # Check that data set was created properly
which(is.na(Confirmed_State_June_Sept_joined)) # There are no NA values

# Convert data to long format for plotting - 
Confirmed_State_June_Sept_joined_long <- Confirmed_State_June_Sept_joined %>% 
  pivot_longer(c(Confirmed_13_June_2020, Confirmed_13_September_2020), 
               names_to = "Date", values_to = "Count")
head(Confirmed_State_June_Sept_joined_long) # Formated correctly

# Now plot - 
basic_confirmed_states_plot <- ggplot(Confirmed_State_June_Sept_joined_long, aes(x = Province_State, y = Count, color = Date)) +
  geom_bar(stat = 'identity') +
  scale_y_log10() + # Make axes log10 so the differences can be seen more easily
  theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust = 1)) # Make the states readable
basic_confirmed_states_plot

## 3. Add a title and more informative x and y axes labels to the plot in Exercise 2
advanced_confirmed_states_plot <- basic_confirmed_states_plot +
  labs(x = "State/Territory", y = "Number of Confirmed Cases", title = "Total number of Confirmed COVID-19 Cases by State in June and September") +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        plot.title = element_text (size = 16, face = 'bold', hjust = 0.5))
advanced_confirmed_states_plot

## 4. Using the time series data, plot the total number of worldwide confirmed deaths per day over time

# Use the time series data for deaths from earlier in the tutorial -
head(time_series_deaths) # Checking what it looks like
# Pivoting to long format by dates, then summarizing for worldwide statistics by date
time_series_deaths_worldwide <- time_series_deaths %>% 
  select(-c(Lat, Long)) %>% # Filter out the columns Lat and Long
  pivot_longer(-c(Country_Region, Province_State), names_to = "Date", values_to = "Deaths") %>% # Must preserve Province_state, too
  group_by(Date) %>% 
  summarize(Total_Deaths = sum(Deaths))
head(time_series_deaths_worldwide) # Check that it was created properly
# Now adjust the date format and plot - 
time_series_deaths_worldwide$Date <- mdy(time_series_deaths_worldwide$Date)
plot_time_series_deaths_worldwide <- ggplot(time_series_deaths_worldwide, aes(x = Date, y = Total_Deaths)) +
  geom_point() +
  geom_line() + 
  labs(y = "Total Deaths", title = "Total Deaths from COVID-19 Worldwide over Time") +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        plot.title = element_text (size = 16, face = 'bold', hjust = 0.5))
plot_time_series_deaths_worldwide
# For fun, here it is with log10 axes - 
plot_time_series_deaths_worldwide + scale_y_log10()

## 5. Use Mutate to add a new column of deaths/confirmed cases in the time series data

# Start with the combined time_series_long_joined tibble from earlier that has the 
# Confirmed and Deaths for each observations for each date, then mutate to add another column
time_series_long_joined_ratio <- time_series_long_joined %>% 
  mutate(Death_Confirmed_Ratio = Deaths/Confirmed)
  # Decided to keep the NaN (not a number) values as is since data aren't plotted yet 
head(time_series_long_joined_ratio) # New column is now added


## 6. Plot US deaths/confirmed cases over time

# Take the above data set and filter out non-US data, then summarize for total US deaths 
# and confirmed, then re-calculated ratio for entire US, pivot_longer to get just the ratio,
# and plot over time. Note that this method will yield slightly different results than 
# averaging all the Death_Confirmed_Ratio values due to the nature of the ratio
time_series_US_ratio <- time_series_long_joined_ratio %>% 
  filter(Country_Region == "US") %>% 
  group_by(Date) %>% 
  summarize(Total_Deaths = sum(Deaths), Total_Confirmed = sum(Confirmed)) %>% 
  mutate(Death_Confirmed_Ratio = Total_Deaths/Total_Confirmed)
head(time_series_US_ratio)

# Plot - 
plot_time_series_US_ratio <- ggplot(time_series_US_ratio, aes(x = Date, y = Death_Confirmed_Ratio)) +
  geom_point() +
  geom_line() +
  scale_x_date(limits = as.Date(c('2020-01-01', '2020-10-01'))) + # Make is so the Jan label is on the x axis
  labs(y = "Ratio of Deaths to Confirmed Cases", title = "Death to Confirmed Cases Ratio in US over Time") +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        plot.title = element_text (size = 18, face = 'bold', hjust = 0.5))
plot_time_series_US_ratio
# Note the massive spike in March due to the large ratio of deaths to confirmed cases
# that happened at the beginning of the pandemic in the US.It's pretty jarring


## 7. Make a single graph of deaths over time for the 10 countries with the highest total deaths

# First, determine which countries have the highest deaths as of latest date in time series
# Unfortunately, I can't think of a more eloquent way to do so right now
deaths_top10 <- time_series_deaths %>% 
  select(-c(Lat, Long)) %>% # Remove the extraneous variables
  # First, need to sum total deaths for all countries since some have state data
  group_by(Country_Region) %>% 
  summarize(Total_Latest_Deaths = sum(`9/25/20`)) %>% # Not sure how to specify the data in the last column in R
  arrange(desc(Total_Latest_Deaths)) %>% 
  slice(0:10)
deaths_top10 # Check to see the top 10 countries

time_series_deaths_top10 <- time_series_deaths %>% 
  select(-c(Lat, Long)) %>% # Remove the extraneous variables
  filter(Country_Region %in% deaths_top10$Country_Region) %>%  # Filter for just the top 10 countries by deaths
  # Need to combine data for regions that have state data (i.e., France)
  # Only way I know how to right now is by pivoting and summarizing. I don't know how 
  # to do loops in R right now
  pivot_longer(-c(Province_State, Country_Region), names_to = "Date", values_to = "Deaths") %>% 
  group_by(Country_Region, Date) %>% 
  summarize(Total_Deaths = sum(Deaths))
mdy(time_series_deaths_top10$Date) -> time_series_deaths_top10$Date

head(time_series_deaths_top10)
# Data are now ready to plot in one large graph
plot_top10_deaths_worldwide <- ggplot(time_series_deaths_top10, aes(x = Date, y = Total_Deaths, color = Country_Region)) +
  geom_point() +
  geom_line() +
  labs(y = "Deaths", title = "COVID-19 Deaths by Country in the Top 10 Countries") +
  scale_x_date(limits = as.Date(c('2020-01-01', '2020-10-01'))) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = 'bold'))
plot_top10_deaths_worldwide


## 8. Use facet_wrap() to graph the data above in their own little graphs, adjusting settings as needed

plot_top10_deaths_worldwide_facet <- ggplot(time_series_deaths_top10, aes(x = Date, y = Total_Deaths, color = Country_Region)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country_Region, nrow = 5, scales = "free_y") + # Made the y axes scale independently to see trends
  labs(y = "Deaths", title = "COVID-19 Deaths by Country in the Top 10 Countries") +
  scale_x_date(limits = as.Date(c('2020-01-01', '2020-10-01'))) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = 'bold'))
plot_top10_deaths_worldwide_facet


## 9. Use facet_wrap() to graph the time series data for confirmed cases in all US states
# and territories. Format the graphs to look nice

# Download the data from the Github site and read to memory 
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
              destfile = 'data/time_series_covid19_confirmed_US.csv')
time_series_confirmed_US <- read_csv('data/time_series_covid19_confirmed_US.csv') %>% 
# Very large document with similar organization as before. States are in Province_State,
# plus some extra variables (columns) before. Still has Country_Region, Lat, and Long 
# variables, too. Filter out these extra columns (all but Province_State, Combined_Key,
# and the dates) by using select().
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Lat, Long_, Country_Region))  %>% 
  pivot_longer(-c(Province_State, Combined_Key), names_to = "Date", values_to = "Confirmed") %>% 
  group_by(Province_State, Date) %>% 
  summarize(Total_Confirmed = sum(Confirmed))
head(time_series_confirmed_US) # Check that the data set was parsed properly
length(unique(time_series_confirmed_US$Province_State)) # 58 unique entries
mdy(time_series_confirmed_US$Date) -> time_series_confirmed_US$Date # Don't forget this!

# Now plot the data
plot_time_series_confirmed_States <- ggplot(time_series_confirmed_US, aes(x = Date, y = Total_Confirmed, group = Province_State)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Province_State, ncol = 3, scales = 'free_y') +
  labs(y = "Confirmed Cases", title = "COVID-19 Confirmed Cases by US State/Territory over TIme") +
  scale_x_date(limits = as.Date(c('2020-01-01', '2020-10-01'))) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = 'bold'))
plot_time_series_confirmed_States


## 10. Use a theme that was not used in the ggplot2 tutorial to graph the data from 8.

plot_top10_deaths_worldwide_facet_theme <- plot_top10_deaths_worldwide_facet +
  theme_linedraw()
plot_top10_deaths_worldwide_facet_theme