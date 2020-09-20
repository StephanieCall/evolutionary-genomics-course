# Import the packages for this lab
library('tidyverse')
library("DT")

# Download the report from the Friday before Spring Break (before UMass closed)
report_03_11_2020 <- read_csv(url('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/03-11-2020.csv')) %>% 
  rename(Country_Region = "Country/Region", Province_State = 'Province/State')

# Check that the data were imported as expected 
str(report_03_11_2020)
View(report_03_11_2020)

## Interactive data tables

# DT package can be used to create interactive tables
# Be warned to not do this with extremely large tables (hundreds of thousands of rows)
# Interactive data table for the 11 March 2020 COVID data report -
datatable(report_03_11_2020)
# With this table, can search for specific entry, sort by different variables (columns),
# show different numbers of entries per page, and more.

## Exercises (Part 1)

## 1. What are the differences between the report on 11 March vs 13 September? 
# Download the report on 13 September 2020 (don't need to rename columns) -
report_9_13_2020 <- read_csv(url('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/09-13-2020.csv'))
# View and inspect both reports -
str(report_03_11_2020); view(report_03_11_2020)
str(report_9_13_2020); view(report_9_13_2020)

# The September report contains much more data, including the following additional 
# columns (variables) - FIPS, Admin2, Active (cases), Combined_Key, Incidence_Rate, 
# and Case-Fatality_Ratio. The September report also changed the "/" in the 
# Province/State and Country/Region variables to "_".
# The September report contains much more information about more countries/provinces, too. 
# The March report only contains 216 entries (different states/provinces/countries) 
# while the September report contains 3954 entries. 

## 2. Make a tibble for country totals for each report using group_by() and summarize()

# March report summarized -
report_03_11_2020_total <- report_03_11_2020 %>% 
  group_by(Country_Region) %>% # Going to assume all entries have a Country_Region
  summarize(Total_Confirmed = sum(Confirmed),
            Total_Deaths = sum(Deaths),
            Total_Recovered = sum(Recovered))
datatable(report_03_11_2020_total)

# September report summarized -
report_9_13_2020_total <- report_9_13_2020 %>% 
  group_by(Country_Region) %>% 
  summarize(Total_Confirmed = sum(Confirmed),
            Total_Deaths = sum(Deaths),
            Total_Recovered = sum(Recovered),
            Total_Active = sum(Active))
datatable(report_9_13_2020_total)

## 3. Make a tibble for total US states statistics like above

report_03_11_2020_US <- report_03_11_2020 %>% 
  filter(Country_Region == 'US') %>% 
  group_by(Province_State) %>% 
  summarize(Total_Confirmed = sum(Confirmed),
            Total_Deaths = sum(Deaths),
            Total_Recovered = sum(Recovered))
datatable(report_03_11_2020_US)
# Note that there are 52 entrees because the District of Columbia (DC) and 
# the Diamond Princess (cruise ship) are included

report_9_13_2020_US <- report_9_13_2020 %>% 
  filter(Country_Region == 'US') %>% 
  group_by(Province_State) %>% 
  summarize(Total_Confirmed = sum(Confirmed),
            Total_Deaths = sum(Deaths),
            Total_Recovered = sum(Recovered),
            Total_Active = sum(Active))
datatable(report_9_13_2020_US)
# Note that there are an additional 6 entries in this table, including additional 
# cruise ships (Grand Princess), US territories (Guam, Northern Mariana Islands, 
# Puerto Rico, and Virgin Islands), and, oddly, an entry for just recovered patients

## 4. Use arrange_by() and slice() to get just the top 10 countries for Deaths

# arrange() orders a data frame by values from a given variable (column); from dplyr.
# Can also be used to sort rows into groups before arranging.
# Usage - arrange(.data, [options], .by_group = FALSE)
# slice() is used to select and subset data frames via row indices; from dplyr.
# Has a few variations for specific slicing (i.e., top/bottom of data frame, min/max values)
# Usage - slice(.data, [options], .preserve = FALSE)
# .preserve is argument that indicates whether the data groups are preserved after slicing or not

report_03_11_2020_top_deaths <- report_03_11_2020_total %>% 
  arrange(desc(Total_Deaths)) %>% 
  slice(1:10)
datatable(report_03_11_2020_top_deaths)

report_9_13_2020_top_deaths <- report_9_13_2020_total %>% 
  arrange(desc(Total_Deaths)) %>% 
  slice(1:10)
datatable(report_9_13_2020_top_deaths)

## 5. Make scatterplots of the top 10 countries with the most confirmed cases and deaths

# March report, confirmed cases -
report_03_11_2020_top_confirmed <- report_03_11_2020_total %>% 
  arrange(desc(Total_Confirmed)) %>% 
  slice(1:10)
datatable(report_03_11_2020_top_confirmed)
# Order the data in descending order to make it look better in the plot
report_03_11_2020_top_confirmed$Country_Region <- factor(report_03_11_2020_top_confirmed$Country_Region, levels = report_03_11_2020_top_confirmed$Country_Region[order(report_03_11_2020_top_confirmed$Total_Confirmed, decreasing = TRUE)])
plot_March_top_confirmed <- ggplot(report_03_11_2020_top_confirmed, aes(x = Country_Region, y = Total_Confirmed)) +
  geom_point() +
  xlab("Country") + ylab("Total Confirmed Cases")
plot_March_top_confirmed

# September report, confirmed cases - 
report_9_13_2020_top_confirmed <- report_9_13_2020_total %>% 
  arrange(desc(Total_Confirmed)) %>% 
  slice(1:10)
datatable(report_9_13_2020_top_confirmed)

report_9_13_2020_top_confirmed$Country_Region <- factor(report_9_13_2020_top_confirmed$Country_Region, levels = report_9_13_2020_top_confirmed$Country_Region[order(report_9_13_2020_top_confirmed$Total_Confirmed, decreasing = TRUE)])
plot_September_top_confirmed <- ggplot(report_9_13_2020_top_confirmed, aes(x = Country_Region, y = Total_Confirmed)) +
  geom_point() +
  xlab("Country") + ylab("Total Confirmed Cases")
plot_September_top_confirmed

# March report, deaths -
report_03_11_2020_top_deaths$Country_Region <- factor(report_03_11_2020_top_deaths$Country_Region, levels = report_03_11_2020_top_deaths$Country_Region[order(report_03_11_2020_top_deaths$Total_Deaths, decreasing = TRUE)])
plot_March_top_deaths <- ggplot(report_03_11_2020_top_deaths, aes(x = Country_Region, y = Total_Deaths)) +
  geom_point() +
  xlab("Country") + ylab("Total Deaths")
plot_March_top_deaths

# September report, deaths - 
report_9_13_2020_top_deaths$Country_Region <- factor(report_9_13_2020_top_deaths$Country_Region, levels = report_9_13_2020_top_deaths$Country_Region[order(report_9_13_2020_top_deaths$Total_Deaths, decreasing = TRUE)])
plot_September_top_deaths <- ggplot(report_9_13_2020_top_deaths, aes(x = Country_Region, y = Total_Deaths)) +
  geom_point() +
  xlab("Country") + ylab("Total Deaths")
plot_September_top_deaths

## 6. Make a bar graph for the top 10 states with the most confirmed deaths and confirmed cases for both reports

# March report, top state confirmed cases -
report_03_11_2020_top_state_confirmed <- report_03_11_2020_US %>% 
  arrange(desc(report_03_11_2020_US$Total_Confirmed)) %>% 
  slice(1:10)
report_03_11_2020_top_state_confirmed$Province_State <- factor(report_03_11_2020_top_state_confirmed$Province_State, levels = report_03_11_2020_top_state_confirmed$Province_State[order(report_03_11_2020_top_state_confirmed$Total_Confirmed, decreasing = TRUE)])
plot_March_states_confirmed <- ggplot(report_03_11_2020_top_state_confirmed, aes(x = Province_State, y = Total_Confirmed)) +
  geom_bar(stat = "identity") +
  xlab("State") + ylab("Total Confirmed Cases")
plot_March_states_confirmed

# September report, top state confirmed cases
report_9_13_2020_top_state_confirmed <- report_9_13_2020_US %>% 
  arrange(desc(report_9_13_2020_US$Total_Confirmed)) %>% 
  slice(1:10)
report_9_13_2020_top_state_confirmed$Province_State <- factor(report_9_13_2020_top_state_confirmed$Province_State, levels = report_9_13_2020_top_state_confirmed$Province_State[order(report_9_13_2020_top_state_confirmed$Total_Confirmed, decreasing = TRUE)])
plot_September_states_confirmed <- ggplot(report_9_13_2020_top_state_confirmed, aes(x = Province_State, y = Total_Confirmed)) +
  geom_bar(stat = "identity") +
  xlab("State") + ylab("Total Confirmed Cases")
plot_September_states_confirmed

# March report, top state deaths -
report_03_11_2020_top_state_deaths <- report_03_11_2020_US %>% 
  arrange(desc(report_03_11_2020_US$Total_Deaths)) %>% 
  slice(1:10)
report_03_11_2020_top_state_deaths$Province_State <- factor(report_03_11_2020_top_state_deaths$Province_State, levels = report_03_11_2020_top_state_deaths$Province_State[order(report_03_11_2020_top_state_deaths$Total_Deaths, decreasing = TRUE)])
plot_March_states_deaths <- ggplot(report_03_11_2020_top_state_deaths, aes(x = Province_State, y = Total_Deaths)) +
  geom_bar(stat = "identity") +
  xlab("State") + ylab("Total Deaths")
plot_March_states_deaths

# September report, top state deaths - 
report_9_13_2020_top_state_deaths <- report_9_13_2020_US %>% 
  arrange(desc(report_9_13_2020_US$Total_Deaths)) %>% 
  slice(1:10)
report_9_13_2020_top_state_deaths$Province_State <- factor(report_9_13_2020_top_state_deaths$Province_State, levels = report_9_13_2020_top_state_deaths$Province_State[order(report_9_13_2020_top_state_deaths$Total_Deaths, decreasing = TRUE)])
plot_September_states_deaths <- ggplot(report_9_13_2020_top_state_deaths, aes(x = Province_State, y = Total_Deaths)) +
  geom_bar(stat = "identity") +
  xlab("State") + ylab("Total Deaths")
plot_September_states_deaths
