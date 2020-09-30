## Lab 6 - Data Mpas and Interactive Graphs from the COVID-19 Reporting DAta

## Creating Country, State, and County maps

# Two approaches for this lab - Lat and Long data to add specific points on a map and 
# add info to shapes in a map based on the name of the shape (like states). 
# We could use ggmaps, but that is beyond what we will be using right now

## Building maps

# Import the packages we will need to make the maps 
library(tidyverse)
library(maps)
library(mapdata)
library(lubridate)
library(viridis)
library(wesanderson)

# Import the data for the daily report from 2 March, 2020. Note that is is not summarized by country.
# There are a lot of points for the US because of the information about US counties.

daily_report <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv")) %>% 
                           rename(Long = "Long_") # Rename the Longitude variable to match that in the time series 
# Graph on a world map with the size of the points corresponding to number of confirmed cases
ggplot(daily_report, aes(x = Long, y = Lat, size = Confirmed/1000)) +
  borders("world", colour = NA, fill = 'gray90') + # Note that the argument is colour, not color
  theme_bw() +
  geom_point(shape=21, color='purple', fill='purple', alpha = 0.5) +
  labs(title = "World COVID-19 Confirmed Cases", x = '', y = '', size = "Cases (X1000)") +
  theme(legend.position = 'right') +
  coord_fixed(ratio=1.5)
# Got a warning message that 54 rows were removed because they contained missing data 


# Now zoom into the 48 contiguous states by filtering out the other data. This time,
# look at the 5 March data
daily_report <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-05-2020.csv")) %>% 
  rename(Long = "Long_") %>% 
  filter(Country_Region == 'US') %>% 
  filter(!Province_State %in% c('Alaska', 'Hawaii', 'American Samoa',
                                'Puerto Rico', 'Northern Mariana Islands',
                                'Virgin Islands', 'Recovered', 'Guam', 'Grand Princess',
                                'District of Columbina', 'Diamond Princess')) %>% 
  filter(Lat > 0) #Filter out any data with Lat = 0
ggplot(daily_report, aes(x = Long, y = Lat, size = Confirmed/1000)) +
  borders('state', colour = 'black', fill = 'grey 90') +
  theme_bw() +
  geom_point(shape = 21, color = 'purple', fill = 'purple', alpha = 0.5) +
  labs(title = 'COVID-19 Confirmed Cases in the US', x = '', y = '', size = "Cases (X1000)") +
  theme(legend.position = 'right') +
  coord_fixed(ratio = 1.5)

# Here is a more colorful version based on an example by Anisa Dhana

mybreaks <- c(1, 100, 1000, 10000, 10000)
ggplot(daily_report, aes(x = Long, y = Lat, size = Confirmed)) +
  borders("state", colour = "white", fill = "grey90") +
  geom_point(aes(x=Long, y=Lat, size=Confirmed, color=Confirmed),stroke=F, alpha=0.7) +
  scale_size_continuous(name="Cases", trans="log", range=c(1,7), 
                        breaks=mybreaks, labels = c("1-99",
                                                    "100-999", "1,000-9,999", "10,000-99,999", "50,000+")) +
  scale_color_viridis_c(option="viridis",name="Cases",
                        trans="log", breaks=mybreaks, labels = c("1-99",
                                                                 "100-999", "1,000-9,999", "10,000-99,999", "50,000+"))  +
# Cleaning up the graph
  
  theme_void() + 
  guides( colour = guide_legend()) +
  labs(title = "Anisa Dhana's lagout for COVID-19 Confirmed Cases in the US") +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA), 
    panel.background = element_rect(fill = "#ffffff", color = NA), 
    legend.background = element_rect(fill = "#ffffff", color = NA)) +
  coord_fixed(ratio=1.5)
# Got some warning messages - transformation introduced infinite values in discrete y-axis,
# sqrt(x) produced some NaNs, and 40 rows were removed due to missing data

# ggplot borders() was used to define the borders of the map in both examples


## Mapping data to shapes 

# Again, read the daily report from  March and filter to US states
daily_report <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv")) %>% 
  rename(Long = "Long_") %>% 
  filter(Country_Region == "US") %>% 
  group_by(Province_State) %>% 
  summarize(Confirmed = sum(Confirmed)) %>% 
  mutate(Province_State = tolower(Province_State)) # Makes all observations (states) lowercase to match map_data
# Load the US map data
us <- map_data('state')
# We need to join the US map data with our daily report to make one data frame/tibble
state_join <- left_join(us, daily_report, by = c('region' = 'Province_State'))
# Plot state map with an R color palette
ggplot(us, aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  # Add data layer
  geom_polygon(data = state_join, aes(fill = Confirmed), color = 'black') +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 100, type = 'continuous'),
                       trans = 'log10') +
  labs(title = "COVID-19 Confirmed Cases in the US")

# Now, look at the countires using RColorBrewer
library(RColorBrewer)
# To display only colorblind-friendly brewer palettes, specify the option 
# colorblindFriendly = TRUE as follows - 
# display.brewer.all(colorblindFriendly = TRUE)
# Get and format the COVID report data -
report_03_27_2020 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv")) %>% 
  rename(Long = "Long_") %>%
  unite(Key, Admin2, Province_State, sep = '.') %>% # Make a key for the county data (Admin2)
  group_by(Key) %>% 
  summarize(Confirmed = sum(Confirmed)) %>% 
  mutate(Key = tolower(Key))
dim(report_03_27_2020) # 2390 observations, 2 variables
# Get and format the map data
us <- map_data('state')
counties <- map_data('county') %>% 
  unite(Key, subregion, region, sep = '.', remove = FALSE)
# Join the two tibbles
state_join <- left_join(counties, report_03_27_2020, by = c('Key'))
sum(is.na(state_join$Confirmed)) # Check how many entries are NA - 21617
ggplot(us, aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  # Add the COVID-19 data layer 
  borders('state', colour = 'black') +
  geom_polygon(data = state_join, aes(fill = Confirmed)) +
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = 'PuRd'),
                       breaks = c(1, 10, 100, 1000, 10000, 100000),
                       trans = 'log10', na.value = 'White') +
  ggtitle("Number of Confirmed Cases by US County") +
  theme_bw()

# Looking at just Massachusetts - 
daily_report <-   read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv")) %>% 
  rename(Long = "Long_") %>% 
  filter(Province_State == "Massachusetts") %>% 
  group_by(Admin2) %>% 
  summarize(Confirmed = sum(Confirmed)) %>% 
  mutate(Admin2 = tolower(Admin2)) # Again, Admin2 is the county information
us <- map_data('state')
ma_us <- subset(us, region == 'massachusetts')
counties <- map_data('county')
ma_county <- subset(counties, region == 'massachusetts')
state_join <- left_join(ma_county, daily_report, by = c('subregion' = 'Admin2'))
# Plot the MA county data 
ggplot(ma_county, aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  # Add the data layer
  geom_polygon(state_join, mapping = aes(fill = Confirmed), color = 'white') +
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = 'BuGn'),
                       trans = 'log10') +
  labs(title = 'COVID-19 Confirmed Cases in Massachusetts')
# Note that in the daily report, the Nantucket and Dukes counties were reported together
# and not included in the graph. Additionally, there is an unassigned category with 303 
# confirmed cases that were not included, either.
daily_report


## Interactive Graphs
