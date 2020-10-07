## Lab 6 - Data Maps and Interactive Graphs from the COVID-19 Reporting Data

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
# look at the 5 April data
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

# Again, read the daily report from April 2 and filter to US states
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

# Now, look at the counties using RColorBrewer
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

# plotly can also be used to create interactive graphs with maps
# Import plotly
library(plotly)
# Now make a plot of the same MA county data, but interactive and with a diff palette
ggplotly(
  ggplot(ma_county, aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  # Add the data layer
  geom_polygon(data = state_join, mapping = aes(fill = Confirmed), color = 'black') +
  scale_fill_gradientn(colours = wes_palette('Zissou1', 100, type = 'continuous')) +
  ggtitle("COVID-19 Cases in MA") +
  # Clean up the graph
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank())
)

# Here is an example with the world map -
daily_report <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-26-2020.csv")) %>% 
rename(Long = 'Long_') %>% 
  group_by(Country_Region) %>% 
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths))

# Read in the world map data
world <- as_tibble(map_data("world"))

# Check to see if there are differences in the naming of the countries
setdiff(world$region, daily_report$Country_Region)
# Many of the differences are considered states or territories in the JHU COVID reports,
# but let's fix a few of them 
world <- as_tibble(map_data('world')) %>% 
  mutate(region = str_replace_all(region, c("USA" = "US", "Czech Republic" = "Czechia",  
                                            "Ivory Coast" = "Cote d'Ivoire", "Democratic Republic of the Congo" = "Congo (Kinshasa)", 
                                            "Republic of Congo" = "Congo (Brazzaville)")))
# Join the COVID report data with the map data - 
country_join <- left_join(world, daily_report, by = c('region' = 'Country_Region'))
# Create the graph
ggplotly(
  ggplot(world, aes(x = long, y = lat, text = region, group = group)) + # Make the text that pops up be the region names
    coord_fixed(1.3) + 
    # Add data layer
    geom_polygon(data = country_join, mapping = aes(fill = Deaths), color = 'black') +
    scale_fill_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) +
    labs(title = "COVID-19 Deaths")
)


## Exercise 1
# Summarize the counts for each Country in the COVID-19 Deaths graph above, and update
# the graph to 9/26/2020. 

# Read the 9/26/2020 daily report and summarize like before 
# Additionally, I have chosen to use the median Lat and Long to make sure the countries
# with multiple Lat and Long points have on the plot
daily_report <- read_csv('data/daily_report_09-26-2020.csv') %>% 
  rename(Long = 'Long_') %>% 
  group_by(Country_Region) %>% 
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Lat = median(Lat), Long = median(Long))
# Not sure what the question is asking, so I am going to try plotting each country 
# on a world map where the size and color of the point corresponds to Deaths. 
# I'll be using the borders() function to make the borders of the world map. 

ggplotly(
  ggplot(daily_report, aes(x = Long, y = Lat, text = Country_Region, size = Deaths/1000)) +
    borders('world', fill = 'grey90', colour = 'black') +
    coord_fixed(1.3) +
    geom_point(mapping = aes(x = Long, y = Lat, size = Deaths/1000, color = Deaths/1000), shape = 16, alpha =0.7) +
    theme_classic() +
    scale_color_viridis_c(option = 'inferno', name = 'Deaths (X1000)') +
    labs(title = "COVID-19 Deaths Worldwide by 26 September 2020", x = '', y = '') +
    theme(axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
          axis.ticks = element_blank(),
          axis.line = element_blank())
)

# Same style for the confirmed cases, with a different color palette - 
ggplotly(
  ggplot(daily_report, aes(x = Long, y = Lat, text = Country_Region, size = Confirmed/1000)) +
    borders('world', fill = 'grey90', colour = 'black') +
    coord_fixed(1.3) +
    geom_point(aes(size = Confirmed/1000, color = Confirmed/1000), shape = 16, alpha =0.7) +
    theme_classic() +
    scale_color_viridis_c(option = 'viridis', name = 'Confirmed (X1000)') +
    labs(title = "COVID-19 Confirmed Cases Worldwide by 26 September 2020", x = '', y = '') +
    theme(axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
          axis.ticks = element_blank(),
          axis.line = element_blank())
)
# If instead you mean the number of confirmed cases - 

# Read in the world map data
world <- as_tibble(map_data("world")) %>% 
  mutate(region = str_replace_all(region, c("USA" = "US", "Czech Republic" = "Czechia",  
                                          "Ivory Coast" = "Cote d'Ivoire", "Democratic Republic of the Congo" = "Congo (Kinshasa)", 
                                          "Republic of Congo" = "Congo (Brazzaville)", 'UK' = "United Kingdom",
                                          "South Korea" = 'Korea, South', 'Taiwan' = 'Taiwan*')))
# Join the COVID report data with the map data - 
country_join <- left_join(world, daily_report, by = c('region' = 'Country_Region'))

# Make the plot for confirmed cases
ggplotly(
  ggplot(world, aes(x = long, y = lat, text = region, group = group)) + # Make the text that pops up be the region names
    coord_fixed(1.3) + 
    # Add data layer
    geom_polygon(data = country_join, mapping = aes(fill = Confirmed), color = 'black') +
    scale_fill_gradientn(colours = wes_palette("Rushmore1", 100, type = "continuous")) +
    labs(title = "COVID-19 Confirmed Worldwide by 26 September 2020") +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5))
)


## Exercise 2
# Update Anisa Dhana's graph layout of the US to 9/26/2020
download.file(url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-26-2020.csv',
              destfile = 'data/daily_report_09-26-2020.csv')
daily_report <- read_csv('data/daily_report_09-26-2020.csv') %>% 
  rename(Long = 'Long_') %>% 
  filter(Country_Region == "US", Lat > 0) %>% 
  filter(!Province_State %in% c("Alaska","Hawaii", "American Samoa",
                                "Puerto Rico","Northern Mariana Islands", 
                                "Virgin Islands", "Recovered", "Guam", "Grand Princess",
                                "District of Columbia", "Diamond Princess"))
mybreaks2 <- c(1, 1000, 10000, 100000, 250000)
ggplot(daily_report, aes(x = Long, y = Lat, size = Confirmed)) +
  borders('state', colour = 'white', fill = 'grey90') +
  geom_point(aes(x = Long, y = Lat, size = Confirmed, color = Confirmed), stroke = F, alpha = 0.7) +
  scale_size_continuous(name = 'Cases', range=c(1,7),
                        breaks = mybreaks2,labels = c("1-999",
                        "1,000-9,999", "10,000-99,999", "100,000-249,999", '250,000+'))  +
  scale_color_viridis_c(option='viridis', name="Cases",
                        breaks = mybreaks2, labels = c("1-999",
                        "1,000-9,999", "10,000-99,999", "100,000-249,999", '250,000+'))  +
  # Clean up the graph
  theme_void() +
  guides(colour = guide_legend()) +
  labs(title = "Anisa Dhana's style layout for COVID-19 Confirmed Cases in the US by 26 September 2020") +
  theme(legend.position = 'bottom', 
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#ffffff", color = NA), 
        panel.background = element_rect(fill = "#ffffff", color = NA), 
        legend.background = element_rect(fill = "#ffffff", color = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio=1.5)
# Learned that if you make the breaks higher than the highest data value, the break 
# won't appear in the plot


## Exercise 3
# Update the "Number of Confirmed Cases by US County" to 9/26/2020 and use a different
# color/scheme.
# I have chosen to use the Greys color scheme from RColorBrewer with the classic theme -

# Extract and summarize the county data
daily_report_county <- read_csv('data/daily_report_09-26-2020.csv') %>% 
  rename(Long = 'Long_') %>% 
  filter(Country_Region == "US") %>% 
  unite(Key, Admin2, Province_State, sep = '.') %>% # Recall that Admin2 is the county
  group_by(Key) %>% 
  summarize(Confirmed = sum(Confirmed)) %>% 
  mutate(Key = tolower(Key))
# Get the map data for US counties
us <- map_data('state')
counties <- map_data('county') %>% 
  unite(Key, subregion, region, sep = '.', remove = FALSE)
# Join the two tibbles for to later graph
county_join_Sep <- left_join(counties, daily_report_county, by = c("Key"))
sum(is.na(county_join_Sep$Confirmed)) # Many don't have data, but we will continue anyways
ggplot(us, aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  # Add the COVID data layer
  borders('state', colour = 'black') +
  geom_polygon(data = county_join_Sep, aes(fill = Confirmed)) +
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = 'Greys'),
                       breaks = c(1, 10, 100, 1000, 10000, 100000),
                       trans = 'log10', na.value = 'White') + # Keeping no data white
  ggtitle('Number of Confirmed Cases by US County by 26 September 2020') +
  theme_classic()
  
## Exercise 4
# Make an interactive plot of a state of your choosing using a theme different from the 
# above examples

# I will choose Washington since it was the location of the first known case in the US -

# First, extract the data for Washington from the map data for the US (country and county)
washington_state <- map_data('state') %>% 
  subset(region == 'washington')
washington_counties <- map_data('county') %>% 
  subset(region == 'washington')

# Now, extract the data for Washington counties from the daily report data for 9/26/2020
washington_report <- read_csv('data/daily_report_09-26-2020.csv') %>% 
  rename(Long = 'Long_') %>% 
  filter(Country_Region == "US", Province_State == 'Washington') %>% 
  group_by(Admin2) %>% 
  summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Active = sum(Active)) %>% 
  mutate(Admin2 = tolower(Admin2))
  
# Join the tibbles for the county data with the report data - 
washington_counties_joined <- left_join(washington_counties, washington_report, by = c('subregion' = 'Admin2'))

# Now, plot the data into a ggplotly interactive graph -
ggplotly(
  ggplot(washington_counties, aes(x = long, y = lat, group = group, text = subregion)) +
    coord_fixed(1.5) +
    # Add the Washington data 
    geom_polygon(data = washington_counties_joined, mapping = aes(fill = Deaths)) +
    scale_fill_viridis(option = 'cividis', name = 'Deaths', 
                       breaks = c(200, 400, 600, 800)) +
    # Adjust the look of the graph to make it look better
    labs(title = 'COVID-19 Deaths in Washington by 26 September 2020', x = '', y = '') +
    theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        panel.background = element_blank())
)

# Because I am interested, here is the same map for the Confirmed Cases data with 
# a different scale and color scheme

# Confirmed cases - 
ggplotly(
  ggplot(washington_counties, aes(x = long, y = lat, group = group, text = subregion)) +
    coord_fixed(1.5) +
    # Add the Washington data 
    geom_polygon(data = washington_counties_joined, mapping = aes(fill = Confirmed)) +
    scale_fill_viridis(option = 'plasma', name = 'Confirmed', 
                       breaks = c(1000, 5000, 10000, 15000, 20000)) +
    # Adjust the look of the graph to make it look better
    labs(title = 'COVID-19 Confirmed Cases in Washington by 26 September 2020', x = '', y = '') +
    theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
          axis.ticks = element_blank(),
          axis.text  = element_blank(),
          panel.background = element_blank())
)

