## Data Carpentry Tutorial "Data Visualization with ggplot2"

# Import tidyverse and data
library('tidyverse')
surveys_complete <- read_csv("data/surveys_complete.csv")

## Plotting with ggplot2

# ggplot2 functions with data in the 'long' format - variables in columns and observations
# in rows. Graphics are built step by step by adding new elements in layers, making 
# it great for customization and flexibility. 
# Basic template - ggplot(data = [DATA], mapping = aes([MAPPINGS])) + [GEOM_FUNC]()

# Bind the plot to a specific data frame with data argument 
ggplot(data = surveys_complete)
# Define the aesthetics by selecting variables to plot and structure/visuals
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) # empty plot
# Add graphical representations of the data in the plot using + operator
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point()  # New line here for visual separation and easier reading. Now plot has data
# This uses geom_point() for scatter plot. Others include geom_boxplot() for boxplots, 
# geom_line() for lines connecting points (i.e., time points), and more

# The + operator also allows you to modify existing ggplot objects, which allows for
# templates to be created and then tweaked as needed.
# Ex - Assign plot to a variable 
surveys_plot <- ggplot(data = surveys_complete,
                       mapping = aes(x = weight, y = hindfoot_length))
# Draw the plot
surveys_plot + geom_point() # Same plot as above

# Additional notes - anything in the base ggplot() is seen in additional layers
# Can specify additional aesthetics for a geom outside of ggplot() base
# + operator must go at the end of the line to add another layer (beginning of
# the next line results in an error)
surveys_plot +
  gem_point()      # Adds another layer
surveys_plot
  + geom_point()   # Returns an error and does not add another layer

## Challenge Question 1

# Use hexigonal binning of the large data set of surveys_complete to plot the data,
# then compare and contrast the strengths and weaknesses of this kind of bin plot 
# versus the usual scatterplot made earlier

# Import the hexbin package for ggplot2
library('hexbin')

# Plot the surveys_plot data using hexbin
surveys_plot +
  geom_hex()
# Compared to the scatterplot 
surveys_plot +
  geom_point()

# The hexigonal binning plot is much faster (and likely less intensive) to make compared
# to the scatterplot, and it is easier to see major groupings/clusters in the large 
# data set with this plotting style. However, the resolution is far worse in the 
# hexigonal plot to the point where you can't tell the values for the points. Additionally,
# depending on the resolution bins and color scale, you may miss some of the relatively 
# smaller clusters or finer patterns in the data. Finally, the hexagonal binning style 
# would not work well with smaller data sets or data sets that are spread apart with 
# relatively little clustering. A scatterplot would work better in these situations 
# (and to see if there are finer patterns in the data).

## Building your plots iteratively

# First, define the data set, then axes. Then, add the geom in another layer -
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point()

# Can then add additional layers to the plot to extract more information
# For example, add transparency (via alpha argument) to avoid overplotting
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)
# Add color (via color argument) to make plots distinct
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")
# Color different variables (using vector and aes in geom) look for patterns in the data
# ggplot2 will automatically apply different colors to the vector and add a legend
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

## Challenge Question 2

# Create a scatterplot of weight over species_id with the plot types showing in 
# different colors. Is this a good way to show this type of data?
surveys_weight_species <- ggplot(data = surveys_complete,
                                 mapping= aes(x = species_id, y = weight)) +
  geom_point(aes(color = plot_type))
surveys_weight_species
# I would say that this is not a good way to plot these data. A box plot to visually
# show the distribution of these data would be much better

## Boxplot

# Can use the boxplot geom to create boxplots 
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot()

# Can add points to boxplots to get a better idea of the number of data points per 
# independent variable and their relative distributions
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) + # Make the boxplot lines and dots fully solid 
  geom_jitter(alpha = 0.3, color = "tomato") # Make the dots offset from each other
                                             # and slightly transparent
# Notice that the dot layer, which was added last, is visually above the boxplot layer
# To make the boxplot layer in front of the dots, it should be the last geom line -
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot(alpha = 0)
  
## Challenge Question 3

# 1. Replace the boxplot of the above weight vs species_id data with a violin plot
#    to better visualize the shape of the data -
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin()
# Data are difficult to see for many of the species because it is so widely distributed
# Some of them just look like a line

# 2. Change the axes scales - change weight to a log_10 scale
# ?scale_y_log10 # Investigate the scale_y_log10() object
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin() +
  scale_y_log10('weight')
# Data are now much easier to visualize, and can see the distributions much more easily

# 3. Create a boxplot for hindfoot_length within a species, with a jitter layer of data
# points and color to indicate the plot where the sample was taken (plot_id)
ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.25, aes(color = plot_id))
# Check the class of plot_id - 
class(surveys_complete$plot_id) #Numeric
surveys_complete$plot_id <- factor(surveys_complete$plot_id)
class(surveys_complete$plot_id) # Changed to factor
ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.4, aes(color = plot_id))
# Now, plot shows a wider range of colors for the plot types instead of a gradiant,
# which allows for easier identification of the different plots (although there 
# are a lot of plots that makes identification somewhat difficult)

## Plotting time series data

# To calculate the number of counter per year for each genus,
# group the data and counts records for each group
yearly_counts <- surveys_complete %>% 
  count(year, genus)

# Timelapse data can be visualized via line plots using dateas on the x-axis 
# and counts on the y-axis
ggplot(yearly_counts, aes(x = year, y = n)) +
  geom_line()
# The plot doesn't look very good since all the genera are plotted together.
# Need to group by genus in "aes()" function -
ggplot(yearly_counts, aes(x = year, y = n, group = genus)) +
  geom_line()
# Add color to easily distinguish between the different genera -
ggplot(yearly_counts, aes(x = year, y = n, color = genus)) +
  geom_line()
# Note that the color argument automatically groups by that value

## Integrating the pipe operator with ggplot2

# Can use the pipe operator to pipe the input data into ggplot() function.
# Remember to use + to build the ggplot, though. To make the same graph as above - 
yearly_counts %>% 
  ggplot(aes(x = year, y = n, color = genus)) +
  geom_line()

# The pipe can also be used to manipulate data and create a plot at the same time -
yearly_counts_graph <- surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(aes(x = year, y = n, color = genus)) +
  geom_line()
yearly_counts_graph

## Faceting

# Faceting allows a plot with  data with different groupings to be plot into multiple,
# separate and smaller plots based on a grouping in the dataset. 
# To do so, use the facet_wrap() function - 
ggplot(yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

# Faceting also allows graphing of multiple variables on the facet graphs.
# For example, graph the sex of each genus in the facet graphs -
yearly_sex_counts <- surveys_complete %>% 
  count(year, genus, sex)
ggplot(yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus))
# Faceting allows for multiple facets, such as sex and genus, using facet_grid() -
ggplot(yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus))

# Note that facet_wrap is best when you are splitting by only one variable while
# facet_grid is best for two discrete variables where both functions have data at all levels.
# To organize by only rows (analogous for only columns), just specify rows (or cols) -
ggplot(yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(genus))
ggplot(yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(cols = vars(genus))

## ggplot2 Themes

# All components of a ggplot can be customized with the theme() function, and there
# are preloaded themes available to change the appearance by just adding an additional layer.
# For example, the faceted plots above can be given a simpler white background with theme_bw() -
ggplot(yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  theme_bw()

# A complete list of available themes can be found at https://ggplot2.tidyverse.org/reference/ggtheme.html.
# Additionally, the ggthemes packages has a lot of options for themes.

## Challenge Question 4 

# Create a plot that depicts how the average weight of each species changes over time

yearly_avg_weights <- surveys_complete %>% 
  group_by(year, species) %>% 
  summarize(avg_weight = mean(weight))
head(yearly_avg_weights) # Check that the data were grouped properly
yearly_avg_weights_plot <- ggplot(yearly_avg_weights, aes(x = year, y = avg_weight)) +
  geom_line() + 
  facet_wrap(vars(species))
yearly_avg_weights_plot

# For species_id (since that is what the answer shows), change species to species_id -
yearly_avg_weights_id <- surveys_complete %>% 
  group_by(year, species_id) %>% 
  summarize(avg_weight = mean(weight))
yearly_avg_weights_plot_id <- ggplot(yearly_avg_weights_id, aes(x = year, y = avg_weight)) +
  geom_line() + 
  facet_wrap(vars(species_id))
yearly_avg_weights_plot_id

## Customization 

# Many other aspects of plots can be changed, which can be perused through the ggplot2
# cheat sheet at https://rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf.
# For example, different the axes be rescaled or relabelled, the coordinate system can 
# be changed, different themes can be applied, legends and titles can be added, etc.

# To change the axes labels and add a title, use labs() function in another layer -
ggplot(yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = 'Observed genera through time',
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw()

# Increase the font size to improve readability using theme() with the text argument-
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw() +
  theme(text=element_text(size = 16))

# The fonts in the plots can also be changed, although the extrafont package may need 
# to be installed on Windows

# Change the orientation of the labels to increase readability. Note the different 
# arguments in the element_text functions to customize the text -
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

# New themes can be saved as an object to easily apply to other data -
grey_theme <- theme(axis.text.x = element_text(colour="grey20", size = 12, 
                                               angle = 90, hjust = 0.5, 
                                               vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text=element_text(size = 16))

ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  grey_theme

## Challenge Question 5

# Change the appearance of another graph (either one here or your own data) 
# I will use data I just got last week from a flow cytometry experiment - 
sgRNA_2X_titration = tibble(IPTG_conc = c(8, 4,	2,	1,	0.5,	0.25,	0.125,	0.0625,	0.03125,	0.015625,	0.0078125,	0.00390625,	0.001953125,	0.000976563,	0.000488281),
                            RPU = c(0.401006711,	0.384467881,	0.36864813,	0.295302013,	0.341323106,	0.422579099,	0.853307766,	2.246164909,	4.023729626,	4.895254075,	5.21021093,	4.618408437,	5.36409396,	5.43096836,	5.49928092))
head(sgRNA_2X_titration) # Check that the tibble was created properly
sgRNA_titration_plot <- ggplot(sgRNA_2X_titration, aes(x = IPTG_conc, y = RPU)) +
  geom_point(color = "blue") +
  labs(title = "sgRNA titration",
       x = "IPTG concentration (mM)",
       y = "Output (RPU)",
       size = 16, face = 'bold') + 
  scale_y_log10(limits = c(0.1, 10), minor_breaks = seq(1, 10, by = 1)) +
  scale_x_log10(limits = c(0.0001, 10), labels = scales::label_number()) +
  theme_classic() +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        plot.title = element_text (size = 18, face = 'bold', hjust = 0.5))
  
sgRNA_titration_plot
# Still need to figure out how to add minor tick marks, but need to move on since 
# I have spent probably an hour messing around with graphing

## Arranging and exporting plots

# The gridExtra package allows ggplots to be combined into a single figure for exporting
# using the grid.arrange() function

library(gridExtra)

spp_weight_boxplot <- ggplot(surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  labs(x = "Species", y = expression(log[10](Weight))) +
  scale_y_log10() +
  labs()

spp_count_plot <- ggplot(yearly_counts, aes(x = year, y = n, color = genus)) +
  geom_line() + 
  labs(x = "Year", y = "Abundance")

grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))

# ncol and nrow arguments allow for the plots to be arranged as desired. There are 
# additional arguments and tools for more complex layouts, which can be found at 
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html. 

# After creating a plot, it can be saved to a different file format. However, the 
# normal Export tab saves it at a low resolution. Instead, the ggsave() function 
# allows the plot to be adjusted and saved using various arguments (width, height, dpi)
my_plot <- ggplot(data = yearly_sex_counts, 
                  aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))

ggsave('example_save_plot.png', my_plot, width = 15, height = 10)

# This also works for grid.arrange() plots
combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, 
                           widths = c(4, 6))
ggsave("combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)
# width and height adjust the size of the plot while dpi changes the resolutoin
