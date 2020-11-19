## Lab 11x - Phyloseq tutorials

## Ordination plots 

# Load packages, prepare the data
library(phyloseq); packageVersion('phyloseq')
data("GlobalPatterns") # Get the data needed for the tutorial
library(ggplot2); packageVersion('ggplot2')
library(plyr); packageVersion('plyr')
theme_set(theme_bw()) # Set the theme for the ggplots

# Process the data to filter out most OTUs to decrease the data set (since we aren't 
# looking for additional patterns) and increase computation time for demonstration purposes
# Determine then filter out taxa that don't appear > 5 times in more than 1/2 samples
GP <- GlobalPatterns
wh0 <- genefilter_sample(GP, filterfun_sample(function(x) x > 5), A = 0.5*nsamples(GP))
GP1 <- prune_taxa(wh0, GP)

# Transform the data to an even sample depth using the provided function
GP1 <- transform_sample_counts(GP1, function(x) 1E6 * x/sum(x))

# Finally, keep only the five most abundant phyla in the final dataset
# Determine abundance of all phylum in dataset
phylum.sum <- tapply(taxa_sums(GP1), tax_table(GP1)[, "Phylum"], sum, na.rm = TRUE)
# Determine top five phyla through sorting
top5phyla <- names(sort(phylum.sum, TRUE))[1:5]
# Filter out the rest of the phyla
GP1 <- prune_taxa((tax_table(GP1)[, 'Phylum'] %in% top5phyla), GP1)
# There are still 204 OTUs is the dataset

# We will be looking at human vs non-human associated OTUs, so define a human categorical
# variable 
human <- get_variable(GP1, 'SampleType') %in% c('Feces', 'Mock', 'Skin', 'Tongue')
sample_data(GP1)$human <- factor(human)


## Four main ordination plots 

# 1 - Just the OTUs

# Plot just the OTUs, shaded by phylumn
GP.ord <- ordinate(GP1, 'NMDS', 'bray') # Use non-metric multidimensional scaling method
# For some reason, still worked despite the standardization not converging
p1 <- plot_ordination(GP1, GP.ord, type = 'taxa', color = 'Phylum', title = 'taxa')
p1

# Facet the plot by phylum to make it more readable and understandable
p1 + facet_wrap(~Phylum, 3) # 3 columns


# 2 - Just samples

# Plot the samples with a shape to show human vs non-human associated samples and 
# with additional layers to show make the graph more colorful
p2 <- plot_ordination(GP1, GP.ord, type = 'samples', color = 'SampleType', 
                      shape = 'human')
p2 + geom_polygon(aes(fill = SampleType)) + # Connect all related samples and fill with color
  geom_point(size = 5) + # Add points for each sample to distinguish in the shape
  ggtitle('Samples')


# 3 - Biplot

# Create a plot that plots both the OTUs (taxa, phyla in this example) and samples
# Make plot by both sample and OTU (phylum)
p3 <-  plot_ordination(GP1, GP.ord, type = 'biplot', color = 'SampleType', 
                      shape = 'Phylum', title = 'Biplot')
# Create some variables to modify the automatic shape scale for the plot
GP1.shape.names <- get_taxa_unique(GP1, 'Phylum') # Get the unique phylum names
GP1.shape <- 15:(15 + length(GP1.shape.names) - 1) # Make vector to make shapes for phyla 
names(GP1.shape) <- GP1.shape.names
GP1.shape['samples'] <- 16
p3 + scale_shape_manual(values = GP1.shape)
# Not sure why the taxa factor is present in the SampleType to ruin the color mapping


# 4 - split graphic

# You can split the samples and OTUs plots into side-by-side plots by specifying 
# type = 'split' in the plot_ordination() function
p4 <- plot_ordination(GP1, GP.ord, type = 'split', color = 'Phylum', 
                      shape = 'human', label = 'SampleType', title = 'Split')
p4

# Change the color scheme for the plot to make the samples black
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n+1)
  hcl(h = hues, l = 65, c = 100)[1:100] # Make a vector of colors
}
color.names <- levels(p4$data$Phylum) # Determine the labels for the colors
p4cols <- gg_color_hue(length(color.names)) # Set the colors
names(p4cols) <- color.names # Set the names for each color
p4cols['Samples'] <- 'black'
p4 + scale_color_manual(values = p4cols)


## Supported Ordination Methods

# This section will use every ordination method available in plot_ordination to analyze 
# the dataset, then plot all the results into one large facet plot for comparision

# First, apply the ordination methods to the dataset and store the results in a list
dist  <-  'bray'
ord_meths <- c('DCA', 'CCA', 'RDA', 'DPCoA', 'NMDS', 'MDS', 'PCoA')
plist <- llply(as.list(ord_meths), function(i, physeq, dist){
  ordi = ordinate(physeq, method = i, distance = dist) # Perform the ordination
  plot_ordination(physeq, ordi, 'samples', color = 'SampleType') # Make the ordination plot by the sample
}, GP1, dist)
names(plist) <- ord_meths # Give each ordination method in the results list a name

# Next, extract the results and store in a data.frame object for plotting with ggplot2
pdataframe <- ldply(plist, function(x){
  df = x$data[, 1:2]
  colnames(df) = c('Axis_1', 'Axis_2')
  return(cbind(df, x$data))
})
# ldply applies the function to each element in the list and makes the results into 
# a data.frame object.
names(pdataframe)[1] <- 'method' # Rename the first column to the ordination method

# Plot the different methonds into a large facet plot 
p <- ggplot(pdataframe, aes(Axis_1, Axis_2, color = SampleType, shape = human,
                            fill = SampleType)) +
  geom_point(size = 4) +
  geom_polygon() + # Connect the same samples and fill with color to group them better
  facet_wrap(~method, scales = 'free') +
  # Set the colors for the fill and samples
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  scale_colour_brewer(type = 'qual', palette = 'Set1') 
p

# To call a sinlge graphic for an ordination method, call the graph from the list of 
# ggplot graphics, plist, using normal calling methods
plist[[2]] # Recall that to call a list from a list, you need double brackets
# Now, add some extra layers to make it look nicer
p <- plist[[2]] + 
  scale_colour_brewer(type = 'qual', palette = 'Set1') +
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  geom_point(size = 5) +
  geom_polygon(aes(fill = SampleType))
p


## MDS (PCoA) on UniFrac Distances 

# The ordinate() function can be used to perform both a weighted UniFrac analysis and 
# then perform a principal coordinate analysis on the resulting distance matrix in 
# just one line. The data can then be passed to plot_ordination() to make a ggplot plot
ordu <- ordinate(GP1, 'PCoA', 'unifrac', weighted = TRUE)
plot_ordination(GP1, ordu, color = 'SampleType', shape = 'human')

# Now, add layers to the graphic to make the plot more colorful
p <- plot_ordination(GP1, ordu, color = 'SampleType', shape = 'human') +
  geom_point(size = 7, alpha = 0.75) +
  scale_color_brewer(type = 'qual', palette = 'Set1') +
  ggtitle("MDS/PCoA on weighted-UniFrac distance, GlobalPatterns")
p



## Alpha diversity graphics

## Load the packages and set the parameters
# The packages are already loaded, so just reload the data and set the default parameters
data('GlobalPatterns')
pal <- 'Set1' # Set the color palette
# Make functions to set the default color palettes for plots to Set1
scale_colour_discrete <- function(palname = pal, ...) {
  scale_colour_brewer(palette = palname, ...)
}
scale_fill_discrete <- function(palname = pal, ...){
  scale_fill_brewer(palette = palname, ...)
}

## Prepare the data

# Since we want to keep as much diversity in the dataset as possible, just trim the
# OTUs that aren't present in any sample in the GlobalPatterns dataset
GP <- prune_taxa(taxa_sums(GlobalPatterns) > 0, GlobalPatterns)

## Plot Examples

# Here is the basic plot_richness() facet plot - 
plot_richness(GP)
# For some reason, I don't get an error for removing rows containing missing values
# Perhaps the prune_taxa and taxa_sums functions were update to prune samples without data?

# You can plot only specific methods by specifying the method(s) in the measures argument 
# of plot_richness()
plot_richness(GP, measures=c("Chao1", "Shannon"))
# Again, no warning message, unlike that in the tutorial

# Change the x-axis to a more meaningful categorical variable rather than sample names
plot_richness(GP, x = "SampleType", measures = c("Chao1", 'Shannon'))

# Define a new variable in the dataset to indicate whether the sample if human-associated
# or not
sample_data(GP)$human <- get_variable(GP, 'SampleType') %in% c('Feces', 'Mock', 'Skin',
                                                               'Tongue')
# This both creates the new human column and define it for each sample depending on 
# whether or not  that sample if from feces, mock, skin, or tongue

# Now, plot by the human variable on the x-axis and shade by the SampleType
plot_richness(GP, x = 'human', color = 'SampleType', measures = c("Chao1", 'Shannon')) 

# Now, mess around with the plots. Merge the samples by SampleType (calculate mean) and 
# add another layer to the new plot
GPst <- merge_samples(GP, 'SampleType') # Merge and calculate mean by default
# Repair the variables that were damaged during the merge (coerced to numeric)
sample_data(GPst)$SampleType <- factor(sample_names(GPst))
sample_data(GPst)$human <- as.logical(sample_data(GPst)$human)
p <- plot_richness(GPst, x = 'human', color = 'SampleType', 
                   measures = c("Chao1", 'Shannon'))
p + geom_point(size = 5, alpha = 0.7)


## More details about ggplot2

# ggplot2 objects have their layers stored in lists, which can easily be inspected using 
# standard list notation
p$layers
# Note that there are a few additional details in the geom_errorbar information that 
# isn't present in the tutorial

# Now, remove the first layer and add another layer with larger, semi-transparent points
p$layers <- p$layers[-1] # Note that this removes the first layer, not selects the 
# last element in the list like in Python
p + geom_point(size = 5, alpha = 0.7)


## Heatmap Plots

## Load Packages, Data
# Packages have already been imported
# Re-import the data and filter to the top 300 Bacteria taxa 
data('GlobalPatterns')
gpt <- subset_taxa(GlobalPatterns, Kingdom = 'Bacteria') # Pick out Bacteria samples
# Get names of top 300 Bacteria taxa by abundance, then filter
gpt <- prune_taxa(names(sort(taxa_sums(gpt), TRUE)[1:300], gpt)) 


## Plot a 300-taxa dataset
# Plot by sample (OTU) using default dislay conditions
plot_heatmap(gpt, sample.label = "SampleType")


## Subset a smaller dataset based on an Archaeal phylum
# Subset to Crenarchaeota phylum
gpac <- subset_taxa(GlobalPatterns, Phylum == 'Crenarchaeota')


## Default plot_heatmap() settings
plot_heatmap(gpac)


## Relabel by sample variable and different taxonomic rank
(p <- plot_heatmap(gpac, 'NMDS', 'bray', 'SampleType', "Family"))
# Note that the () call the object to create the graphic 
# Note the default argument order in plot_heatmap() - physeq object (data), ordination
# method, distance method, sample.label, and taxa.label


## Relabel axis titles
# To relabel axis titles without changing the axis ticks - 
p$scales$scales[[1]]$name <- 'My X-Axis' # This corresponds to attributes for x axis
p$scales$scales[[2]]$name <- 'My Y-Axis' # This corresponds to attributes for y axis
p


## Change the color schemes on same plots
# These can be change with the high, low, and na.value arguments
# First, do a dark blue to light green gradient 
plot_heatmap(gpac, 'NMDS', 'bray', 'SampleType', 'Family',
             low = '#000033', high = '#CCFF66') # NA value color is same (black)

# Now, do a dark blue to red color scheme
plot_heatmap(gpac, 'NMDS', 'bray', 'SampleType', 'Family',
             low = '#000033', high = '#FF3300') # NA value color is same (black)

# Finally, do a very dark blue to very light blue scheme (the default)
plot_heatmap(gpac, 'NMDS', 'bray', 'SampleType', 'Family',
             low = '#000033', high = '#66CCFF') # NA value color is same (black)

# Now, do a dark-on-light color scheme, where the low abundances are light blue and
# the high abundances dark blue. Note that the missing or zero values are white.
plot_heatmap(gpac, 'NMDS', 'bray', 'SampleType', 'Family',
             low = '#66CCFF', high = '#000033', na.value = 'white')

# Here is another dark-on-light color scheme but with the near-zero color as a light cream
# color and darker colors more like blue-grey instead of dark blue. This has a better 
# contrast than a lot of schemes but is less colorful
plot_heatmap(gpac, 'NMDS', 'bray', 'SampleType', 'Family',
             low = '#FFFFCC', high = '#000033', na.value = 'white')


## Use different ordination methods and distances
# Change the ordination method and distance with the method and distance arguments, resp.
# Start with the NMDS ordinate on the jaccard distance
plot_heatmap(gpac, 'NMDS', 'jaccard')

# Now plot the detrended correspondence analysis
plot_heatmap(gpac, 'DCA', 'none', 'SampleType', 'Family') 
# Note that there is no ecological distance for this analysis

# Detrended correspondence analysis
plot_heatmap(gpac, "DCA", "none", "SampleType", "Family")

# Unconstrained redundancy analysis (principal components analysis) 
plot_heatmap(gpac, "RDA", "none", "SampleType", "Family")

# Principal coordinates analysis with bray-curtis distance (default)
plot_heatmap(gpac, "PCoA", "bray", "SampleType", "Family")

# PCoA with unweighted UniFrac distance 
plot_heatmap(gpac, "PCoA", "unifrac", "SampleType", "Family")

# Multidimensional Scaling (MDS) with weighted UniFrac distance
plot_heatmap(gpac, "MDS", "unifrac", "SampleType", "Family", weighted=TRUE)

# For comparison, default heatmap using base R graphics using heirarchical clustering
heatmap(otu_table(gpac))


## Plot Microbiome Networks

# Import the data, set the random seed for network construction, and filter the data
data(enterotype)
set.seed(711L)
# Remove the samples without an enterotype designation
enterotype <- subset_samples(enterotype, !is.na(Enterotype)) 


## The plot_net() function
# This is a quick and easy way to plot a network from a dataset and allows for some 
# customization. The older plot_network() function is more involved but can provide 
# more flexibility in the graphs (see later part of tutorial). 
plot_net(enterotype, maxdist = 0.4, point_label = "Sample_ID")

# Now, make a network that maps some sample variables (the sequencing technology used 
# and the eterotype of the sample) to look for patterns in the data
plot_net(enterotype, maxdist = 0.3, color = "SeqTech", shape = "Enterotype")
# Note that the maximum distance was changed, 
# which is why the two main clusters are no longer connected

# Explore what happens when the maximum distance between the nodes is decreased 
plot_net(enterotype, maxdist = 0.25, color = "SeqTech", shape = "Enterotype")


## The plot_network() function
# Must first create an igraph object with the base methods to calculate the network,
# then use the plot_network() function to create the image of the network (and include
# any customization)
# Plot the data with the default plot_network conditions - 
ig <- make_network(enterotype, max.dist = 0.3)
plot_network(ig, enterotype)
# Notice that the image is different than the network created using plot_net().

# Now, map the same sample variables to the nodes as before
plot_network(ig, enterotype, color = 'SeqTech', shape = 'Enterotype', 
             line_weight = 0.4, label = NULL)
# Note that the same igraph object is used to generate this plot as the default

# Explore what happens when you calculate the network with a lower maximum distance 
ig <- make_network(enterotype, max.dist = 0.2)
plot_network(ig, enterotype, color = 'SeqTech', shape = "Enterotype", 
             line_weight = 0.4, label = NULL)

# FInally, recalculate and graph the network created using the Bray-Curtis distance 
# instead of the default Jaccard
ig <- make_network(enterotype, dist.fun = 'bray', max.dist = 0.3)
plot_network(ig, enterotype, color = "SeqTech", shape = 'Enterotype', 
             line_weight = 0.4, label = NULL)
