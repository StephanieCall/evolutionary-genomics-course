## Lab 10: Population genomics based on high throughput sequencing (HTS)

## Part I - VCF file format

# import the package and read the dataset into memory
library(vcfR)
data(vcfR_example) # This loads vcf into memory, among some other datasets
vcf

# Meta region

# Look at raw first seven lines
strwrap(vcf@meta[1:7]) # strwrap() formats string output by breaking lines at word boundaries
# Look at the entire metadata with nice function
queryMETA(vcf)
# SPecify a specific element for more information
queryMETA(vcf, element = 'DP')

# Fixed region

# Basic look using getFIX()
head(getFIX(vcf))


# gt region

# Look at the raw data of the gt region
vcf@gt[1:6, 1:4]

# vcfR objects

# Read a vcf file into memory using read.vcfR() function
vcf <- read.vcfR('Grunwald_workshop/pinfsc50_filtered.vcf.gz')
# Now investigate it's properties using head()
head(vcf)
# Finally, when finished manipulating the object, save it as a new VCF file using write.vcf()
write.vcf(vcf, 'processedVCFdata_filtered.vcf.gz')


# Part I Exercises

# 1 - How to learn more about read.vcfR()
?read.vcfR()


# 2 - How to learn what acronym AD stands for
queryMETA(vcf, element = 'AD')


# 3 - How can we view the last few lines of the fix data?
# With the INFO column - 
tail(vcf@fix)
# Without the INFO column - 
tail(getFIX(vcf))


# 4 - Is QUAL (found in the fixed region) a useful property for the genotypic variants?
# QUAL stands for Phred-scaled quality reads across all samples. It may be helpful 
# depending on the values across the different variants, so create a histogram of the 
# QUAL values across all variants using ggplot - 
library(ggplot2)
qplot(getQUAL(vcf), geom = 'histogram', binwidth = 30)

# 5 - How do you query the samples? 
# All sample names, plus the FORMAT column
colnames(vcf@gt) # Note the gt region is specified since it contains info about samples
# For a specific sample, subset just like a matrix using the sample name
head(vcf@gt[,'P7722'])
# For a specific sample and variant, specify both the sample (by name) and row (by number)
# The row number for the variant should be taken from the fix region matrix
# For example, extract the data for the 5th genotypic variant for sample P7722
vcf@gt[5, 'P7722']


# Part II - Analysis of genome data

# Read data into R and inspect contents
# We will use the pinfsc50 data set for example
library(vcfR)
library(adegenet)
vcf <- read.vcfR("Grunwald_workshop/pinfsc50_filtered.vcf.gz")
vcf

# If contents good, proceed

# Convert to a genlight object for analysis using the adegenet package - 
x <- vcfR2genlight(vcf)
x
# Note that genlight objects can only contain biallelic data

# View the differences between allelic data storage between the vcfR and genlight objects
# vcfR 
gt <- extract.gt(vcf, element = 'GT') # This extract the allelic info from vcfR objects
gt[c(2, 6, 18), 1:3] # Loci were chosen to show all different possible allelic combos

# genlight
t(as.matrix(x))[c(1,5,17), 1:3]
# Note the transpose used to make the genlight output have the same layout as vcfR 

# vcfR has no concept of populations in the samples, so this must be added manually to 
# genlight objects since genlight allows for populations within the samples 
pop(x) <- as.factor(c('us', 'eu', 'us', 'af', 'eu', 'us', 'mx', 'eu', 'eu',
                      'sa', 'mx', 'sa', 'us', 'sa', 'Pmir', 'us', 'eu', 'eu'))
popNames(x)

# Ploidy in vcfR objects is independent between each sample and loci, but in genlight 
# objects, ploidy must be the same for all loci in a given sample (can vary between samples)
# Set the ploidy for all samples in a genlight object using ploidy()
ploidy(x) <- 2


## Distance matrices 

# Find basic Euclidean distance between all pairs in the genlight object 
x.dist <- dist(x)
x.dist
# The bitwise.dist() function from poppr also creates a distance matrix from genlight
x.dist <- poppr::bitwise.dist(x)
x.dist # Note the different values from the analysis due to a different algorithm


# chromR objects 

## Creating chromR objects 
# Find the files 
vcf_file <- system.file('extdata', 'pinf_sc50.vcf.gz', package = 'pinfsc50')
dna_file <- system.file('extdata', 'pinf_sc50.fasta', package = 'pinfsc50')
gff_file <- system.file('extdata', 'pinf_sc50.gff', package = 'pinfsc50')

# Read the files to memory 
vcf <- read.vcfR(vcf_file, verbose = FALSE)
dna <- ape::read.dna(dna_file, format = 'fasta')
gff <- read.table(gff_file, sep = '\t', quote = '')

# Create a chromR object 
chrom <- create.chromR(name = "Supercontig", vcf = vcf, seq = dna, ann = gff, verbose = TRUE)

# Inspect the contents of the created chromR object
chrom

# Visually inspect the chromR object using plot(), which gives histogram summaries
plot(chrom)

# Visually inspect the crhomR object using chromoqc() to plot the same summary data 
# over the genome coordinates (includes the annotation data)
chromoqc(chrom, dp.alpha = 66)


## Processing chromR objects

# Creating and processing has been separated to allow for different manipulations of the 
# data without needing to reload the data into memory.
# Process the chromR object as loaded into memory (raw)
chrom <- proc.chromR(chrom, verbose = TRUE)
plot(chrom)
chromoqc(chrom, dp.alpha = 66)

# Now, apply some quality control filters to the data and view the results 
vcf <- read.vcfR('Grunwald_workshop/pinfsc50_filtered.vcf.gz', verbose = FALSE) # Using the filtered data set
chrom <- create.chromR(name = 'Supercontig', vcf = vcf, seq = dna, ann = gff,
                       verbose = FALSE)
chrom <- proc.chromR(chrom, verbose = FALSE)
chromoqc(chrom, dp.alpha = 66)

## Tabular summaries

# Processing a chromR object creates tabular summaries on a per-variant and sliding-window
# basis, which can be save for further analysis. This allows genomes to be broken into 
# smaller fragments for analysis when all data can't be loaded into memory.
head(chrom@var.info) # Table for variants 
head(chrom@win.info) # Table for sliding winodw

## Genetic differentiation

# genetic_diff() calculated the population diversity and differentiation using a 
# specified method. Use the methods described in (Hedrick, 2005) for our example data

# Library is already loaded
data(vcfR_example)
pop <- as.factor(c('us', 'eu', 'us', 'af', 'eu', 'us', 'mx', 'eu', 'eu',
                   'sa', 'mx', 'sa', 'us', 'sa', 'Pmir', 'us', 'eu', 'eu'))
myDiff <- genetic_diff(vcf, pops = pop, method = 'nei')
# Display part of the results to get an idea of what they look like
knitr::kable(head(myDiff[, 1:15])) 
# Check the rest of the columns in the results table - 
knitr::kable(head(myDiff[,16:19]))

# Summarize the means for the select column, removing NA and NaN values if present
knitr::kable(round(colMeans(myDiff[,c(3:9,16,19)], na.rm = TRUE), digits = 3))

# The data can also be summarized using violin plots, which show the distribution 
# of each result using the shape of the plot 
library(reshape2)
# ggplot2 is already loaded into the environment
# Again, plot select statistics from the results table and remove missing data
dpf <- melt(myDiff[,c(3:8, 19)], varnames = c('Index', 'Sample'), value.name = 'Depth',
            na.rm = TRUE)
p <- ggplot(dpf, aes(x = variable, y = Depth)) +
  geom_violin(fill = '#2ca25f', adjust = 1.2) +
  xlab('') +
  ylab('') +
  theme_bw()
p


# Exercises Part II

# 1 - Plot G'_ST vs genomic position
# Manhattan plots are scatterplots of G'_ST  along genomic position
# Determine the names of the columns in the results data frame 
head(myDiff)
tail(myDiff) # Max position is 99989
# Make a break for every 10,000 bp (10 kbp)
my_breaks <- seq(0, 100, by = 10)
# Need POS on the x axis, Gprimest on the y axis
man_plot <- ggplot(myDiff, aes(x = as.numeric(POS)/1000, y = Gprimest)) +
  geom_point(color = 'purple', alpha = 0.5) +
  scale_x_continuous(breaks = my_breaks) +
  labs(x = 'Genomic position (kbp)', y = "G'_ST",
       title = "Manhattan plot of Supercontig_1.50") + 
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5)) 
man_plot

# 2 - This Manhattan plot looks unusual. Why?
# See the Rmd file for answer - sample size for each population is too small
table(pop)

# 3 - To zoom in on part of a genomic position in chromoqc(), first look at documentation
?chromoqc()
# Based on documentation, use xlim (supplied as a vector for position) to define window
chromoqc(chrom, dp.alpha = 66, xlim = c(100000, 200000))

# 4 - Use queryMETA() to find other data in the file that may be of interest to look at
queryMETA(vcf)
# Look at a few of the data types in more detail to see if any are of interest
queryMETA(vcf, element = 'FORMAT=<ID=GQ') # Genotype quality
queryMETA(vcf, element = 'FORMAT=<ID=AD') # Allelic depths
queryMETA(vcf, element = 'FORMAT=<ID=PL') # Phred-scaled likelihoods for genotypes
queryMETA(vcf, element = 'INFO=<ID=ClippingRankSum') # A Z-score for Wilcoxon rank sum test
queryMETA(vcf, element = 'INFO=<ID=MQ') # A different Z-score for a Wilcoxon rank sum test
queryMETA(vcf, element = 'INFO=<ID=MLEAF') # Maximum likelihood expectation for allele freq.