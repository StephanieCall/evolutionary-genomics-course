## Lab 10: Introduction to Microbiome ANalysis using dada2 and phyloseq

# Install the necessary packages for microbiome analysis, specifically dada2 for analyzing
# sequencing data, phyloseq for assigning taxonomy to analyzed community data, and DECIPHER
# for other stuff
BiocManager::install('dada2')
BiocManager::install('phyloseq')
BiocManager::install('DECIPHER')

## Getting ready

# Import the dada2 package and define the path to the fastq files for the tutorial
library(dada2); packageVersion('dada2')
path <- 'data/MiSeq_SOP' # Changed to the directory containing the fastq files
list.files(path)

# Forward and reverse fastq file names have the following format: 
# SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs <- sort(list.files(path, pattern = '_R1_001.fastq', full.names = TRUE)) 
# This lists the names of the forward fastq files that match this regular expression 
# pattern, returning the relative path to these files.
fnRs <- sort(list.files(path, pattern = '_R2_001.fastq', full.names = TRUE))
# Extract the sample names, assuming the file names have the following format: 
# SAMPLENAME_XXX.fastq
sample.names <- sapply(strsplit(basename(fnFs), '_'), `[`, 1)
# basename() returns the file (base) name from the input path name
# strplit() splits the string by _ in this case, and sapply extracts the first entry from 
# the returned list


## Inspect read quality profiles 

# First, inspect the forward reads 
plotQualityProfile(fnFs[1:2]) # only the first two samples for simplicity
# Based on this, trim the data to 240 nt (trim last 10 nt to reduce possible errors)
# Now, inspect the reverse reads
plotQualityProfile(fnRs[1:2]) # only the first two samples for simplicity
# From this, trim the data to 160 nt (where the quality starts to crash)

# When trimming paired-end data, must make sure the sequences overlap enough for merging
# later. Must overlap at least 20 + biological.length.variation


## Filter and trim

# Assign file names for the filtered data 
# Place filtered files in a new 'filtered/' *subdirectory
filtFs <- file.path(path, 'filtered', paste0(sample.names, '_F_filt.fastq.gz'))
filtRs <- file.path(path, 'filtered', paste0(sample.names, '_R_filt.fastq.gz'))
names(filtFs) <- sample.names
names(filtRs) <- sample.names
# names() gets/sets the names for an object 

# Use the standard filtering parameters for this example data. Note that maxEE sets the 
# maximum number of expected errors allowed in a read, which has shown to be a better 
# filter than averaging quality reads
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen = c(240, 160), 
                     maxN = 0, maxEE = c(2, 2), truncQ = 2, rm.phix = TRUE, 
                     compress = TRUE, multithread = FALSE) # Set to FALSE on Windows
head(out)


## Learn the error rates

# Use a machine-learning, parametric error model to estimate error rates for this data set
errF <- learnErrors(filtFs, multithread = TRUE) # Forward data 
errR <- learnErrors(filtRs, multithread = TRUE) # Reverse data 

# Now, check that the estimated error rates are good fits to the observed error rates
plotErrors(errF, nominalQ = TRUE) # Only show the forward reads data


## Sample Inference

# Use the core sample inference algorithm to analyze the filtered and trimmed sequences
dadaFs <- dada(filtFs, err = errF, multithread = TRUE) # Forward analysis
dadaRs <- dada(filtRs, err = errR, multithread = TRUE) # Reverse analysis

# Now inspect the forward dada-class (analyzed) object to make sure it is as expected 
dadaFs[[1]] # Inspect the forward dada-class object


## Merge paired reads

# Use the mergePairs() function to merge the forward and reverse pairs for each sample 
# from the analyzed dada-class objects. Will only merge if at least 12 bp overlap and 
# all nt in the overlapping region match
mergers <- mergePairs(dadaFs, filtFs, dadaRs, filtRs, verbose = TRUE)
# Inspect the first few merged pairs of the first sample 
head(mergers[[1]])


## Construct sequence table

# Make an amplicon sequence variant table (ASV) from the merged paired reads 
seqtab <- makeSequenceTable(mergers)
dim(seqtab)
# Inspect distribution of sequence lengths
table(nchar(getSequences(seqtab)))


## Remove chimeras

# The core dada function corrects for substitutions and indels, but not PCR chimeras. 
# These are removed after de-noising and merging the amplicons for greater accuracy
seqtab.nochim <- removeBimeraDenovo(seqtab, method = 'consensus', multithread = TRUE, 
                                    verbose = TRUE)
dim(seqtab.nochim)
sum(seqtab.nochim)/sum(seqtab)


## Track reads through the pipeline 

# Define a function to calculate the number unique sequences for each step
getN <- function(x) sum(getUniques(x))
# Make a matrix with the columns as the step in the workflow and the rows as samples
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN),
               rowSums(seqtab.nochim))
# If only a single sample is processed, remove the sapply calls (e.g., getN(dadaFs))
colnames(track) <- c('input', 'filtered', 'de-noisedF', 'de-noisedR',
                     'merged', 'nonchimeric')
rownames(track) <- sample.names
head(track)


## Assign taxonomy

# Use the assignTaxonomy() function and a training set (since this function uses the naive
# Bayesian classifier method) to assign taxonomy to each sequence variant based on the 
# minBoot minimal bootstrap confidence. For this tutorial, use the Silva reference
# database as the training set
taxa <- assignTaxonomy(seqtab.nochim, 'data/MiSeq_SOP/silva_nr99_v138_train_set.fa.gz',
                       multithread = TRUE) # minBoot is implicitly 50

# Assign species to the taxa as a bonus using the species assignment data from Silva
taxa.print <- taxa # Remove the sequence rownames for displaying
rownames(taxa.print) <- NULL
head(taxa.print)

# An alternative approach to assign taxa is to use IdTaxa() from DECIPHER, which uses
# a new IDTAXA algorithm that has shown better performance than the classic naive Bayesian
# classifier. The following block creates the same formatted taxa table as assignTaxonomy()
library(DECIPHER); packageVersion("DECIPHER")
dna <- DNAStringSet(getSequences(seqtab.nochim)) # Create a DNAStringSet from the ASVs
load('/data/MiSeq_SOP/SILVA_SSU_r138_2019.RData') # Import the R data into the environment
# This uses all of the processors to assign the taxa
ids <- IdTaxa(dna, trainingSet, strand = 'top', processor = NULL, verbose = FALSE) 
# Character vector of all the taxanomic ranks of interest
ranks <- c('domain', 'phylum', 'class', 'order', 'family', 'genus', 'species') 
# Now, convert the output Taxa object to a matrix like the output from assignTaxonomy()
taxid <- t(sapply(ids, funciton(x) {
  m <- match(ranks, x$rank)
  taxa <- x$taxon[m]
  taxa[startsWith(taxa, 'unclassified_')] <- NA # Convert all unclassified taxa to NA
  taxa
}))
colnames(taxid) <- ranks
rownames(taxid) <- getSequences(seqtab.nochim)


# Evaluate accuracy

# Evaluate the accuracy of the DADA2 pipeline by comparing the results of the analysis for 
# the planted mock community with that of the known composition of the mock community
unqs.mock <- seqtab.nochim['Mock',] # Get the results for the mock community only
unqs.mock <- sort(unqs.mock[unqs.mock>0], decreasing = TRUE) # Drop ASVs absent in Mock
cat('DADA inferred', length(unqs.mock), 'sample sequences present in the Mock community.\n')
mock.ref <- getSequences(file.path(path, 'HMP_MOCK.v35.fasta'))
match.ref <- sum(sapply(names(unqs.mock), function(x) any(grepl(x, mock.ref))))
# grepl() searches the reference database for the genetic variant sequence
# any() returns true if any hit is found from the search
cat('Of those,', sum(match.ref), 'were exact matches to the expected reference sequences.\n')
# All 20 were found, so it was accurate for at least this sample


## Bonus - Handoff to phyloseq

# Import the necessary packages and set the theme
library(phyloseq); packageVersion('phyloseq')
library(Biostrings); packageVersion('Biostrings')
library(ggplot2); packageVersion('ggplot2')
theme_set(theme_bw()) # Set the theme for the plots to black and white

# Construct a table for the metadata for the samples of the experiment
samples.out <- rownames(seqtab.nochim)
subject <- sapply(strsplit(samples.out, 'D'), `[`, 1) # Extract first part of file name
gender <- substr(subject, 1, 1) # Gender is first letter
subject <- substr(subject, 2, 999) # Subject is rest of string
day <- as.integer(sapply(strsplit(samples.out, 'D'), `[`, 2)) # Extract second part (day)
samdf <- data.frame(Subject = subject, Gender = gender, Day = day)
samdf$When <- 'Early' # Specify the samples that are early and late during the time course
samdf$When[samdf$Day>100] <- 'Late'
rownames(samdf) <- sample.out # Name the rows by the sample name

# Make the phyloseq object 
ps <- phyloseq(otu_table(seqtab.nochim, taxa_are_rows = FALSE), 
               sample_data(samdf),
               tax_table(taxa))
ps <- prune_samples(sample_names(ps) != "Mock", ps) # Remove the mock sample