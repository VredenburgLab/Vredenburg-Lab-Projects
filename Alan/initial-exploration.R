#load packages
library(phyloseq)
library(data.table)
library(ggplot2)

#load merged file
mp = readRDS("mp-all-merge.RDS")
mp
nsamples(mp)
ntaxa(mp)
sample_variables(mp)
rank_names(mp)
phy_tree(mp)

#summarize sequencing depths in general
sdt = data.table(as(sample_data(mp), "data.frame"),
                 TotalReads = sample_sums(mp), keep.rownames = TRUE)
setnames(sdt, "rn", "SampleID")
pSeqDepth = ggplot(sdt, aes(TotalReads)) + geom_histogram() + ggtitle("Sequencing Depth")
pSeqDepth

#summarize sequenceing depths by metadata category
pSeqDepth + facet_wrap(~Species)
pSeqDepth + facet_wrap(~Site)

#compare two categories' sequencing depths
pSeqDepth + 
  facet_grid(Species ~ Site) + 
  ggtitle("Seq. Depth by Species and Site")

#total counts of each OTU
tdt = data.table(tax_table(mp),
                 TotalCounts = taxa_sums(mp),
                 OTU = taxa_names(mp))
ggplot(tdt, aes(TotalCounts)) + 
  geom_histogram() + 
  ggtitle("Histogram of Total Counts")

# How many singletons (OTUs that occur in just one sample, one time)?
tdt[(TotalCounts <= 0), .N]

# How many doubletons? (OTU that occurs just twice)
tdt[(TotalCounts <= 2), .N]

# taxa cumulative sum
taxcumsum = tdt[, .N, by = TotalCounts]
setkey(taxcumsum, TotalCounts)
taxcumsum[, CumSum := cumsum(N)]
# Define the plot
pCumSum = ggplot(taxcumsum, aes(TotalCounts, CumSum)) + 
  geom_point() +
  xlab("Filtering Threshold, Minimum Total Counts") +
  ylab("OTUs Filtered") +
  ggtitle("OTUs that would be filtered vs. the minimum count threshold")
pCumSum

#zoom in to different depths to observe how many OTUs get filtered
pCumSum + xlim(0, 5000)
