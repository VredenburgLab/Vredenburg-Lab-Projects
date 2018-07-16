#load packages
library(phyloseq)
library(biomformat)
library(ggplot2)

#create file paths
tablewithtaxonomy = "C:/Users/Alan/Desktop/QIIME2/normal/R"
biom_file = file.path(tablewithtaxonomy, "table-with-taxonomy-hdf5.biom")

#read in and import biom file
biom1 = read_biom(biom_file)
biom1
mp0 = import_biom(biom1)
mp0

#keep only the first seven taxanomic ranks (but used mainly to fix a weird importing artifact)
tax_table(mp0) = tax_table(mp0)[, 1:7]

#import mapping file and merge with biom file
qsd = import_qiime_sample_data("Taiwan-Mapping-File.txt")
mp1 = merge_phyloseq(mp0, qsd)
mp1
sample_variables(mp1)

#import rooted phylogenetic tree
treeFile1 = "tree.nwk"
tree1 = read_tree(treeFile1)
tree1
class(tree1)

#merge with biom and taxonomy
mp2 = merge_phyloseq(mp1, tree1)
mp2

#import reps seqs
library(Biostrings)
repseqsFile = "rep-set.fasta"
bs1 = readDNAStringSet(repseqsFile)
bs1
#Remove non-OTU info from sequence name
names(bs1) <- gsub("\\s.+$", "", names(bs1))
sum(names(bs1) %in% taxa_names(mp2))

#add reps seqs to data obejct
mp3 = merge_phyloseq(mp2, bs1)
mp3

saveRDS(mp2, "mp-all-merge.RDS")
