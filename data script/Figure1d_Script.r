rm(list = ls())
setwd("\\data_script\\Figure 1d")
library(microeco)
library(magrittr)
library(ggplot2)
library(ggtern)
theme_set(theme_bw())
set.seed(123)

df <- read.delim("\\data\\table_ASV.txt",sep = "\t",row.names = 1,header = T)
Sample <- read.table("\\data\\Sample.txt",header = T,row.names = 1)

taxonomy <- read.table("\\data\\taxonomy.txt",header = T,sep = "\t",row.names = 1)
taxonomy %<>% tidy_taxonomy
dataset <- microtable$new(otu_table = df,
                          sample_table = Sample,
                          tax_table = taxonomy)

t1 <- trans_abund$new(dataset = dataset, 
                      taxrank = "Phylum", 
                      ntaxa = 10, 
                      groupmean = "Group")

t1$plot_tern() #+ 
  scale_fill_manual(values=c("#7e66a4","#D98481","#DFABCF","#CFECBB",
                             "#E9B9AA","#D6C4DD","#8E0152","#DE77AE")) +
  scale_color_manual(values=c("#7e66a4","#D98481","#DFABCF","#CFECBB",
                              "#E9B9AA","#D6C4DD","#8E0152","#DE77AE"))

ggsave("Figure_1d_10phylum.pdf", width = 6, height = 6)


rm(list = ls())
setwd("\\data_script\\Figure 1d")
library(microeco)
library(magrittr)
library(ggplot2)
library(ggtern)
theme_set(theme_bw())
set.seed(123)

##############Pie chart###############
df <- read.delim("\\data\\Proteobacteria_ASV.txt",sep = "\t",row.names = 1,header = T)
Sample <- read.table("\\data\\Sample.txt",header = T,row.names = 1)
taxonomy <- read.table("\\data\\tax-Proteobacteria.txt",header = T,sep = "\t",row.names = 1)
taxonomy %<>% tidy_taxonomy
dataset <- microtable$new(otu_table = df,
                          sample_table = Sample,
                          tax_table = taxonomy)

t1 <- trans_abund$new(dataset = dataset, taxrank = "Genus", ntaxa = 10, groupmean = "Group")
t1$plot_donut(label = TRUE,
              color_values = c("#D8E9F7","#D6C4DD","#E9B9AA","#DCDEC8","#CFECBB","#DFABCF","#D98481","#F08719","#7e66a4","#5D7AB5"))
ggsave("Figure_1d_Proteobacteria_10genus_pie_chart.pdf", width = 10, height = 6)
