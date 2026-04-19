rm(list = ls())
setwd("\\data_script\\Figure 1e")
library(tidyverse)
library(reshape2)
library(psych)
set.seed(123)
###############################read data#######################################
table1 <- read.table("data\\0_biaoxing.txt",header = T,row.names = 1)
table2 <- read.table("data\\1_pms_bms_10genus.txt",header = T,row.names = 1)
cor.result<-corr.test(table1,table2,method="pearson",adjust = "fdr")
cor.result$p
cor.result$r

corr_r <- cor.result$r %>% reshape2::melt(value.name = "r.value")
head(corr_r)
corr_p <- cor.result$p %>% reshape2::melt(value.name = "p.value")
head(corr_p)
corr_df <- corr_r %>% left_join(corr_p)
head(corr_df)

corr_df <- corr_df %>%
  mutate(Var1 = as.character(Var1), 
         Var2 = as.character(Var2))
head(corr_df)

corr_dta <- corr_df %>% mutate(sig = insight::format_p(p.value, stars_only  = TRUE)) %>% 
  mutate(r.value = ifelse(Var1 == Var2, NA, r.value)) %>% 
  mutate(r_label = paste0(sprintf("%.3f", r.value), sig)) %>% 
  mutate(r_label2 = ifelse(Var1 == Var2, NA, r_label)) 

dt <- ggplot(corr_dta, aes(Var1, Var2, fill = r.value)) + 
  geom_tile(color = "grey90") +
  geom_text(aes(label = r_label2) , fontface = "bold" ) + 
  scale_fill_distiller(palette = "PiYG", limits = c(-1.0, 1.0), na.value = "grey95") + 
  guides(size = "none") + 
  theme_minimal(base_size = 17, base_family = "serif") +  
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = "black"))
dt

ggsave("Figure_1e_1_pms_vs_bms_Genus_phenotype_cor.PDF", dt, width = 8, height = 6)

