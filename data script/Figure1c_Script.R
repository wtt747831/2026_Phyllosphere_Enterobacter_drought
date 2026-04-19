setwd("\\data\\Figure1C")
library(multcompView)
library(tidyverse)
library(agricolae)
set.seed(123)
df <- read.table("Shannon.txt",header = T,sep = "\t",check.names = F)
df$Group <- factor(df$Group,levels = c("BMS-RS","RMS-RS","PMS-RS"))

#Normality testing was performed on the data.
shapiro.test(df$root_length)
lm1<-lm(root_length~group,data=df)
summary(lm1)
anova(lm1)
hist(lm1$residuals)
shapiro.test(lm1$residuals)

# Shannon
df$Shannon <- as.numeric(df$Shannon)
anova_Shannon <- aov(Shannon~Group,data=df)
summary(anova_Shannon)

# Multiple pairwise comparisons were conducted using Fisher LSD test
LSD_Shannon <- LSD.test(anova_Shannon, "Group", p.adj = "fdr", alpha = 0.05, group = TRUE)
cld <- multcompView::multcompLetters4(anova_Shannon,LSD_Shannon)

cld_df <- LSD_result$groups
cld_df$Group <- rownames(cld_df)
cld_df <- cld_df %>% select(Group, groups)

dt <- df %>% 
  group_by(Group) %>% 
  summarise(w = mean(Shannon), sd = sd(Shannon), max = max(Shannon)) %>%
  arrange(desc(w)) %>%
  left_join(cld_df, by = "Group")

mycolor<-c("#D8E9F7","#D6C4DD","#DFABCF")

p_Shannon <- ggplot(data = df,mapping = aes(x = Group,y = Shannon,fill = Group)) + 
  stat_boxplot(geom="errorbar",position = position_dodge(0.75),width = 0.3) +
  geom_boxplot(aes(fill = Group),outlier.shape = NA) + 
  geom_point(aes(color = Group), position=position_jitterdodge(0.05),alpha = 0.6) +
  geom_text(data = dt, aes(x = Group,y = max, label = Letters), nudge_x = 0.05, nudge_y = 0.5) +
  theme_bw()+
  ylim(4.5,8)+
  scale_fill_manual(values = mycolor)+
  scale_color_manual(values = mycolor)

ggsave("Figure1c_Shannon.pdf",width = 6,height = 4)
