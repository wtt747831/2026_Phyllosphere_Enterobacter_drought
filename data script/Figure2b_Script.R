# Clear workspace
rm(list = ls())

# Load required R packages
library(readxl)
library(agricolae)
library(Rmisc)
library(ggplot2)

# Import data (read from clipboard)
setwd("\\data_script\\sup_figs\\Figure 2b")
data <- read_excel("Figure 2b.xlsx")
df <- data
# View column names
colnames(df)

# ====================== Modification 1: Process grouping names ======================
df$Treatment <- factor(df$Treatment, levels = c("ck","Uridine","Thymidine","Myristic acid"))
# ===================================================================================

# Set random seed
set.seed(123)

# Inspect data
str(df)
head(df)

# Normality test
shapiro.test(df$Area)

# Linear model
lm1 <- lm(Area ~ Treatment, data = df)
summary(lm1)
anova(lm1)

# Residual check
hist(lm1$residuals)
shapiro.test(lm1$residuals)

# Mean and standard deviation
sd <- summarySE(df, measurevar = "Area", groupvars = c("Treatment"), na.rm = TRUE)

# ANOVA
anova_trait <- aov(Area ~ Treatment, data = df)
summary(anova_trait)

# Multiple comparisons
bijiao <- LSD.test(anova_trait, "Treatment", alpha = 0.05, p.adj = "fdr", group = TRUE, main = "Area")
df1 <- bijiao$groups
df2 <- bijiao$means
df_bijiao <- merge(df1, df2, by = "row.names")
colnames(df_bijiao) <- c("Treatment", "Area", "groups", "Area.y", "std", "r", "LCL", "UCL", "Min", "Max", "Q25", "Q50", "Q75")

# ====================== Modification 2: Color palette for 4 treatments ======================
mycolor <- c("#8FD2E6","#8BD3C1","#F5BFA8","#A8D8B9")
# ===========================================================================================

# Plot (English title unchanged)
ggplot(data = df, mapping = aes(x = Treatment, y = Area, fill = Treatment)) +
  stat_boxplot(geom = "errorbar", position = position_dodge(0.75), width = 0.3) +
  geom_boxplot(aes(fill = Treatment), outlier.shape = NA, width = 0.4) +
  geom_point(aes(color = Treatment), position = position_jitterdodge(0.05), alpha = 0.6) +
  geom_text(
    data = df_bijiao,
    aes(x = Treatment, y = Area + std + 0.1, label = groups),
    vjust = -0.5, size = 4
  ) +
  theme_bw() +
  scale_fill_manual(values = mycolor) +
  scale_color_manual(values = mycolor) +
  labs(x = "Treatment", y = "Bacterial area", title = "Bacterial area analysis") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  