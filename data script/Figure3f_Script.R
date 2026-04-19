# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Read data from clipboard
setwd("\\data_script\\sup_figs\\Figure f1")
data <- read_excel("Figure f1.xlsx")
# View data
head(data)

# Convert data from wide format to long format
data_long <- data %>%
  pivot_longer(cols = starts_with("0h"):starts_with("72h"),
               names_to = "Time",
               values_to = "OD") %>%
  mutate(Time = as.numeric(gsub("h", "", Time))) # Convert time to numeric

# Calculate the mean value for each time point
data_avg <- data_long %>%
  group_by(Strain, Treatment, Time) %>%
  summarise(OD_avg = mean(OD), .groups = 'drop')

# Plot line chart
ggplot(data_avg, aes(x = Time, y = OD_avg, color = Treatment, shape = Strain, linetype = Strain)) +
  geom_line(linewidth =0.5) +  
  geom_point(size = 1) +       
  scale_shape_manual(values = c("A041" = 17, "A042" = 16, "A055" = 15)) + 
  scale_linetype_manual(values = c("A041" = "solid", "A042" = "dashed", "A055" = "dotted")) + 
  labs(title = "OD600 over Time by Strain and Treatment",
       x = "Time (h)",
       y = "OD600",
       color = "Treatment",
       shape = "Strain",
       linetype = "Strain") +
  theme_minimal() +
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(legend.position = "bottom")