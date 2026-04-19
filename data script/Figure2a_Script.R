rm(list = ls())
setwd("\\data_script\\\\Figure 2a")
library(ggplot2)
library(dplyr)
library(readxl)
library(gridExtra)

df <- read_excel("Figure 2a.xlsx")

df$Sample <- 1:nrow(df)

# ==================== CMP ====================
p1 <- ggplot(df, aes(x=1, y=Sample)) +
  geom_tile(aes(fill=CMP), color="white", linewidth=0.1) +
  scale_fill_gradientn(colors=c("#4575B4","#FFFFBF","#D73027"), name="CMP    ") +
  scale_y_reverse(expand=c(0,0)) +
  labs(x="",y="") +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.grid=element_blank())

# ==================== FC(R/B) ====================
p2 <- ggplot(df, aes(x=1, y=Sample)) +
  geom_tile(aes(fill=`FC(R/B)`), color="white", linewidth=0.1) +
  scale_fill_gradientn(colors=c("#4575B4","#FFFFBF","#D73027"), name="FC(R/B)") +
  scale_y_reverse(expand=c(0,0)) +
  labs(x="",y="") +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.grid=element_blank())

# ==================== FC(P/B) ====================
p3 <- ggplot(df, aes(x=1, y=Sample)) +
  geom_tile(aes(fill=`FC(P/B)`), color="white", linewidth=0.1) +
  scale_fill_gradientn(colors=c("#4575B4","#FFFFBF","#D73027"), name="FC(P/B)") +
  scale_y_reverse(expand=c(0,0)) +
  labs(x="",y="") +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.grid=element_blank())

p<-grid.arrange(p1, p2, p3, ncol=3, widths=c(1,1,1))
ggsave("CMP.pdf", p,width = 5, height = 3)