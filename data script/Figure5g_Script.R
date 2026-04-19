library(circlize)
library(dplyr)
library(tidyr)
library(stringr)

# Read from clipboard (1st column: sample e.g. "G26-ck", 2nd column: value)
setwd("\\data_script\\sup_figs\\Figure 5g")
raw_in <- read_excel("Figure 2g.xlsx")
if(ncol(raw_in) < 2) stop("Fewer than 2 columns read, please check clipboard content and delimiter.")
colnames(raw_in)[1:2] <- c("sample", "value_raw")
raw_in$sample <- trimws(as.character(raw_in$sample))
raw_in$value_raw <- trimws(as.character(raw_in$value_raw))
raw_in$value_raw <- gsub(",", ".", raw_in$value_raw)
raw_in$value <- suppressWarnings(as.numeric(raw_in$value_raw))
raw <- raw_in %>% filter(!is.na(value)) %>% select(sample, value)

# Parse sample -> genotype & treatment (expected "geno-treatment" format)
extracted <- tidyr::extract(raw, col = "sample",
                            into = c("genotype", "treatment"),
                            regex = "^(.*?)-(.*)$", remove = FALSE)
extracted <- extracted %>% filter(!is.na(genotype) & !is.na(treatment))
extracted$treatment <- factor(extracted$treatment)
genotypes <- sort(unique(extracted$genotype))

# Remove outliers with 2x variance (per-genotype winsorize)
extracted <- extracted %>%
  group_by(genotype) %>%
  mutate(mu_g = mean(value, na.rm = TRUE),
         sd_g = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower_g = mu_g - 2 * sd_g,
         upper_g = mu_g + 2 * sd_g,
         value_clean = ifelse(is.na(sd_g) | sd_g <= 0,
                              value,
                              pmin(pmax(value, lower_g), upper_g))
  )
message("Number of truncated (winsorized)/modified observations: ", sum(extracted$value != extracted$value_clean, na.rm = TRUE))

# Summarize (using value_clean)
summary_df <- extracted %>%
  group_by(genotype, treatment) %>%
  summarise(mean_value = mean(value_clean, na.rm = TRUE),
            sd_value = sd(value_clean, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  mutate(sd_value = ifelse(is.na(sd_value), 0, sd_value))

# Simple t.test (based on value_clean)
sig_df <- extracted %>%
  group_by(genotype) %>%
  filter(n_distinct(treatment) >= 2) %>%
  summarise(p.value = tryCatch(t.test(value_clean ~ treatment)$p.value, error = function(e) NA_real_), .groups = "drop") %>%
  mutate(sig = case_when(
    is.na(p.value) ~ "NA",
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

# ---- Dopamine Rainbow Color Palette (10 colors) ----
dopamine_palette <- colorRampPalette(c(
  "#006D77","#06D6A0","#FFD166","#FF7A00","#EF476F","#B5179E","#7209B7","#3A86FF","#FFA69E","#FFD36E"
))(10)
track_alpha <- 0.25
label_colors <- sapply(dopamine_palette, function(col) adjustcolor(col, alpha.f = track_alpha))
main_bg_colors <- label_colors

# Treatment color mapping: ck -> light blue, other treatments -> light pink
unique_treats <- sort(unique(as.character(extracted$treatment)))
ck_col <- "#B3D9FF"      # Light blue
treat_col <- "#FFD6E8"   # Light pink
treatment_colors <- setNames(sapply(unique_treats, function(t){
  col_base <- ifelse(grepl("^ck$", t, ignore.case = TRUE) | grepl("ck", t, ignore.case = TRUE),
                     ck_col, treat_col)
  adjustcolor(col_base, alpha.f = 0.7)
}), unique_treats)
point_colors <- setNames(sapply(unique_treats, function(t){
  col_base <- ifelse(grepl("^ck$", t, ignore.case = TRUE) | grepl("ck", t, ignore.case = TRUE),
                     ck_col, treat_col)
  adjustcolor(col_base, alpha.f = 1)
}), unique_treats)

point_border_col <- "#222222"

# Plot (circlize)
circos.clear()
circos.par(start.degree = 90, gap.degree = 8, track.margin = c(0, 0.02), cell.padding = c(0,0,0,0))
circos.initialize(factors = genotypes, xlim = c(0,1))

# First track: label track with color
n_geno <- length(genotypes)
sector_label_cols <- rep(label_colors, length.out = n_geno)
sector_bg_cols <- rep(main_bg_colors, length.out = n_geno)

circos.track(factors = genotypes, ylim = c(0,1), track.height = 0.06,
             bg.col = sector_label_cols,
             panel.fun = function(x,y){
               circos.text(CELL_META$xcenter, 0.5, CELL_META$sector.index,
                           facing = "bending.inside", cex = 0.6, col = "black")
             })

set.seed(123)
# Second track: main track (background color)
circos.trackPlotRegion(factors = genotypes,
                       ylim = c(0, 1),
                       track.height = 0.7,
                       bg.col = sector_bg_cols,
                       bg.border = NA,
                       panel.fun = function(x, y) {
                         geno <- CELL_META$sector.index
                         df <- summary_df %>% filter(genotype == geno) %>% arrange(treatment)
                         pts <- extracted %>% filter(genotype == geno)
                         
                         if(nrow(df) == 0 && nrow(pts) == 0) return()
                         
                         raw_max_loc <- if(nrow(pts) > 0) max(pts$value_clean, na.rm = TRUE) else 0
                         summary_max_loc <- if(nrow(df) > 0) max(df$mean_value + df$sd_value, na.rm = TRUE) else 0
                         overall_loc <- max(raw_max_loc, summary_max_loc, na.rm = TRUE)
                         if(!is.finite(overall_loc) || overall_loc <= 0) {
                           local_max <- 1
                         } else {
                           local_max <- overall_loc * 1.2
                         }
                         
                         # Axis ticks
                         at <- seq(0, local_max, length.out = 5)
                         for (a in at[-1]) {
                           circos.lines(CELL_META$xlim, c(a, a) / local_max, lty = 5, col = "grey70")
                         }
                         for (a in at) {
                           lab <- ifelse(abs(a - round(a)) < .Machine$double.eps^0.5, as.character(round(a)), format(round(a, 2), nsmall = 2))
                           circos.text(CELL_META$cell.xlim[1] - mm_h(1.8), a / local_max, labels = lab,
                                       facing = "clockwise", adj = c(0.5,0), cex = 0.5, col = "black")
                         }
                         
                         # Bars and points
                         n_bars <- nrow(df)
                         if(n_bars > 0) {
                           bar_width <- 0.18
                           gap_width <- 0.06
                           total_width <- n_bars * bar_width + (n_bars - 1) * gap_width
                           start_x <- (1 - total_width) / 2
                           xcenters <- numeric(n_bars); tops <- numeric(n_bars)
                           
                           for (i in seq_len(n_bars)) {
                             xleft <- start_x + (i - 1) * (bar_width + gap_width)
                             xright <- xleft + bar_width
                             xcenter <- (xleft + xright) / 2
                             xcenters[i] <- xcenter
                             
                             ytop <- df$mean_value[i] / local_max
                             tops[i] <- ytop
                             
                             tname <- as.character(df$treatment[i])
                             fillcol <- if(tname %in% names(treatment_colors)) treatment_colors[[tname]] else adjustcolor("#CCCCCC", alpha.f = 0.7)
                             
                             circos.rect(xleft = xleft, ybottom = 0, xright = xright, ytop = ytop,
                                         col = fillcol, border = "black", lwd = 1)
                             
                             error_y_min <- (df$mean_value[i] - df$sd_value[i]) / local_max
                             error_y_max <- (df$mean_value[i] + df$sd_value[i]) / local_max
                             error_y_min <- max(error_y_min, 0)
                             error_y_max <- min(error_y_max, 1 - 0.01)
                             circos.segments(x0 = xcenter, y0 = error_y_min, x1 = xcenter, y1 = error_y_max, col = "black", lwd = 1)
                             short_bar <- bar_width * 0.4
                             circos.segments(x0 = xcenter - short_bar / 2, y0 = error_y_min,
                                             x1 = xcenter + short_bar / 2, y1 = error_y_min, col = "black", lwd = 1)
                             circos.segments(x0 = xcenter - short_bar / 2, y0 = error_y_max,
                                             x1 = xcenter + short_bar / 2, y1 = error_y_max, col = "black", lwd = 1)
                           }
                           
                           # Scatter points
                           if(nrow(pts) > 0) {
                             pts$treatment <- as.character(pts$treatment)
                             df_treats <- as.character(df$treatment)
                             jitter_width <- bar_width * 0.4
                             xlim_sector <- CELL_META$xlim
                             x_margin <- 1e-6
                             y_margin <- 0.01
                             for (r in seq_len(nrow(pts))) {
                               tname <- pts$treatment[r]
                               j <- match(tname, df_treats)
                               if (is.na(j)) next
                               xpt <- xcenters[j] + runif(1, -jitter_width, jitter_width)
                               xpt <- max(min(xpt, xlim_sector[2] - x_margin), xlim_sector[1] + x_margin)
                               
                               ypt <- pts$value_clean[r] / local_max
                               ypt <- max(min(ypt, 1 - y_margin), 0)
                               pcol <- if(tname %in% names(point_colors)) point_colors[[tname]] else adjustcolor("#555555", alpha.f = 1)
                               circos.points(x = xpt, y = ypt, col = point_border_col, bg = pcol, pch = 21, cex = 0.6)
                             }
                           }
                           
                           # Significance bracket and label (if two groups) - draw only with significance stars
                           if (length(xcenters) >= 2) {
                             sig_label <- sig_df %>% filter(genotype == geno) %>% pull(sig)
                             if (length(sig_label) == 0) sig_label <- NA_character_
                             if (!is.na(sig_label) && sig_label %in c("*", "**", "***")) {
                               top_max <- max(tops, na.rm = TRUE)
                               bracket_y <- top_max + 0.1
                               vline_ybottom <- top_max + 0.5
                               margin <- 0.02
                               bracket_y <- min(bracket_y, 1 - margin)
                               vline_ybottom <- min(vline_ybottom, bracket_y - 0.01)
                               circos.segments(x0 = xcenters[1], y0 = bracket_y, x1 = xcenters[2], y1 = bracket_y, col = "black", lwd = 1)
                               circos.segments(x0 = xcenters[1], y0 = vline_ybottom, x1 = xcenters[1], y1 = bracket_y, col = "black", lwd = 1)
                               circos.segments(x0 = xcenters[2], y0 = vline_ybottom, x1 = xcenters[2], y1 = bracket_y, col = "black", lwd = 1)
                               circos.text(mean(c(xcenters[1], xcenters[2])), bracket_y + 0.02,
                                           labels = sig_label, cex = 0.6, facing = "inside")
                             }
                           }
                           
                         } else {
                           # Draw points only if no summary but raw points exist
                           if(nrow(pts) > 0) {
                             bar_width <- 0.3
                             xcenter <- 0.5
                             xlim_sector <- CELL_META$xlim
                             for (r in seq_len(nrow(pts))) {
                               xpt <- xcenter + runif(1, -bar_width*0.4, bar_width*0.4)
                               xpt <- max(min(xpt, xlim_sector[2] - 1e-6), xlim_sector[1] + 1e-6)
                               ypt <- pts$value_clean[r] / local_max
                               ypt <- max(min(ypt, 1 - 0.01), 0)
                               tname <- pts$treatment[r]
                               pcol <- if(tname %in% names(point_colors)) point_colors[[tname]] else adjustcolor("#555555", alpha.f = 1)
                               circos.points(x = xpt, y = ypt, col = point_border_col, bg = pcol, pch = 21, cex = 0.6)
                             }
                           }
                         }
                       })

# Legend (treatment)
legend("topright", legend = names(treatment_colors), fill = treatment_colors, bty = "n", cex = 0.8)