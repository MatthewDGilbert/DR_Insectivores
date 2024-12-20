# PCA

# Load required libraries
library(ggplot2)
library(ggpubr)
library(dplyr)

# Set Working directory to folder containing data and R scripts
setwd("~/Desktop/dominican_republic")

# Load data frames from folder
data = read.csv("Dom Republic 2023 Data Sheets - July (1).csv", header=T)


#______________________________________________________________________________

# Filter out BBTO and BWVI species
data_filtered <- data %>%
  filter(!Species %in% c("BBTO", "BWVI"))

# Perform PCA on Bird.Height and Canopy.height
pca <- prcomp(data_filtered[, c("Bird.Height", "Canopy.height")], scale. = TRUE)

# Add PCA scores to the filtered dataset
data_pca <- data_filtered %>%
  mutate(PC1 = pca$x[, 1], PC2 = pca$x[, 2])

# Compute means and confidence intervals for each species
species_summary <- data_pca %>%
  group_by(Species) %>%
  summarize(
    mean_PC1 = mean(PC1),
    mean_PC2 = mean(PC2),
    se_PC1 = sd(PC1) / sqrt(n()),
    se_PC2 = sd(PC2) / sqrt(n()),
    lower_PC1 = mean_PC1 - qt(0.975, df = n() - 1) * se_PC1,
    upper_PC1 = mean_PC1 + qt(0.975, df = n() - 1) * se_PC1,
    lower_PC2 = mean_PC2 - qt(0.975, df = n() - 1) * se_PC2,
    upper_PC2 = mean_PC2 + qt(0.975, df = n() - 1) * se_PC2
  )

# Plot PCA with only color-coded error bars
ggplot(species_summary, aes(x = mean_PC1, y = mean_PC2, color = Species)) +
  geom_errorbar(aes(ymin = lower_PC2, ymax = upper_PC2), width = 0.05, size = 1.2) +  # Vertical error bars with thicker lines
  geom_errorbarh(aes(xmin = lower_PC1, xmax = upper_PC1), height = 0.05, size = 1.2) +  # Horizontal error bars with thicker lines
  geom_point(size = 5,  shape = 19) +  # Add dots for means
  labs(title = "PCA with 95% Confidence Intervals for Species Means",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right")


#______________________________________________________________________________


data_filtered <- data

# Reorder species so BBTO and BWVI are first
data_filtered <- data_filtered %>%
  mutate(Species = factor(Species, levels = c("BBTO", "BWVI", setdiff(unique(Species), c("BBTO", "BWVI")))))

# Compute means and confidence intervals for each species
species_summary <- data_filtered %>%
  group_by(Species) %>%
  summarize(
    mean_x = mean(Canopy.height),
    mean_y = mean(Height.in.Canopy),
    se_x = sd(Canopy.height) / sqrt(n()),
    se_y = sd(Height.in.Canopy) / sqrt(n()),
    lower_x = mean_x - qt(0.975, df = n() - 1) * se_x,
    upper_x = mean_x + qt(0.975, df = n() - 1) * se_x,
    lower_y = mean_y - qt(0.975, df = n() - 1) * se_y,
    upper_y = mean_y + qt(0.975, df = n() - 1) * se_y
  )

# Plot with color-coded error bars and dots for the means
ggplot(species_summary, aes(x = mean_x, y = mean_y, color = Species)) +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y), width = 0.1, size = 1.2) +  # Vertical error bars
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x), height = 0.01, size = 1.2) +  # Horizontal error bars
  geom_point(size = 3, shape = 19) +  # Add dots for means
  labs(x = "\n Canopy Height (m)", y = "Proportional Height within Canopy \n") +  # Removed title
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),  # Larger font size for axis titles
    axis.text = element_text(size = 16),   # Larger font size for axis tick labels
    legend.text = element_text(size = 14), # Larger font size for legend text
    legend.title = element_text(size = 16) # Larger font size for legend title
  )


#______________________________________________________________________________

#               MIDDLE QUARTILES

# Filter out BBTO and BWVI species
data_filtered <- data
#%>%
  filter(!Species %in% c("BBTO", "BWVI"))

# Compute quartiles for each species
species_summary <- data_filtered %>%
  group_by(Species) %>%
  summarize(
    Q1_x = quantile(Canopy.height, 0.25),
    Q3_x = quantile(Canopy.height, 0.75),
    Q1_y = quantile(Height.in.Canopy, 0.25),
    Q3_y = quantile(Height.in.Canopy, 0.75)
  )

# Plot with color-coded error bars for the 1st and 3rd quartiles
ggplot(species_summary, aes(x = (Q1_x + Q3_x) / 2, y = (Q1_y + Q3_y) / 2, color = Species)) +
  geom_errorbar(aes(ymin = Q1_y, ymax = Q3_y), width = 0, size = 1.2) +  # Vertical error bars for quartiles
  geom_errorbarh(aes(xmin = Q1_x, xmax = Q3_x), height = 0, size = 1.2) +  # Horizontal error bars for quartiles
  labs(title = "Height in Canopy vs. Canopy Height with 1st and 3rd Quartiles",
       x = "Canopy Height (1st to 3rd Quartile)",
       y = "Height in Canopy (1st to 3rd Quartile)") +
  theme_minimal() +
  theme(legend.position = "right")








