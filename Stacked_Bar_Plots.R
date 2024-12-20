#
#                   R SCRIPT FOR CREATING STACKED BAR PLOTS FOR 
#                     OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                   BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Feb 15 2023
# Last edited by Matthew Gilbert, Nov 17 2024
#                                
#______________________________________________________________________________

#                                   SETUP

# Set Working directory to folder containing data and R scripts
setwd("~/Desktop/dominican_republic")

# load libraries
library(dplyr)
library(ggtext)
library(ggplot2)

# Load data frame from folder
Stacked = read.csv("FINAL3.csv", header=F)
Location = read.csv("V2.csv", header=F)
Combined_Location = read.csv("Combined_Location1.csv", header=F)


#______________________________________________________________________________

#                             FORAGING BEHAVIOR

# create a dataset
Species <- c(rep("AMRE" , 8) , rep("BAWW" , 8) , rep("BBTO" , 8) , rep("BWVI" , 8) , rep("CMWA" , 8) , rep("NOPA" , 8) , rep("OVEN" , 8) , rep("PAWA" , 8) , rep("PRAW" , 8) , rep("YRWA" , 8) )
Behavior <- rep(c("Consuming Plant" , "Drinking Nectar" , "Flycatching", "Gleaning", "Hover-Gleaning", "Poking Ground", "Pounching", "Snatching") , 10)
Proportion <- abs(Stacked$V3)
data <- data.frame(Species,Behavior,Proportion)

# sort data for standardized
sorted_data <- data

# Restructure the species order: BBTO and BWVI first, followed by the others
sorted_data$Species <- factor(sorted_data$Species,
                              c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"))

behavior_colors <- c(
  "Consuming Plant" = "#D55E00",        
  "Drinking Nectar" = "gold",        
  "Flycatching" = "#0072B2",           
  "Gleaning" = "#009E73",              
  "Hover-Gleaning" = "#56B4E9",      
  "Poking Ground" = "#C47F17",         
  "Pounching" = "#CC79A7",             
  "Snatching" = "#999999"             
)

# Plot with manually set colors
ggplot(sorted_data, aes(fill = Behavior, y = Proportion, x = Species)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = behavior_colors) +  # Custom colors for behaviors
  labs(fill = "Foraging Behavior", x = "Species", y = "Proportion") + 
  theme(text = element_text(size = 20, color = "black")) +
  theme(axis.text.y = element_text(size = 15, color = "black")) +
  theme(axis.text.x = element_text(size = 13, color = "black")) +
  theme(axis.text.x = element_markdown(size = 15, color = "black", 
                                       face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain"))) + 
  theme(axis.title.y = element_text(margin = margin(t = -50)))


#______________________________________________________________________________

#                       FORAGING LOCATION / SUBSTRATE

# Create a dataset
Species <- c(rep("AMRE", 8), rep("BAWW", 8), rep("BBTO", 8), rep("BWVI", 8), 
             rep("CMWA", 8), rep("NOPA", 8), rep("OVEN", 8), rep("PAWA", 8), 
             rep("PRAW", 8), rep("YRWA", 8))
Foraging_Location <- rep(c("Leaf", "Twig", "Branch", "Trunk", "Dead Material", "Ground", "Flower/Berry", "Other"), 10)
Proportion <- abs(Combined_Location$V3)
data <- data.frame(Species, Foraging_Location, Proportion)

# Reorder species with BBTO and BWVI first
sorted_data <- data
sorted_data$Species <- factor(sorted_data$Species, 
                              c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", 
                                "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"))

# Reorder Foraging_Location categories
sorted_data$Foraging_Location <- factor(sorted_data$Foraging_Location, 
                                        c("Ground", "Dead Material", "Trunk", 
                                          "Branch", "Twig", "Leaf", 
                                          "Flower/Berry", "Other"))

# Define color-blind-friendly palette
foraging_colors <- c(
  "Ground" = "#8B4513",         # Saddle brown for ground substrate
  "Dead Material" = "#D55E00",  # Rich orange-brown for dead material
  "Trunk" = "darkblue",          # Golden orange for trunk-level foraging
  "Branch" = "#0072B2",         # Deep blue for larger branches
  "Twig" = "#56B4E9",           # Light blue for smaller branches
  "Leaf" = "#009E73",           # Teal green for foliage
  "Flower/Berry" = "#CC79A7",   # Magenta for flowers and berries
  "Other" = "#999999"           # Neutral gray for unspecified substrate
)

# Create the plot
ggplot(sorted_data, aes(fill = Foraging_Location, y = Proportion, x = Species)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = foraging_colors) +  # Apply manual color palette
  labs(fill = "Foraging Substrate", 
       #title = "Foraging Substrate Type by Species", 
       x = "Species", 
       y = "Proportion") + 
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_markdown(size = 15, color = "black", 
                                   face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain")),
    axis.title.y = element_text(margin = margin(t = -50))
  )




#______________________________________________________________________________

#                       FORAGING LOCATION / SUBSTRATE




# Remove BBTO and BWVI from the dataset
filtered_data <- data[!data$Species %in% c("BBTO", "BWVI"), ]

# Reorder species to exclude BBTO and BWVI
filtered_data$Species <- factor(filtered_data$Species, 
                                levels = c("OVEN", "PAWA", "YRWA", "AMRE", 
                                           "PRAW", "BAWW", "NOPA", "CMWA"))

# Reorder Foraging_Location categories
filtered_data$Foraging_Location <- factor(filtered_data$Foraging_Location, 
                                          c("Ground", "Dead Material", "Trunk", 
                                            "Branch", "Twig", "Leaf", 
                                            "Flower/Berry", "Other"))

# Define color-blind-friendly palette
foraging_colors <- c(
  "Ground" = "#8B4513",         # Saddle brown for ground substrate
  "Dead Material" = "#D55E00",  # Rich orange-brown for dead material
  "Trunk" = "darkblue",         # Dark blue for trunk-level foraging
  "Branch" = "#0072B2",         # Deep blue for larger branches
  "Twig" = "#56B4E9",           # Light blue for smaller branches
  "Leaf" = "darkgreen",           # Teal green for foliage
  "Flower/Berry" = "lightcoral",   # Magenta for flowers and berries
  "Other" = "#999999"           # Neutral gray for unspecified substrate
)

# Create the plot
ggplot(filtered_data, aes(fill = Foraging_Location, y = Proportion, x = Species)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = foraging_colors) +  # Apply manual color palette
  labs(fill = "Foraging Substrate", 
       title = "Foraging Substrate Type by Species", 
       x = "\nSpecies", 
       y = "Proportion\n") + 
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.title.y = element_text(margin = margin(t = -50))
  )



#______________________________________________________________________________

#                        FISHERS EXACT TEST for SUBSTRATE

## Filter out BBTO and BWVI species and remove missing data
filtered_data <- Mar_3_data %>%
  filter(!Species %in% c("BBTO", "BWVI")) %>%
  drop_na(Species, Foraging.Location)  # Remove rows with missing values in these columns

# Get unique species names
species <- unique(filtered_data$Species)

# Create an empty matrix to store raw p-values
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Fisher's Exact Tests with simulated p-values
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- filtered_data[filtered_data$Species %in% c(species[i], species[j]), ]
    
    # Create a contingency table
    contingency_table <- table(subset_data$Foraging.Location, subset_data$Species)
    
    # Perform Fisher's Exact Test with simulated p-values
    if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
      test_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
      # Store the p-value in the matrix
      pairwise_pvals[species[i], species[j]] <- test_result$p.value
      pairwise_pvals[species[j], species[i]] <- test_result$p.value # Mirror the result
    }
  }
}

# Flatten the upper triangle of the matrix into a data frame
library(reshape2)
pairwise_df <- melt(pairwise_pvals, varnames = c("Species1", "Species2"), value.name = "P_Value")
pairwise_df <- pairwise_df[!is.na(pairwise_df$P_Value), ]

# Adjust p-values using Bonferroni correction (multiply by number of comparisons)
n_comparisons <- nrow(pairwise_df)
pairwise_df$Adjusted_P <- pmin(pairwise_df$P_Value * n_comparisons, 1)  # Cap at 1

# Add a significance level category
pairwise_df$Significance <- cut(
  pairwise_df$Adjusted_P,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("P < 0.001", "P < 0.01", "P < 0.05", "Not Significant")
)

"dark_blue" = "#1F78B4"
"light_blue" = "#A6CEE3"


# Plot the heatmap of adjusted p-values
ggplot(pairwise_df, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray80",
      "P < 0.05" = "#5DADE2",
      "P < 0.01" = "royalblue3",
      "P < 0.001" = "darkblue"
    ),
    name = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16, color = "black"),
    legend.title = element_text(size = 20, color = "black"),
    legend.text = element_text(size = 15, color = "black")
  ) +
  labs(
    x = "\nSpecies",
    y = "Species\n"
  )


#______________________________________________________________________________



# Filter out BBTO and BWVI species and remove missing data
filtered_data <- Combined_Location %>%
  filter(!V1 %in% c("BBTO", "BWVI")) %>%
  drop_na(V1, V2, V3)  # Ensure no missing values in Species (V1), Foraging_Type (V2), or Count (V3)

# Get unique species names
species <- unique(filtered_data$V1)
#species <- c("AMRE", "NOPA", "BAWW")

# Create an empty matrix to store raw p-values
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Fisher's Exact Tests with simulated p-values
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- filtered_data %>%
      filter(V1 %in% c(species[i], species[j])) %>%
      select(V1, V2, V3)
    
    # Create a contingency table
    contingency_table <- xtabs(V3 ~ V2 + V1, data = subset_data)
    
    # Perform Fisher's Exact Test with simulated p-values
    if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
      test_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
      # Store the p-value in the matrix
      pairwise_pvals[species[i], species[j]] <- test_result$p.value
      pairwise_pvals[species[j], species[i]] <- test_result$p.value # Mirror the result
    }
  }
}

# Flatten the upper triangle of the matrix into a data frame
library(reshape2)
pairwise_df <- melt(pairwise_pvals, varnames = c("Species1", "Species2"), value.name = "P_Value")
pairwise_df <- pairwise_df[!is.na(pairwise_df$P_Value), ]

# Adjust p-values using Bonferroni correction (multiply by number of comparisons)
n_comparisons <- nrow(pairwise_df)
pairwise_df$Adjusted_P <- pairwise_df$P_Value * n_comparisons

# Add a significance level category
pairwise_df$Significance <- cut(
  pairwise_df$Adjusted_P,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("P < 0.001", "P < 0.01", "P < 0.05", "Not Significant")
)

# Plot the heatmap of adjusted p-values
ggplot(pairwise_df, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray80",
      "P < 0.05" = "#5DADE2",
      "P < 0.01" = "royalblue3",
      "P < 0.001" = "darkblue"
    ),
    name = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 20, ),
    legend.text = element_text(size = 15)
  ) +
  labs(
    x = "\nSpecies",
    y = "Species\n"
  )




# Filter out BBTO and BWVI species and remove missing data
filtered_data <- Combined_Location %>%
  filter(!V1 %in% c("BBTO", "BWVI")) %>%
  drop_na(V1, V2, V3)  # Ensure no missing values in Species (V1), Foraging_Type (V2), or Count (V3)

# Get unique species names
species <- unique(filtered_data$V1)

# Create an empty matrix to store raw p-values
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Chi-Square Tests
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- filtered_data %>%
      filter(V1 %in% c(species[i], species[j])) %>%
      select(V1, V2, V3)
    
    # Create a contingency table
    contingency_table <- xtabs(V3 ~ V2 + V1, data = subset_data)
    
    # Perform Chi-Square Test
    if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
      test_result <- chisq.test(contingency_table)
      # Store the p-value in the matrix
      pairwise_pvals[species[i], species[j]] <- test_result$p.value
      pairwise_pvals[species[j], species[i]] <- test_result$p.value # Mirror the result
    } else {
      # Debugging: Insufficient data for Chi-Square Test
      print(paste("Insufficient data for", species[i], "and", species[j]))
    }
  }
}

# Flatten the upper triangle of the matrix into a data frame
library(reshape2)
pairwise_df <- melt(pairwise_pvals, varnames = c("Species1", "Species2"), value.name = "P_Value")
pairwise_df <- pairwise_df[!is.na(pairwise_df$P_Value), ]

# Adjust p-values using Bonferroni correction (multiply by number of comparisons)
n_comparisons <- nrow(pairwise_df)
pairwise_df$Adjusted_P <- pmin(pairwise_df$P_Value * n_comparisons, 1)  # Cap at 1

# Add a significance level category
pairwise_df$Significance <- cut(
  pairwise_df$Adjusted_P,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("P < 0.001", "P < 0.01", "P < 0.05", "Not Significant")
)

# Plot the heatmap of adjusted p-values
ggplot(pairwise_df, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray",
      "P < 0.05" = "gold1",
      "P < 0.01" = "orange1",
      "P < 0.001" = "red"
    ),
    name = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  labs(
    x = "Species",
    y = "Species"
  )


